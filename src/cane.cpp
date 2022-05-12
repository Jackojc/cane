#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <string_view>
#include <chrono>
#include <thread>
#include <filesystem>
#include <memory>

#include <report.hpp>
#include <view.hpp>
#include <lib.hpp>

#include <conflict/conflict.hpp>

extern "C" {
	#include <jack/jack.h>
	#include <jack/midiport.h>
	#include <jack/ringbuffer.h>
}

struct jack_deleter {
	template <typename T> constexpr void operator()(T arg) const {
		jack_free(arg);
	}
};

using JackPorts = std::unique_ptr<const char*[], jack_deleter>;

enum {
	OPT_HELP = 0b01,
	OPT_LIST = 0b10,
};

inline std::string read_file(std::filesystem::path path) {
	try {
		std::filesystem::path cur = path;

		while (std::filesystem::is_symlink(cur)) {
			std::filesystem::path tmp = std::filesystem::read_symlink(cur);

			if (tmp == cur)
				cane::general_error(cane::STR_SYMLINK_ERROR, path);

			cur = tmp;
		}

		if (std::filesystem::is_directory(cur) or std::filesystem::is_other(cur))
			cane::general_error(cane::STR_NOT_FILE_ERROR, path);

		if (not std::filesystem::exists(cur))
			cane::general_error(cane::STR_FILE_NOT_FOUND_ERROR, path);

		std::ifstream is(cur, std::ios::binary);

		if (not is.is_open())
			cane::general_error(cane::STR_FILE_READ_ERROR, path);

		std::stringstream ss;
		ss << is.rdbuf();

		return ss.str();
	}

	catch (const std::filesystem::filesystem_error&) {
		cane::general_error(cane::STR_FILE_READ_ERROR, path);
	}
}

int main(int argc, const char* argv[]) {
	std::string_view device;
	std::string_view filename;
	uint64_t flags;

	auto parser = conflict::parser {
		conflict::option { { 'h', "help", "show help" }, flags, OPT_HELP },
		conflict::option { { 'l', "list", "list available midi devices" }, flags, OPT_LIST },

		conflict::string_option { { 'f', "file", "input file" }, "filename", filename },
		conflict::string_option { { 'm', "midi", "midi device to connect to" }, "device", device },
	};

	parser.apply_defaults();
	auto status = parser.parse(argc - 1, argv + 1);

	try {
		// Handle argument parsing errors.
		switch (status.err) {
			case conflict::error::invalid_option: cane::general_error(cane::STR_OPT_INVALID_OPTION, status.what1);
			case conflict::error::invalid_argument: cane::general_error(cane::STR_OPT_INVALID_ARG, status.what1, status.what2);
			case conflict::error::missing_argument: cane::general_error(cane::STR_OPT_MISSING_ARG, status.what1);
			case::conflict::error::ok: break;
		}


		if (flags & OPT_HELP) {
			parser.print_help();
			return 0;
		}


		// Setup JACK
		using namespace std::chrono_literals;

		struct JackData {
			jack_client_t* client = nullptr;
			jack_port_t* port = nullptr;

			std::vector<cane::Event> events;

			~JackData() {
				jack_port_unregister(client, port);
				jack_deactivate(client);
				jack_client_close(client);
			}
		} midi {};

		// Connect to JACK, register a port and register our callback.
		if (not (midi.client = jack_client_open(cane::CSTR_EXE, JackOptions::JackNoStartServer, nullptr)))
			cane::general_error(cane::STR_MIDI_CONNECT_ERROR);

		if (not (midi.port = jack_port_register(midi.client, cane::CSTR_PORT, JACK_DEFAULT_MIDI_TYPE, JackPortIsOutput, 0)))
			cane::general_error(cane::STR_MIDI_PORT_ERROR);

		midi.events.reserve(midi.events.capacity() + jack_get_buffer_size(midi.client));


		// MIDI out callback
		jack_set_process_callback(midi.client, [] (jack_nframes_t nframes, void *arg) {
			auto& [client, port, events] = *static_cast<JackData*>(arg);

			void* out_buffer = jack_port_get_buffer(port, nframes);
			jack_midi_clear_buffer(out_buffer);

			jack_nframes_t f = events.size() < nframes ? events.size() : nframes;

			// Copy every MIDI event into the buffer provided by JACK.
			for (cane::Event& ev: events) {
				std::array msg = ev.message();

				if (jack_midi_event_write(out_buffer, f, msg.data(), msg.size()))
					cane::general_error(cane::STR_MIDI_WRITE_ERROR);

				size_t lost = 0;
				if ((lost = jack_midi_get_lost_event_count(out_buffer)))
					cane::general_warning(cane::STR_MIDI_LOST_EVENT, lost);
			}

			events.clear();  // important so that we don't leak memory

			return 0;
		}, static_cast<void*>(&midi));

		if (jack_activate(midi.client))
			cane::general_error(cane::STR_MIDI_ACTIVATE_ERROR);


		// If no device is specified _and_ `-l` is not passed,
		// throw an error. It's perfectly valid to give an empty
		// string to JACK here and it will give us back a list
		// of ports regardless.
		if (device.empty() and (flags & OPT_LIST) != OPT_LIST)
			cane::general_error(cane::STR_MIDI_NO_DEVICE);


		// Get an array of all MIDI input ports that we could potentially connect to.
		JackPorts ports { jack_get_ports(midi.client, std::string { device }.c_str(), JACK_DEFAULT_MIDI_TYPE, JackPortIsInput) };

		if (not ports)
			cane::general_error(cane::STR_MIDI_GET_PORTS_ERROR);

		if (not ports[0])  // No MIDI input ports.
			cane::general_error(cane::STR_MIDI_NOT_FOUND, device);

		if (flags & OPT_LIST) {
			for (size_t i = 0; ports[i] != nullptr; ++i)
				cane::general_notice(cane::STR_MIDI_DEVICE, ports[i]);

			return 0;
		}

		cane::general_notice(cane::STR_MIDI_FOUND, ports[0]);

		if (jack_connect(midi.client, jack_port_name(midi.port), ports[0]))
			cane::general_error(cane::STR_MIDI_PATCH_ERROR);


		// Compiler
		if (filename.empty())
			cane::general_error(cane::STR_OPT_NO_FILE);

		auto path = std::filesystem::current_path() / std::filesystem::path { filename };
		std::filesystem::current_path(path.parent_path());

		std::string in = read_file(path);

		cane::View src { &*in.begin(), &*in.end() };
		cane::Lexer lx { src };

		if (not cane::utf_validate(src))
			lx.error(cane::Phases::ENCODING, src, cane::STR_ENCODING);

		namespace time = std::chrono;

		using clock = time::steady_clock;
		using unit = time::microseconds;

		auto t1 = clock::now();
			cane::Context ctx = cane::compile(lx);
		auto t2 = clock::now();

		time::duration<double, std::micro> t = t2 - t1;
		CANE_LOG(cane::LOG_SUCC, CANE_ANSI_FG_YELLOW "took: {}µs" CANE_ANSI_RESET, t.count());

		if (ctx.timeline.empty())
			return 0;


		// Sequencer
		size_t dt = 0;
		auto it = ctx.timeline.begin();

		while (true) {
			// Gather all events we need to send now.
			for (; it != ctx.timeline.end() and it->time <= dt; ++it)
				midi.events.emplace_back(*it);

			if (it == ctx.timeline.end())
				break;

			// Wait until the next event.
			// We wait for successively shorter times until we apprach the
			// target until we end up in a busy loop to make sure we keep
			// latency from the OS scheduler to a minimum while also not
			// turning the CPU into a glorified heater.
			auto slpt = it->time - dt;
			auto target = std::chrono::steady_clock::now() + std::chrono::milliseconds { slpt };

			while (std::chrono::steady_clock::now() < target) {
				slpt /= 2;

				if (slpt < 10)
					continue;

				std::this_thread::sleep_for(std::chrono::milliseconds { slpt });
			}

			dt += (it->time - dt);
		}
	}

	catch (cane::Error) {
		return 1;
	}

	return 0;
}
