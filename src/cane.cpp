#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <string_view>
#include <chrono>
#include <thread>
#include <filesystem>
#include <memory>

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
	OPT_HELP = 0b001,
	OPT_LIST = 0b010,
};

inline std::string read_file(std::filesystem::path path) {
	try {
		std::filesystem::path cur = path;

		while (std::filesystem::is_symlink(cur)) {
			std::filesystem::path tmp = std::filesystem::read_symlink(cur);

			if (tmp == cur)
				cane::general_error(cane::STR_SYMLINK_ERROR, path.string());

			cur = tmp;
		}

		if (std::filesystem::is_directory(cur) or std::filesystem::is_other(cur))
			cane::general_error(cane::STR_NOT_FILE_ERROR, path.string());

		if (not std::filesystem::exists(cur))
			cane::general_error(cane::STR_FILE_NOT_FOUND_ERROR, path.string());

		std::ifstream is(cur);

		if (not is.is_open())
			cane::general_error(cane::STR_FILE_READ_ERROR, path.string());

		std::stringstream ss;
		ss << is.rdbuf();

		return ss.str();
	}

	catch (const std::filesystem::filesystem_error&) {
		cane::general_error(cane::STR_FILE_READ_ERROR, path.string());
	}
}

int main(int argc, const char* argv[]) {
	std::string_view device;
	std::string_view filename;
	std::string_view bpm;
	std::string_view note;
	uint64_t flags;

	auto parser = conflict::parser {
		conflict::option { { 'h', "help", "show help" }, flags, OPT_HELP },
		conflict::option { { 'l', "list", "list available midi devices" }, flags, OPT_LIST },

		conflict::string_option { { 'f', "file", "input file" }, "filename", filename },
		conflict::string_option { { 'm', "midi", "midi device to connect to" }, "device", device },
		conflict::string_option { { 'b', "bpm",  "global bpm" }, "bpm", bpm },
		conflict::string_option { { 'n', "note", "base note" }, "note", note },
	};

	parser.apply_defaults();
	auto status = parser.parse(argc - 1, argv + 1);

	try {
		// Handle argument parsing errors.
		switch (status.err) {
			case conflict::error::invalid_option:
				cane::general_error(cane::STR_OPT_INVALID_OPTION, status.what1);

			case conflict::error::invalid_argument:
				cane::general_error(cane::STR_OPT_INVALID_ARG, status.what1, status.what2);

			case conflict::error::missing_argument:
				cane::general_error(cane::STR_OPT_MISSING_ARG, status.what1);

			case::conflict::error::ok:
				break;
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

			jack_nframes_t sample_rate = 0;
			jack_nframes_t buffer_size = 0;
			cane::Unit time = cane::Unit::zero();

			cane::Timeline::iterator it;
			cane::Timeline::const_iterator end;

			cane::Timeline events;

			~JackData() {
				if (client != nullptr)
					jack_deactivate(client);

				if (port != nullptr)
					jack_port_unregister(client, port);

				if (client != nullptr)
					jack_client_close(client);
			}
		} midi {};


		// Connect to JACK
		if (not (midi.client = jack_client_open(cane::CSTR_EXE, JackOptions::JackNoStartServer, nullptr)))
			cane::general_error(cane::STR_CONNECT_ERROR);


		// Sample rate changed callback. We use the sample rate to determine timing
		// information so this is crucial.
		if (jack_set_sample_rate_callback(midi.client, [] (jack_nframes_t sample_rate, void* arg) {
			JackData& midi = *static_cast<JackData*>(arg);

			cane::general_warning(cane::STR_SAMPLE_RATE_CHANGE, midi.sample_rate, sample_rate);
			midi.sample_rate = sample_rate;

			return 0;
		}, static_cast<void*>(&midi)))
			cane::general_error(cane::STR_SAMPLE_RATE_CALLBACK_ERROR);


		// Notify of buffer size changes
		if (jack_set_buffer_size_callback(midi.client, [] (jack_nframes_t buffer_size, void* arg) {
			JackData& midi = *static_cast<JackData*>(arg);

			cane::general_warning(cane::STR_BUFFER_SIZE_CHANGE, midi.buffer_size, buffer_size);
			midi.buffer_size = buffer_size;

			return 0;
		}, static_cast<void*>(&midi)))
			cane::general_error(cane::STR_BUFFER_SIZE_CALLBACK_ERROR);


		// Notify of port connections
		if (jack_set_port_connect_callback(midi.client, [] (
			jack_port_id_t a, jack_port_id_t b, int connect, void* arg
		) {
			JackData& midi = *static_cast<JackData*>(arg);

			const char* name_a = jack_port_name(jack_port_by_id(midi.client, a));
			const char* name_b = jack_port_name(jack_port_by_id(midi.client, b));

			if (connect)
				cane::general_warning(cane::STR_PORT_CONNECT, name_a, name_b);
		}, static_cast<void*>(&midi)))
			cane::general_error(cane::STR_PORT_CONNECT_CALLBACK_ERROR);


		// Notify of port register or unregister
		if (jack_set_port_registration_callback(midi.client, [] (
			jack_port_id_t port, int reg, void* arg
		) {
			JackData& midi = *static_cast<JackData*>(arg);

			const char* name = jack_port_name(jack_port_by_id(midi.client, port));
			cane::View str = reg ? cane::STR_PORT_REGISTER: cane::STR_PORT_UNREGISTER;

			cane::general_warning(str, name);
		}, static_cast<void*>(&midi)))
			cane::general_error(cane::STR_PORT_REGISTRATION_CALLBACK_ERROR);


		// Notify of port rename
		if (jack_set_port_rename_callback(midi.client, [] (
			jack_port_id_t port, const char* old_name, const char* new_name, void* arg
		) {
			JackData& midi = *static_cast<JackData*>(arg);
			cane::general_warning(cane::STR_PORT_RENAME, old_name, new_name);
		}, static_cast<void*>(&midi)))
			cane::general_error(cane::STR_PORT_RENAME_CALLBACK_ERROR);


		// MIDI out callback
		if (jack_set_process_callback(midi.client, [] (jack_nframes_t nframes, void *arg) {
			auto& [client, port, sample_rate, buffer_size, time, it, end, events] = *static_cast<JackData*>(arg);

			void* out_buffer = jack_port_get_buffer(port, nframes);
			jack_midi_clear_buffer(out_buffer);

			// Copy every MIDI event into the buffer provided by JACK.
			for (; it != end and it->time <= time; ++it) {
				if (jack_midi_event_write(out_buffer, 0, it->data.data(), it->data.size()))
					cane::general_error(cane::STR_WRITE_ERROR);
			}

			size_t lost = 0;
			if ((lost = jack_midi_get_lost_event_count(out_buffer)))
				cane::general_warning(cane::STR_LOST_EVENT, lost);

			time += std::chrono::duration_cast<cane::Unit>(std::chrono::seconds { nframes }) / sample_rate;

			return 0;
		}, static_cast<void*>(&midi)))
			cane::general_error(cane::STR_PROCESS_CALLBACK_ERROR);


		// If no device is specified _and_ `-l` is not passed,
		// throw an error. It's perfectly valid to give an empty
		// string to JACK here and it will give us back a list
		// of ports regardless.
		if (device.empty() and (flags & OPT_LIST) != OPT_LIST)
			cane::general_error(cane::STR_NO_DEVICE);


		// Register port and get information.
		if (not (midi.port = jack_port_register(midi.client, cane::CSTR_PORT, JACK_DEFAULT_MIDI_TYPE, JackPortIsOutput, 0)))
			cane::general_error(cane::STR_PORT_ERROR);

		midi.buffer_size = jack_get_buffer_size(midi.client);
		midi.sample_rate = jack_get_sample_rate(midi.client);


		// Get an array of all MIDI input ports that we could potentially connect to.
		JackPorts ports { jack_get_ports(midi.client, std::string { device }.c_str(), JACK_DEFAULT_MIDI_TYPE, JackPortIsInput) };

		if (not ports)  // Error occured
			cane::general_error(cane::STR_GET_PORTS_ERROR);

		if (not ports[0])  // No MIDI input ports.
			cane::general_error(cane::STR_NOT_FOUND, device);

		// Print all devices if list option passed
		if (flags & OPT_LIST) {
			for (size_t i = 0; ports[i] != nullptr; ++i)
				cane::general_notice(cane::STR_DEVICE, ports[i]);

			return 0;
		}

		// Print port that we're going to use.
		cane::general_notice(cane::STR_FOUND, ports[0]);

		if (jack_connect(midi.client, jack_port_name(midi.port), ports[0]))
			cane::general_error(cane::STR_PATCH_ERROR);


		// Compiler
		if (filename.empty())
			cane::general_error(cane::STR_OPT_NO_FILE);

		if (bpm.empty())
			cane::general_error(cane::STR_NO_BPM);

		if (note.empty())
			cane::general_error(cane::STR_NO_NOTE);

		// TODO: verify that the string has valid digits
		size_t global_bpm  = cane::b10_decode(cane::View { bpm.data(), bpm.data() + bpm.size() });
		size_t global_note = cane::b10_decode(cane::View { note.data(), note.data() + note.size() });

		std::string in = read_file(filename);

		cane::View src { &*in.begin(), &*in.end() };
		cane::Lexer lx { src };

		if (not cane::validate(src))
			lx.error(cane::Phases::ENCODING, src, cane::STR_ENCODING);


		namespace time = std::chrono;

		using clock = time::steady_clock;
		using unit = time::microseconds;

		// Compile
		auto t1 = clock::now();
			cane::Timeline timeline = cane::compile(lx, global_bpm, global_note);
		auto t2 = clock::now();


		cane::err(std::fixed, std::setprecision(2));
		cane::general_notice(cane::STR_COMPILED, cane::UnitMillis { t2 - t1 }.count(), cane::STR_MILLI_SUFFIX);

		if (timeline.empty())
			return 0;


		CANE_DEBUG_RUN(cane::print(timeline));

		// Setup MIDI events.
		// Very important that we assign these here or else
		// the sequencer will not run, or worse- start
		// sequencing garbage values.
		midi.events = timeline;

		midi.it = timeline.begin();
		midi.end = timeline.cend();

		// cane::general_notice(cane::STR_LENGTH, cane::UnitSeconds { timeline.duration }.count(), cane::STR_SECOND_SUFFIX);


		// Call this or else our callback is never called.
		if (jack_activate(midi.client))
			cane::general_error(cane::STR_ACTIVATE_ERROR);

		// Sleep until timeline is completed.
		size_t count = 1;
		size_t barw = 50;
		while (midi.it != midi.end) {
			cane::print(CANE_ANSI_BOLD "[");

			for (size_t i = 0; i != barw; ++i) {
				size_t perc = count / (100 / barw);

				if (i < perc)       cane::print(CANE_ANSI_FG_YELLOW "=");
				else if (i == perc) cane::print(CANE_ANSI_FG_YELLOW ">");
				else                cane::print(CANE_ANSI_FG_BLUE   "-");
			}

			auto so_far = cane::UnitSeconds{(timeline.duration / 100) * count}.count();
			auto remaining = cane::UnitSeconds{timeline.duration}.count();

			std::cout << std::fixed << std::setprecision(2);
			cane::print(CANE_ANSI_RESET CANE_ANSI_BOLD "] ", so_far, "s/", remaining, "s\r" CANE_ANSI_RESET);
			std::cout.flush();

			count++;
			std::this_thread::sleep_for(timeline.duration / 100);
		}

		cane::println();
	}

	catch (cane::Error) {
		return 1;
	}

	return 0;
}
