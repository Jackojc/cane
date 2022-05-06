#include <iostream>
#include <string>
#include <string_view>
#include <chrono>
#include <thread>

#include <report.hpp>
#include <view.hpp>
#include <lib.hpp>

#include <rtmidi/RtMidi.h>

constexpr decltype(auto) rtmidi2str(RtMidiError::Type t) {
	cane::View sv;

	switch (t) {
		case RtMidiError::Type::WARNING:           { sv = cane::STR_RTMIDI_WARNING;           } break;
		case RtMidiError::Type::DEBUG_WARNING:     { sv = cane::STR_RTMIDI_DEBUG_WARNING;     } break;
		case RtMidiError::Type::UNSPECIFIED:       { sv = cane::STR_RTMIDI_UNSPECIFIED;       } break;
		case RtMidiError::Type::NO_DEVICES_FOUND:  { sv = cane::STR_RTMIDI_NO_DEVICES_FOUND;  } break;
		case RtMidiError::Type::INVALID_DEVICE:    { sv = cane::STR_RTMIDI_INVALID_DEVICE;    } break;
		case RtMidiError::Type::MEMORY_ERROR:      { sv = cane::STR_RTMIDI_MEMORY_ERROR;      } break;
		case RtMidiError::Type::INVALID_PARAMETER: { sv = cane::STR_RTMIDI_INVALID_PARAMETER; } break;
		case RtMidiError::Type::INVALID_USE:       { sv = cane::STR_RTMIDI_INVALID_USE;       } break;
		case RtMidiError::Type::DRIVER_ERROR:      { sv = cane::STR_RTMIDI_DRIVER_ERROR;      } break;
		case RtMidiError::Type::SYSTEM_ERROR:      { sv = cane::STR_RTMIDI_SYSTEM_ERROR;      } break;
		case RtMidiError::Type::THREAD_ERROR:      { sv = cane::STR_RTMIDI_THREAD_ERROR;      } break;
	}

	return sv;
}

inline void rtmidi_callback(RtMidiError::Type, const std::string&, void*) {}


int main(int, const char*[]) {
	constexpr size_t bpm = 120;
	constexpr std::string_view device = "Midi";

	try {
		std::istreambuf_iterator<char> begin(std::cin), end;
		std::string in(begin, end);

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
		cane::printlnfmt(CANE_ANSI_FG_YELLOW "took: {}µs" CANE_ANSI_RESET, t.count());


		using namespace std::chrono_literals;

		RtMidiOut midi { RtMidi::Api::UNSPECIFIED, "cane" };
		midi.setErrorCallback(&rtmidi_callback);

		for (size_t i = 0; i < midi.getPortCount(); ++i) {
			auto name = midi.getPortName(i);
			cane::println(i, ": ", name);

			if (name.find(device) != std::string::npos) {
				midi.openPort(i, "cane");
				cane::general_notice("found '{}'!", name);
				break;
			}
		}

		if (not midi.isPortOpen()) {
			cane::general_error("no device found!");
			return 1;
		}

		cane::println("waiting 3s...");
		std::this_thread::sleep_for(3s);

		size_t dt = 0;
		for (auto it = ctx.timeline.begin(); it != ctx.timeline.end();) {
			auto begin = it;

			while (it != ctx.timeline.end() and it->time < dt) {
				cane::println(*it);
				auto msg = it->message();
				midi.sendMessage(msg.data(), msg.size());
				++it;
			}

			auto target = std::chrono::steady_clock::now() + 1ms;
			while (std::chrono::steady_clock::now() < target);

			dt++;
		}
	}

	catch (cane::Error) {
		return 1;
	}

	catch (RtMidiError& e) {
		cane::general_error(rtmidi2str(e.getType()));
		return 1;
	}

	return 0;
}
