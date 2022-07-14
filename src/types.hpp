#ifndef CANE_TYPES_HPP
#define CANE_TYPES_HPP

namespace cane {

struct Token {
	cane::View view = sym2str(Symbols::NONE);
	cane::Symbols kind = Symbols::NONE;
};

using Unit        = std::chrono::microseconds;
using UnitSeconds = std::chrono::duration<double>;
using UnitMillis  = std::chrono::duration<double, std::milli>;

constexpr auto ONE_MIN = std::chrono::duration_cast<Unit>(std::chrono::minutes { 1 });

struct Event {
	Unit duration;
	uint8_t note;
	uint8_t velocity;
	uint8_t kind;

	constexpr Event(uint8_t kind_):
		duration(DURATION_DEFAULT),
		note(NOTE_DEFAULT),
		velocity(VELOCITY_DEFAULT),
		kind(kind_) {}

	constexpr Event(Unit duration_, uint8_t note_, uint8_t vel_, uint8_t kind_):
		duration(duration_),
		note(note_),
		velocity(vel_),
		kind(kind_) {}
};

struct MidiEvent {
	Unit time;
	std::array<uint8_t, 3> data;

	constexpr MidiEvent(Unit time_, uint8_t status, uint8_t note, uint8_t velocity):
		time(time_), data({status, note, velocity}) {}
};

enum {
	SEQ_NONE,
	SEQ_NOTE      = 1 << 0,
	SEQ_DURATION  = 1 << 1,
};

struct Sequence: public std::vector<Event> {
	uint8_t flags = SEQ_NONE;
	Sequence(): std::vector<Event>::vector() {}
};

struct Timeline: public std::vector<MidiEvent> {
	Unit duration = Unit::zero();
	Timeline(): std::vector<MidiEvent>::vector() {}
};

enum class HandlerKind {
	Error,
	Warning,
	Notice,
};

using Handler = void(*)(HandlerKind, Phases, View, View, std::string);

struct Context {
	std::unordered_map<View, double> constants;
	std::unordered_map<View, Sequence> chains;

	std::unordered_set<View> symbols;

	Timeline tl;
	Unit time = Unit::zero();

	Handler handler;

	inline Context(Handler&& handler_):
		handler(handler_) {}
};

inline std::ostream& operator<<(std::ostream& os, Sequence& s) {
	for (auto& [dur, note, vel, kind]: s)
		print(os, step2colour(kind), step2str(kind));

	return print(os, CANE_RESET);
}


inline std::ostream& operator<<(std::ostream& os, Timeline& tl) {
	constexpr auto longest = *std::max_element(MIDI_TO_STRING.begin(), MIDI_TO_STRING.end(), [] (auto& lhs, auto& rhs) {
		return lhs.size() < rhs.size();
	});

	for (MidiEvent& ev: tl) {
		View sv = int2midi(ev.data[0]);
		std::string padding(longest.size() - sv.size(), ' ');

		// French flag
		print(os, CANE_BLUE, sv, padding, CANE_RESET " ");

		print(os, "[ ", CANE_BOLD, (int)ev.data[1], " ");
		print(os, (int)ev.data[2], CANE_RESET " ] ");

		print(os, CANE_RED, UnitMillis { ev.time }.count(), cane::STR_MILLI_SUFFIX, CANE_RESET);
		println(os);
	}

	return os;
}

}

#endif
