#ifndef CANE_TYPES_HPP
#define CANE_TYPES_HPP

namespace cane {

struct Token {
	cane::View view = sym2str(Symbols::NONE);
	cane::Symbols kind = Symbols::NONE;
};

using Unit = std::chrono::microseconds;

using Literal = int64_t;
using Channel = uint8_t;
using Note    = uint8_t;

enum class Events {
	BEAT, SKIP
};

struct Event {
	Note note;
	Events kind;
	Channel channel;

	constexpr Event(Events kind_):
		note(NOTE_DEFAULT),
		kind(kind_),
		channel(CHANNEL_DEFAULT) {}

	constexpr Event(Note note_, Events kind_, Channel channel_):
		note(note_),
		kind(kind_),
		channel(channel_) {}

	constexpr operator bool() {
		return kind == Events::BEAT;
	}
};

constexpr bool operator==(Event& lhs, Event& rhs) {
	return lhs.kind == rhs.kind;
}

constexpr bool operator!=(Event& lhs, Event& rhs) {
	return not(lhs == rhs);
}

constexpr Event operator&(Event& lhs, Event& rhs) {
	return {  };
}

constexpr void operator!(Event& x) {
	x.kind = x.kind == Events::BEAT ? Events::SKIP: Events::BEAT;
}

struct Sequence: public std::vector<Event> {
	Unit time = Unit::zero();
	Unit duration = Unit::zero();
	Sequence(): std::vector<Event>::vector() {}
};


struct Context {
	std::unordered_map<View, Literal> constants;
	std::unordered_map<View, Channel> channels;
	std::unordered_map<View, Sequence> definitions;

	std::unordered_set<View> symbols;

	Unit time = Unit::zero();
	Sequence seq;

	size_t global_bpm;
	size_t global_note;
};

}

#endif
