#ifndef CANE_LIB_HPP
#define CANE_LIB_HPP

#include <iostream>
#include <vector>
#include <array>
#include <unordered_map>
#include <unordered_set>
#include <algorithm>
#include <numeric>

#include <cmath>
#include <cstddef>
#include <cstdint>

#include <locale.hpp>
#include <print.hpp>
#include <view.hpp>
#include <unicode.hpp>
#include <log.hpp>
#include <report.hpp>

namespace cane {

// Constants
constexpr size_t CHANNEL_MIN      = 1u;
constexpr size_t CHANNEL_MAX      = 16u;
constexpr size_t BPM_MIN          = 1u;
constexpr size_t VELOCITY_DEFAULT = 127u;

constexpr auto ACTIVE_SENSING_INTERVAL = std::chrono::milliseconds { 250 };

constexpr auto ALL_SOUND_OFF = 120;
constexpr auto ALL_RESET_CC  = 121;
constexpr auto LOCAL_CONTROL = 122;
constexpr auto ALL_NOTES_OFF = 123;

constexpr auto OMNI_MODE_OFF = 124;
constexpr auto OMNI_MODE_ON  = 125;
constexpr auto MONO_MODE_ON  = 126;
constexpr auto POLY_MODE_ON  = 127;

constexpr auto LOCAL_CONTROL_ON  = 127;
constexpr auto LOCAL_CONTROL_OFF = 0;

#define MIDI \
	X(NOTE_OFF_1,   0b10000000) \
	X(NOTE_OFF_2,   0b10000001) \
	X(NOTE_OFF_3,   0b10000010) \
	X(NOTE_OFF_4,   0b10000011) \
	X(NOTE_OFF_5,   0b10000100) \
	X(NOTE_OFF_6,   0b10000101) \
	X(NOTE_OFF_7,   0b10000110) \
	X(NOTE_OFF_8,   0b10000111) \
	X(NOTE_OFF_9,   0b10001000) \
	X(NOTE_OFF_10,  0b10001001) \
	X(NOTE_OFF_11,  0b10001010) \
	X(NOTE_OFF_12,  0b10001011) \
	X(NOTE_OFF_13,  0b10001100) \
	X(NOTE_OFF_14,  0b10001101) \
	X(NOTE_OFF_15,  0b10001110) \
	X(NOTE_OFF_16,  0b10001111) \
	\
	X(NOTE_ON_1,    0b10010000) \
	X(NOTE_ON_2,    0b10010001) \
	X(NOTE_ON_3,    0b10010010) \
	X(NOTE_ON_4,    0b10010011) \
	X(NOTE_ON_5,    0b10010100) \
	X(NOTE_ON_6,    0b10010101) \
	X(NOTE_ON_7,    0b10010110) \
	X(NOTE_ON_8,    0b10010111) \
	X(NOTE_ON_9,    0b10011000) \
	X(NOTE_ON_10,   0b10011001) \
	X(NOTE_ON_11,   0b10011010) \
	X(NOTE_ON_12,   0b10011011) \
	X(NOTE_ON_13,   0b10011100) \
	X(NOTE_ON_14,   0b10011101) \
	X(NOTE_ON_15,   0b10011110) \
	X(NOTE_ON_16,   0b10011111) \
	\
	X(NOTE_ON,      0b10010000) \
	X(NOTE_OFF,     0b10000000) \
	X(START,        0b11111010) \
	X(STOP,         0b11111100) \
	X(ACTIVE_SENSE, 0b11111110) \
	X(TIMING_CLOCK, 0b11111000) \
	X(CHANNEL_MODE, 0b10110000)

	#define X(name, value) name,
		enum class Midi { MIDI };
	#undef X

	#define X(name, value) value,
		constexpr std::array MIDI_TO_INT = { MIDI };
	#undef X

	#define X(name, value) #name##_sv,
		constexpr std::array MIDI_TO_STRING = { MIDI };
	#undef X

	#define X(name, value) m == Midi::name ? value:
		constexpr decltype(auto) midi2int(Midi m) {
			return MIDI 0;
		}
	#undef X

	constexpr decltype(auto) midi2str(Midi m) {
		return MIDI_TO_STRING[(int)m];
	}

	#define X(name, value) x == value ? midi2str(Midi::name) :
		constexpr decltype(auto) int2midi(uint8_t x) {
			return MIDI "NONE"_sv;
		}
	#undef X

#undef MIDI


// Errors/Warnings/Notices/Exceptions
struct Error {};

template <typename... Ts>
[[noreturn]] inline void report_error(Ts&&... args) {
	report<Reports::ERROR>(std::cerr, std::forward<Ts>(args)...);
	throw Error {};
}

template <typename... Ts>
inline void report_warning(Ts&&... args) {
	report<Reports::WARNING>(std::cerr, std::forward<Ts>(args)...);
}

template <typename... Ts>
inline void report_notice(Ts&&... args) {
	report<Reports::NOTICE>(std::cerr, std::forward<Ts>(args)...);
}


template <typename... Ts>
[[noreturn]] inline void general_error(Ts&&... args) {
	general_report<Reports::ERROR>(std::cerr, std::forward<Ts>(args)...);
	throw Error {};
}

template <typename... Ts>
inline void general_warning(Ts&&... args) {
	general_report<Reports::WARNING>(std::cerr, std::forward<Ts>(args)...);
}

template <typename... Ts>
inline void general_notice(Ts&&... args) {
	general_report<Reports::NOTICE>(std::cerr, std::forward<Ts>(args)...);
}


// Symbols
#define SYMBOL_TYPES \
	X(NONE,       "none") \
	X(TERMINATOR, "eof") \
	\
	/* Special */ \
	X(LITERAL, "literal") \
	X(IDENT,   "identfier") \
	X(INT,     "int") \
	X(HEX,     "hex") \
	X(BIN,     "bin") \
	\
	/* Channel Keywords */ \
	X(LOOP,  "loop") \
	X(JOIN,  "join") \
	X(WITH,  "with") \
	X(DEF,   "def") \
	\
	/* Sequence Keywords */ \
	X(ALIAS, "alias") \
	X(FIT,   "fit") \
	X(CAR,   "car") \
	X(CDR,   "cdr") \
	\
	/* Grouping */ \
	X(LPAREN,   "(") \
	X(RPAREN,   ")") \
	X(LBRACKET, "[") \
	X(RBRACKET, "]") \
	X(LBRACE,   "{") \
	X(RBRACE,   "}") \
	\
	/* Literal Operators */ \
	X(ADD, "+") \
	X(SUB, "-") \
	X(MUL, "*") \
	X(DIV, "/") \
	X(MOD, "%") \
	\
	/* Literal Keywords */ \
	X(LEN_OF,     "len") \
	X(LET,        "let") \
	X(BPM_GLOBAL, "bpm") \
	\
	/* Sequence */ \
	X(SEP,  ":") \
	X(SKIP, ".") \
	X(BEAT, "!") \
	\
	/* Argument Sequence Operators */ \
	X(CHAIN, "=>") \
	X(SINK,  "~>") \
	X(ROTLN, "<<") \
	X(ROTRN, ">>") \
	X(REPN,  "**") \
	X(BPM,   "@") \
	\
	/* Bitwise Sequence Operators */ \
	X(OR,  "|") \
	X(AND, "&") \
	X(XOR, "^") \
	X(CAT, ",") \
	\
	/* Prefix Sequence Operators */ \
	X(INVERT, "~") \
	X(REV,    "'") \
	X(REF,    "$") \
	\
	/* Postfix Sequence Operators */ \
	X(DBG, "?")

	#define X(name, str) name,
		enum class Symbols: int { SYMBOL_TYPES };
	#undef X

	#define X(name, str) str##_sv,
		constexpr View SYMBOL_TO_STRING[] = { SYMBOL_TYPES };
	#undef X

	constexpr decltype(auto) sym2str(Symbols s) {
		return SYMBOL_TO_STRING[(int)s];
	}

#undef SYMBOL_TYPES

inline std::ostream& operator<<(std::ostream& os, Symbols s) {
	return (os << sym2str(s));
}


// Lexer
struct Token {
	cane::View view = sym2str(Symbols::NONE);
	cane::Symbols kind = Symbols::NONE;
};

struct Lexer {
	cane::View original {};
	cane::View src {};

	cane::Token peek_ {};
	cane::Token prev_ {};

	constexpr Lexer(cane::View src_): original(src_), src(src_) {
		next();  // this can throw
	}

	template <typename F, typename... Ts>
	void expect(const F& fn, View sv, Ts&&... args) {
		if (not fn(peek().kind))
			report_error(Phases::SYNTACTIC, original, sv, std::forward<Ts>(args)...);
	}

	template <typename... Ts>
	[[noreturn]] void error(Phases phase, View sv, Ts&&... args) {
		report_error(phase, original, sv, std::forward<Ts>(args)...);
	}

	template <typename... Ts>
	void warning(Phases phase, View sv, Ts&&... args) {
		report_warning(phase, original, sv, std::forward<Ts>(args)...);
	}

	template <typename... Ts>
	void notice(Phases phase, View sv, Ts&&... args) {
		report_notice(phase, original, sv, std::forward<Ts>(args)...);
	}

	inline Token peek() const {
		return peek_;
	}

	inline Token prev() const {
		return prev_;
	}

	inline Token next() {
		Token tok {};

		auto& [sbegin, send] = src;
		auto& [view, kind] = tok;
		auto& [begin, end] = view;

		// Skip whitespace.
		View ws = cane::consume_decode(src, cane::is_whitespace);
		view = cane::peek(src);

		if (src.is_eof()) {
			kind = Symbols::TERMINATOR;
		}

		else if (cane::peek(src) == "#"_sv) {
			view = cane::consume(src, not_equal("\n"_sv));  // Skip until \n and then return next token.
			return next();
		}

		else if (view == "("_sv) { kind = Symbols::LPAREN;   src = cane::next(src); }
		else if (view == ")"_sv) { kind = Symbols::RPAREN;   src = cane::next(src); }
		else if (view == "{"_sv) { kind = Symbols::LBRACE;   src = cane::next(src); }
		else if (view == "}"_sv) { kind = Symbols::RBRACE;   src = cane::next(src); }
		else if (view == "["_sv) { kind = Symbols::LBRACKET; src = cane::next(src); }
		else if (view == "]"_sv) { kind = Symbols::RBRACKET; src = cane::next(src); }

		else if (view == "!"_sv) { kind = Symbols::BEAT; src = cane::next(src); }
		else if (view == "."_sv) { kind = Symbols::SKIP; src = cane::next(src); }
		else if (view == ":"_sv) { kind = Symbols::SEP;  src = cane::next(src); }

		else if (view == "?"_sv)  { kind = Symbols::DBG;  src = cane::next(src); }
		else if (view == "\'"_sv) { kind = Symbols::REV;  src = cane::next(src); }
		else if (view == "@"_sv)  { kind = Symbols::BPM;  src = cane::next(src); }
		else if (view == "|"_sv)  { kind = Symbols::OR;   src = cane::next(src); }
		else if (view == "^"_sv)  { kind = Symbols::XOR;  src = cane::next(src); }
		else if (view == ","_sv)  { kind = Symbols::CAT;  src = cane::next(src); }
		else if (view == "#"_sv)  { kind = Symbols::REPN; src = cane::next(src); }

		else if (view == "+"_sv) { kind = Symbols::ADD; src = cane::next(src); }
		else if (view == "-"_sv) { kind = Symbols::SUB; src = cane::next(src); }
		else if (view == "/"_sv) { kind = Symbols::DIV; src = cane::next(src); }
		else if (view == "&"_sv) { kind = Symbols::AND; src = cane::next(src); }

		else if (view == "$"_sv) { kind = Symbols::REF; src = cane::next(src); }

		else if (view == "*"_sv) {
			kind = Symbols::MUL;
			src = cane::next(src);

			if (cane::peek(src) == "*"_sv) {
				kind = Symbols::REPN;
				view = encompass(view, cane::peek(src));
				src = cane::next(src);
			}
		}

		else if (view == "<"_sv) {
			src = cane::next(src);

			if (cane::peek(src) == "<"_sv) {
				kind = Symbols::ROTLN;
				view = encompass(view, cane::peek(src));
				src = cane::next(src);
			}
		}

		else if (view == ">"_sv) {
			src = cane::next(src);

			if (cane::peek(src) == ">"_sv) {
				kind = Symbols::ROTRN;
				view = encompass(view, cane::peek(src));
				src = cane::next(src);
			}
		}

		else if (view == "~"_sv) {
			kind = Symbols::INVERT;
			src = cane::next(src);

			if (cane::peek(src) == ">"_sv) {
				kind = Symbols::SINK;
				view = encompass(view, cane::peek(src));
				src = cane::next(src);
			}
		}

		else if (view == "="_sv) {
			src = cane::next(src);

			if (cane::peek(src) == ">"_sv) {
				kind = Symbols::CHAIN;
				view = encompass(view, cane::peek(src));
				src = cane::next(src);
			}
		}

		else if (cane::is_number(decode(cane::peek(src)))) {
			const auto lbegin = begin;  // Save starting position of token so we include "0x" or "0b"

			if (cane::peek(src) == "0"_sv) {
				src = cane::next(src);

				// Hex literal
				if (cane::peek(src) == "x"_sv) {
					kind = Symbols::HEX;
					src = cane::next(src);
					view = cane::consume_decode(src, [] (cp c) {
						return
							(c >= 'A' and c <= 'Z') or
							(c >= 'a' and c <= 'z') or
							(c >= '0' and c <= '9');
					});
				}

				// Binary literal
				else if (cane::peek(src) == "b"_sv) {
					kind = Symbols::BIN;
					src = cane::next(src);
					view = cane::consume_decode(src, [] (cp c) {
						return c == '0' or c == '1';
					});
				}
			}

			kind = Symbols::INT;
			view = cane::consume_decode(src, cane::is_number);

			begin = lbegin;  // Make sure to set the begin pointer here so that we handle the lone 0 case aswell.
		}

		else if (cane::is_letter(decode(cane::peek(src))) or view == "_"_sv) {
			kind = Symbols::IDENT;
			view = cane::consume_decode(src, [] (cp c) {
				return cane::is_alphanumeric(c) or c == '_';
			});

			if      (view == "with"_sv)  kind = Symbols::WITH;
			else if (view == "join"_sv)  kind = Symbols::JOIN;
			else if (view == "def"_sv)   kind = Symbols::DEF;
			else if (view == "loop"_sv)  kind = Symbols::LOOP;
			else if (view == "alias"_sv) kind = Symbols::ALIAS;
			else if (view == "fit"_sv)   kind = Symbols::FIT;
			else if (view == "len"_sv)   kind = Symbols::LEN_OF;
			else if (view == "let"_sv)   kind = Symbols::LET;
			else if (view == "car"_sv)   kind = Symbols::CAR;
			else if (view == "cdr"_sv)   kind = Symbols::CDR;
			else if (view == "bpm"_sv)   kind = Symbols::BPM_GLOBAL;
		}

		// If the kind is still NONE by this point, we can assume we didn't find
		// a valid // token. The reason this check is disconnected from the above
		// if-else chain is because some checks are nested and only fail after
		// succeeding with the original check so we wouldn't fall through if this
		// check was connected.
		if (kind == Symbols::NONE)
			report_error(Phases::LEXICAL, original, view, STR_UNKNOWN_CHAR, view);

		Token out = peek_;

		prev_ = peek_;
		peek_ = tok;

		return out;
	}
};


// Decoders (all of these functions assume correct input)
constexpr uint64_t b10_decode(View sv) {
	uint64_t n = 0;

	for (auto ptr = sv.begin; ptr != sv.end; ++ptr)
		n = (n * 10) + (*ptr - '0');

	return n;
}

constexpr uint64_t b16_decode(View sv) {
	uint64_t n = 0;
	sv = cane::next(cane::next(sv));  // skip `0x`

	// Interesting fact about ASCII:
	// A-Z and a-z are the same ranges minus a difference of a single bit.
	// We can exploit this peculiarity to make the ranges line up.
	// If we mask out the single bit that seperates the two ranges, they
	// become the same range. Another way to formulate this is to
	// mask out all but the lower 4 bits. This has the added benefit of
	// moving the ranges (now single range) into the 1-6 range. From
	// here, it's just a matter of adding 9 if the character is >= 'A'.
	for (auto ptr = sv.begin; ptr != sv.end; ++ptr)
		n = (n * 16) + (*ptr & 0xf) + (*ptr >> 6) * 9;

	return n;
}

constexpr uint64_t b2_decode(View sv) {
	uint64_t n = 0;
	sv = cane::next(cane::next(sv));  // skip `0b`

	for (auto ptr = sv.begin; ptr != sv.end; ++ptr)
		n = (n * 2) + (*ptr - '0');

	return n;
}

using Unit = std::chrono::microseconds;

using UnitSeconds = std::chrono::duration<double>;
using UnitMillis = std::chrono::duration<double, std::milli>;

constexpr auto ONE_MIN = std::chrono::duration_cast<Unit>(std::chrono::minutes { 1 });


struct Event {
	Unit time = Unit::zero();
	std::array<uint8_t, 3> data;

	constexpr Event(Unit time_, uint8_t status_, uint8_t note_, uint8_t velocity_):
		time(time_), data({status_, note_, velocity_})
	{}
};


using Channel = uint8_t;
using Literal = int64_t;
using Step = uint8_t;
using Note = uint8_t;


struct Timeline: public std::vector<Event> {
	Unit duration = Unit::zero();
	Timeline(): std::vector<Event>::vector() {}
};


inline std::ostream& operator<<(std::ostream& os, Timeline& tl) {
	constexpr auto longest = *std::max_element(MIDI_TO_STRING.begin(), MIDI_TO_STRING.end(), [] (auto& lhs, auto& rhs) {
		return lhs.size() < rhs.size();
	});

	for (Event& ev: tl) {
		View sv = int2midi(ev.data[0]);
		std::string padding(longest.size() - sv.size(), ' ');

		// French flag
		out(os, CANE_ANSI_FG_BLUE, sv, padding, CANE_ANSI_RESET " ");

		out(os, "[ ", CANE_ANSI_BOLD, (int)ev.data[1], " ");
		out(os, (int)ev.data[2], CANE_ANSI_RESET " ] ");

		out(os, CANE_ANSI_FG_RED, UnitMillis { ev.time }.count(), cane::STR_MILLI_SUFFIX, CANE_ANSI_RESET);
		outln(os);
	}

	return os;
}


#define STEPS \
	X(SKIP, Symbols::SKIP, CANE_ANSI_FG_BLUE) \
	X(BEAT, Symbols::BEAT, CANE_ANSI_FG_YELLOW)

	#define X(name, sym, colour) name,
		enum Steps: Step { STEPS };
	#undef X

	#define X(name, sym, colour) sym2str(sym),
		constexpr View STEP_TO_STRING[] = { STEPS };
	#undef X

	#define X(name, sym, colour) #name##_sv,
		constexpr View STEP_TO_NAME[] = { STEPS };
	#undef X

	#define X(name, str, colour) colour,
		constexpr View STEP_TO_COLOUR[] = { STEPS };
	#undef X

	constexpr decltype(auto) step2colour(Step s) {
		return STEP_TO_COLOUR[s];
	}

	constexpr decltype(auto) step2str(Step s) {
		return STEP_TO_STRING[s];
	}

	constexpr decltype(auto) step2name(Step s) {
		return STEP_TO_NAME[s];
	}

	#define X(name, sym, colour) s == sym ? name:
		constexpr decltype(auto) sym2step(Symbols s) {
			return STEPS 0;
		}
	#undef X

#undef STEPS


struct Sequence: public std::vector<Step> {
	Sequence(): std::vector<Step>::vector() {}
};


// Identifies repeating pattern in a sequence
// and attempts to minify it so we don't spam
// the stdout for large sequences.
template <typename V>
inline decltype(auto) minify(V v) {
	for (size_t f = 1; f != v.size(); ++f) {
		if (v.size() % f != 0)
			continue;

		bool all_eq = false;
		for (size_t i = 1; i != (v.size() / f); ++i) {
			all_eq = std::equal(v.begin(), v.begin() + f, v.begin() + (f * i));

			if (not all_eq)
				break;
		}

		if (all_eq) {
			v.erase(v.begin() + f, v.end());
			return v;
		}
	}

	return v;
}

inline std::ostream& operator<<(std::ostream& os, Sequence& s) {
	for (Step x: s)
		out(os, step2colour(x), step2str(x));

	return out(os, CANE_ANSI_RESET);
}


struct Context {
	std::unordered_map<View, Sequence> chains;
	std::unordered_map<View, Literal>  constants;
	std::unordered_map<View, Timeline> definitions;
	std::unordered_map<View, Channel>   channels;

	std::unordered_set<View> symbols;

	Unit base_time = Unit::zero();  // Time at which to begin new channels
	Timeline timeline;

	size_t bpm_global;
	size_t note_global;
};


// Operations
template <typename V> constexpr decltype(auto) reverse(V v) {
	std::reverse(v.begin(), v.end());
	return v;
}

template <typename V> constexpr decltype(auto) rotl(V v, size_t n = 1) {
	std::rotate(v.begin(), v.begin() + (n % v.size()), v.end());
	return v;
}

template <typename V> constexpr decltype(auto) rotr(V v, size_t n = 1) {
	std::rotate(v.rbegin(), v.rbegin() + (n % v.size()), v.rend());
	return v;
}

template <typename V> constexpr decltype(auto) invert(V v) {
	std::transform(v.begin(), v.end(), v.begin(), std::logical_not<>{});
	return v;
}

template <typename V> constexpr decltype(auto) repeat(V v, size_t n = 1) {
	// Copy sequence N times to the end of itself.
	// turns i.e. `[a b c]` where N=3 into `[a b c a b c a b c]`.

	if (n == 0)
		return v;

	size_t count = v.size();
	v.reserve(v.capacity() + n * count);

	while (--n)
		std::copy_n(v.begin(), count, std::back_inserter(v));

	return v;
}

template <typename V1, typename V2> constexpr decltype(auto) cat(V1 a, V2 b) {
	a.insert(a.end(), b.begin(), b.end());
	return a;
}

template <typename V1, typename V2> constexpr decltype(auto) disjunction(V1 a, V2 b) {
	std::transform(b.cbegin(), b.cend(), a.begin(), a.begin(), std::bit_or<>{});
	return a;
}

template <typename V1, typename V2> constexpr decltype(auto) conjunction(V1 a, V2 b) {
	std::transform(b.cbegin(), b.cend(), a.begin(), a.begin(), std::bit_and<>{});
	return a;
}

template <typename V1, typename V2> constexpr decltype(auto) ex_disjunction(V1 a, V2 b) {
	std::transform(b.cbegin(), b.cend(), a.begin(), a.begin(), std::bit_xor<>{});
	return a;
}


// Literal expressions
[[nodiscard]] inline Literal literal (Context&, Lexer&, View);

[[nodiscard]] inline Literal lit_ref    (Context&, Lexer&, View);
[[nodiscard]] inline Literal lit_prefix (Context&, Lexer&, View, size_t);
[[nodiscard]] inline Literal lit_infix  (Context&, Lexer&, View, Literal, size_t);
[[nodiscard]] inline Literal lit_expr   (Context&, Lexer&, View, size_t);

// Sequence expressions
[[nodiscard]] inline Sequence sequence (Context&, Lexer&, View);
[[nodiscard]] inline Sequence euclide  (Context&, Lexer&, View);

[[nodiscard]] inline Sequence seq_ref        (Context&, Lexer&, View);
[[nodiscard]] inline Sequence seq_prefix     (Context&, Lexer&, View, size_t);
[[nodiscard]] inline Sequence seq_infix_expr (Context&, Lexer&, View, Sequence, size_t);
[[nodiscard]] inline Sequence seq_infix_lit  (Context&, Lexer&, View, Sequence, size_t);
[[nodiscard]] inline Sequence seq_postfix    (Context&, Lexer&, View, Sequence, size_t);
[[nodiscard]] inline Sequence seq_expr       (Context&, Lexer&, View, size_t);

// Channel expressions
[[nodiscard]] inline Channel channel (Context&, Lexer&);
inline void sink(Context&, Lexer&, Timeline);

[[nodiscard]] inline Timeline chan_ref        (Context&, Lexer&, View);
[[nodiscard]] inline Timeline chan_prefix     (Context&, Lexer&, View, size_t);
[[nodiscard]] inline Timeline chan_infix_expr (Context&, Lexer&, View, Timeline, size_t);
[[nodiscard]] inline Timeline chan_infix_lit  (Context&, Lexer&, View, Timeline, size_t);
[[nodiscard]] inline Timeline chan_expr       (Context&, Lexer&, View, size_t);

// Statements
inline void                   stat    (Context&, Lexer&, View);
[[nodiscard]] inline Timeline compile (Lexer&);


// Predicates
constexpr auto is_lit = partial_eq_any(Symbols::INT, Symbols::HEX, Symbols::BIN);
constexpr auto is_step = partial_eq_any(Symbols::SKIP, Symbols::BEAT);

constexpr auto is_lit_prefix = partial_eq_any(
	Symbols::ADD,
	Symbols::SUB,
	Symbols::LEN_OF
);

constexpr auto is_lit_infix = partial_eq_any(
	Symbols::ADD,
	Symbols::SUB,
	Symbols::MUL,
	Symbols::DIV,
	Symbols::MOD
);

constexpr auto is_seq_prefix = partial_eq_any(Symbols::INVERT, Symbols::REV);
constexpr auto is_seq_postfix = partial_eq_any(
	Symbols::CAR,
	Symbols::CDR,
	Symbols::DBG
);

constexpr auto is_seq_infix_expr = partial_eq_any(
	Symbols::OR,
	Symbols::AND,
	Symbols::XOR,
	Symbols::CAT
);

constexpr auto is_seq_infix_lit = partial_eq_any(
	Symbols::ROTLN,
	Symbols::ROTRN,
	Symbols::REPN,
	Symbols::CHAIN
);

constexpr auto is_seq_infix = [] (auto x) {
	return is_seq_infix_expr(x) or is_seq_infix_lit(x);
};

constexpr auto is_seq_primary = [] (auto x) {
	return is_lit(x) or is_seq_prefix(x) or is_step(x) or eq_any(x,
		Symbols::REF,
		Symbols::LPAREN,
		Symbols::SEP
	);
};

constexpr auto is_lit_primary = [] (auto x) {
	return is_lit(x) or is_lit_prefix(x) or eq_any(x,
		Symbols::IDENT
	);
};

constexpr auto is_seq_expr = [] (auto x) {
	return is_seq_prefix(x) or is_seq_primary(x);
};

constexpr auto is_chan_infix_expr = partial_eq_any(
	Symbols::WITH,
	Symbols::JOIN
);

constexpr auto is_chan_infix_lit = partial_eq_any(
	Symbols::LOOP
);

constexpr auto is_chan_infix = [] (auto x) {
	return is_chan_infix_expr(x) or is_chan_infix_lit(x);
};

constexpr auto is_chan_primary = [] (auto x) {
	return is_seq_expr(x) or eq_any(x,
		Symbols::IDENT,
		Symbols::LBRACE
	);
};


// Precedence table
enum class OpFix {
	LIT_PREFIX,
	LIT_INFIX,

	SEQ_PREFIX,
	SEQ_INFIX,
	SEQ_POSTFIX,

	CHAN_INFIX,
};

inline std::pair<size_t, size_t> binding_power(Lexer& lx, Token tok, OpFix fix) {
	auto [view, kind] = tok;

	enum { LEFT = 1, RIGHT = 0, };

	enum {
		JOIN,
		LOOP,
		WITH,

		DBG,

		CAR,
		CDR = CAR,

		CHAIN,

		CAT,
		OR       = CAT,
		AND      = CAT,
		XOR      = CAT,
		ROTLN    = CAT,
		ROTRN    = CAT,
		REPN     = CAT,

		REV,
		INVERT = REV,

		ADD,
		SUB = ADD,

		MUL,
		DIV = MUL,
		MOD = MUL,

		POS,
		NEG = POS,

		LEN_OF,
	};

	switch (fix) {
		// Literals
		case OpFix::LIT_PREFIX: switch (kind) {
			case Symbols::ADD:    return { 0u, POS    + RIGHT };
			case Symbols::SUB:    return { 0u, NEG    + RIGHT };
			case Symbols::LEN_OF: return { 0u, LEN_OF + RIGHT };
			default: break;
		} break;

		case OpFix::LIT_INFIX: switch (kind) {
  			case Symbols::ADD: return { ADD, ADD + LEFT };
			case Symbols::SUB: return { SUB, SUB + LEFT };
			case Symbols::MUL: return { MUL, MUL + LEFT };
			case Symbols::DIV: return { DIV, DIV + LEFT };
			case Symbols::MOD: return { MOD, MOD + LEFT };
			default: break;
		} break;

		// Sequences
		case OpFix::SEQ_PREFIX: switch (kind) {
			case Symbols::REV:    return { 0u, REV    + RIGHT };
			case Symbols::INVERT: return { 0u, INVERT + RIGHT };
			default: break;
		} break;

		case OpFix::SEQ_INFIX: switch (kind) {
			case Symbols::CHAIN: return { CHAIN, CHAIN + LEFT };

			case Symbols::CAT:   return { CAT,   CAT   + LEFT };
			case Symbols::OR:    return { OR,    OR    + LEFT };
			case Symbols::AND:   return { AND,   AND   + LEFT };
			case Symbols::XOR:   return { XOR,   XOR   + LEFT };

			case Symbols::REPN:  return { REPN,  REPN  + LEFT };
			case Symbols::ROTLN: return { ROTLN, ROTLN + LEFT };
			case Symbols::ROTRN: return { ROTRN, ROTRN + LEFT };

			default: break;
		} break;

		case OpFix::SEQ_POSTFIX: switch(kind) {
			case Symbols::DBG:  return { DBG,  DBG  + LEFT };
			case Symbols::CAR:  return { CAR,  CAR  + LEFT };
			case Symbols::CDR:  return { CDR,  CDR  + LEFT };
			default: break;
		} break;

		case OpFix::CHAN_INFIX: switch(kind) {
			case Symbols::WITH: return { WITH, WITH + LEFT };
			case Symbols::LOOP: return { LOOP, LOOP + LEFT };
			case Symbols::JOIN: return { JOIN, JOIN + LEFT };
			default: break;
		} break;
	}

	lx.error(Phases::INTERNAL, view, STR_UNREACHABLE);
}


// Utils
inline Timeline chan_repeat(Timeline tl, Literal n = 1) {
	auto dur = tl.duration;
	size_t count = tl.size();

	tl = repeat(std::move(tl), n);

	auto it = tl.begin() + count;
	size_t i = 1;

	while (it != tl.end()) {
		auto end = it + count;

		for (; it != end; ++it)
			it->time += (dur * i);

		i++;
	}

	tl.duration *= n;
	return tl;
}


// Parser
inline Literal literal(Context& ctx, Lexer& lx, View lit_v) {
	CANE_LOG(LOG_INFO);

	lx.expect(is_lit, lx.peek().view, STR_LITERAL);
	auto [view, kind] = lx.next();

	Literal n = 0;

	switch (kind) {
		case Symbols::INT: { n = b10_decode(view); } break;
		case Symbols::HEX: { n = b16_decode(view); } break;
		case Symbols::BIN: { n = b2_decode(view);  } break;
		default: break;
	}

	return n;
}

inline Sequence sequence(Context& ctx, Lexer& lx, View expr_v) {
	CANE_LOG(LOG_INFO);

	lx.expect(is_step, lx.peek().view, STR_STEP);
	Sequence seq {};

	while (is_step(lx.peek().kind))
		seq.emplace_back(sym2step(lx.next().kind));

	return seq;
}

inline Sequence euclide(Context& ctx, Lexer& lx, View expr_v) {
	CANE_LOG(LOG_INFO);

	Literal steps = 0;
	Literal beats = lit_expr(ctx, lx, lx.peek().view, 0);

	lx.expect(equal(Symbols::SEP), lx.peek().view, STR_EXPECT, sym2str(Symbols::SEP));
	lx.next();  // skip `:`

	steps = lit_expr(ctx, lx, lx.peek().view, 0);

	if (beats > steps)
		lx.error(Phases::SEMANTIC, encompass(expr_v, lx.prev().view), STR_LESSER_EQ, steps);

	Sequence seq {};

	for (size_t i = 0; i != static_cast<size_t>(steps); ++i)
		seq.emplace_back(((i * beats) % steps) < static_cast<size_t>(beats));

	if (seq.empty())
		lx.error(Phases::SEMANTIC, encompass(expr_v, lx.prev().view), STR_EMPTY);

	return seq;
}

inline Sequence seq_ref(Context& ctx, Lexer& lx, View expr_v) {
	CANE_LOG(LOG_INFO);

	lx.expect(equal(Symbols::IDENT), lx.peek().view, STR_IDENT);
	auto [view, kind] = lx.next();

	// Lookup symbol
	if (auto it = ctx.chains.find(view); it != ctx.chains.end())
		return it->second;

	lx.error(Phases::SEMANTIC, encompass(expr_v, view), STR_UNDEFINED, view);
}

inline Literal lit_ref(Context& ctx, Lexer& lx, View lit_v) {
	CANE_LOG(LOG_INFO);

	lx.expect(equal(Symbols::IDENT), lx.peek().view, STR_IDENT);
	auto [view, kind] = lx.next();

	// Lookup constant
	if (auto it = ctx.constants.find(view); it != ctx.constants.end())
		return it->second;

	lx.error(Phases::SEMANTIC, view, STR_UNDEFINED, view);
}

inline Timeline chan_ref(Context& ctx, Lexer& lx, View chan_v) {
	CANE_LOG(LOG_INFO);

	lx.expect(equal(Symbols::IDENT), lx.peek().view, STR_IDENT);
	auto [view, kind] = lx.next();

	// Lookup constant
	if (auto it = ctx.definitions.find(view); it != ctx.definitions.end())
		return it->second;

	lx.error(Phases::SEMANTIC, view, STR_UNDEFINED, view);
}

// Literals
inline Literal lit_prefix(Context& ctx, Lexer& lx, View lit_v, size_t bp) {
	CANE_LOG(LOG_INFO);

	Literal lit {};
	Token tok = lx.peek();

	CANE_LOG(LOG_INFO, sym2str(tok.kind));

	if (is_lit_prefix(tok.kind)) {
		auto [lbp, rbp] = binding_power(lx, tok, OpFix::LIT_PREFIX);
		lx.next();  // skip operator

		switch (tok.kind) {
			case Symbols::ADD: { lit = std::abs(lit); } break;
			case Symbols::SUB: { lit = -lit;          } break;

			case Symbols::LEN_OF: {
				lit = seq_expr(ctx, lx, tok.view, 0).size();
			} break;

			default: {
				lx.error(Phases::SYNTACTIC, tok.view, STR_LIT_OPERATOR);
			} break;
		}
	}

	else if (is_lit(tok.kind))
		lit = literal(ctx, lx, lx.peek().view);

	else if (tok.kind == Symbols::IDENT)
		lit = lit_ref(ctx, lx, lx.peek().view);

	else if (tok.kind == Symbols::BPM_GLOBAL) {
		lx.next();  // skip `bpm`
		lit = ctx.bpm_global;
	}

	else if (tok.kind == Symbols::LPAREN) {
		lx.next();  // skip `(`

		lit = lit_expr(ctx, lx, lit_v, 0);  // Reset binding power.

		lx.expect(equal(Symbols::RPAREN), lx.peek().view, STR_EXPECT, sym2str(Symbols::RPAREN));
		lx.next();  // skip `)`
	}

	else
		lx.error(Phases::SYNTACTIC, tok.view, STR_LIT_PRIMARY);

	return lit;
}

inline Literal lit_infix(Context& ctx, Lexer& lx, View lit_v, Literal lit, size_t bp) {
	CANE_LOG(LOG_INFO);

	Token tok = lx.next();
	CANE_LOG(LOG_INFO, sym2str(tok.kind));

	switch (tok.kind) {
		case Symbols::ADD: { lit = lit + lit_expr(ctx, lx, lit_v, bp); } break;
		case Symbols::SUB: { lit = lit - lit_expr(ctx, lx, lit_v, bp); } break;
		case Symbols::MUL: { lit = lit * lit_expr(ctx, lx, lit_v, bp); } break;
		case Symbols::DIV: { lit = lit / lit_expr(ctx, lx, lit_v, bp); } break;
		case Symbols::MOD: { lit = lit % lit_expr(ctx, lx, lit_v, bp); } break;

		default: {
			lx.error(Phases::SYNTACTIC, tok.view, STR_LIT_OPERATOR);
		} break;
	}

	return lit;
}

inline Literal lit_expr(Context& ctx, Lexer& lx, View lit_v, size_t bp) {
	CANE_LOG(LOG_WARN);

	Literal lit = lit_prefix(ctx, lx, lit_v, 0);
	Token tok = lx.peek();

	while (is_lit_infix(tok.kind)) {
		CANE_LOG(LOG_INFO, sym2str(tok.kind));
		auto [lbp, rbp] = binding_power(lx, tok, OpFix::LIT_INFIX);

		if (lbp < bp)
			break;

		lit = lit_infix(ctx, lx, lit_v, lit, rbp);
		tok = lx.peek();
	}

	return lit;
}

// Sequences
inline Sequence seq_prefix(Context& ctx, Lexer& lx, View expr_v, size_t bp) {
	CANE_LOG(LOG_INFO);

	Sequence seq {};
	Token tok = lx.peek();
	CANE_LOG(LOG_INFO, sym2str(tok.kind));

	// Prefix operators.
	if (is_seq_prefix(tok.kind)) {
		auto [lbp, rbp] = binding_power(lx, tok, OpFix::SEQ_PREFIX);
		lx.next();  // skip operator

		switch (tok.kind) {
			case Symbols::REV:    { seq = reverse(seq_expr(ctx, lx, expr_v, rbp)); } break;
			case Symbols::INVERT: { seq = invert(seq_expr(ctx, lx, expr_v, rbp)); } break;

			default: {
				lx.error(Phases::SYNTACTIC, tok.view, STR_SEQ_OPERATOR);
			} break;
		}
	}

	// Primary expressions.
	else switch (tok.kind) {
		case Symbols::INT:
		case Symbols::HEX:
		case Symbols::BIN: {
			seq = euclide(ctx, lx, lx.peek().view);
		} break;

		case Symbols::REF: {
			lx.next();  // skip `$`
			seq = seq_ref(ctx, lx, tok.view);
		} break;

		case Symbols::SEP: {
			lx.next();  // skip `:`
			seq = euclide(ctx, lx, lx.peek().view);
		} break;

		case Symbols::SKIP:
		case Symbols::BEAT: {
			seq = sequence(ctx, lx, lx.peek().view);
		} break;

		case Symbols::LPAREN: {
			lx.next();  // skip `(`

			seq = seq_expr(ctx, lx, expr_v, 0);  // Reset binding power.

			lx.expect(equal(Symbols::RPAREN), lx.peek().view, STR_EXPECT, sym2str(Symbols::RPAREN));
			lx.next();  // skip `)`
		} break;

		default: {
			lx.error(Phases::SYNTACTIC, tok.view, STR_SEQ_PRIMARY);
		} break;
	}

	return seq;
}

inline Sequence seq_infix_expr(Context& ctx, Lexer& lx, View expr_v, Sequence lhs, size_t bp) {
	CANE_LOG(LOG_INFO);

	Token tok = lx.next();  // skip operator.
	CANE_LOG(LOG_INFO, sym2str(tok.kind));

	switch (tok.kind) {
		case Symbols::CAT: { lhs = cat(std::move(lhs), seq_expr(ctx, lx, expr_v, bp)); } break;
		case Symbols::OR:  { lhs = disjunction(std::move(lhs), seq_expr(ctx, lx, expr_v, bp)); } break;
		case Symbols::AND: { lhs = conjunction(std::move(lhs), seq_expr(ctx, lx, expr_v, bp)); } break;
		case Symbols::XOR: { lhs = ex_disjunction(std::move(lhs), seq_expr(ctx, lx, expr_v, bp)); } break;

		default: {
			lx.error(Phases::SYNTACTIC, tok.view, STR_SEQ_OPERATOR);
		} break;
	}

	return lhs;
}

inline Sequence seq_infix_lit(Context& ctx, Lexer& lx, View expr_v, Sequence seq, size_t bp) {
	CANE_LOG(LOG_INFO);

	Token tok = lx.next();  // skip operator.
	CANE_LOG(LOG_INFO, sym2str(tok.kind));

	switch (tok.kind) {
		// Infix with literal
		case Symbols::ROTLN: { seq = rotl(std::move(seq), lit_expr(ctx, lx, tok.view, 0)); } break;
		case Symbols::ROTRN: { seq = rotr(std::move(seq), lit_expr(ctx, lx, tok.view, 0)); } break;

		case Symbols::REPN: {
			View before_v = lx.peek().view;
			Literal n = lit_expr(ctx, lx, before_v, 0);

			// We don't want to shrink the sequence, it can only grow.
			if (n == 0)
				lx.error(Phases::SEMANTIC, encompass(before_v, lx.prev().view), STR_GREATER, 0);

			seq = repeat(std::move(seq), n);
		} break;

		case Symbols::CHAIN: {
			lx.expect(equal(Symbols::IDENT), lx.peek().view, STR_IDENT);
			auto [view, kind] = lx.next();  // get identifier

			// Assign or warn if re-assigned.
			if (auto [it, succ] = ctx.symbols.emplace(view); not succ)
				lx.error(Phases::SEMANTIC, view, STR_CONFLICT, view);

			if (auto [it, succ] = ctx.chains.try_emplace(view, seq); not succ)
				lx.error(Phases::SEMANTIC, view, STR_REDEFINED, view);
		} break;

		default: {
			lx.error(Phases::SYNTACTIC, tok.view, STR_SEQ_OPERATOR);
		} break;
	}

	return seq;
}

inline Sequence seq_postfix(Context& ctx, Lexer& lx, View expr_v, Sequence seq, size_t bp) {
	CANE_LOG(LOG_INFO);

	Token tok = lx.next();  // skip operator.

	switch (tok.kind) {
		case Symbols::CAR: {
			CANE_LOG(LOG_INFO, sym2str(Symbols::CAR));

			auto it = seq.begin();

			if (it == seq.end())
				lx.error(Phases::INTERNAL, tok.view, STR_UNREACHABLE);

			seq.insert(seq.begin(), *it);
			seq.erase(seq.begin() + 1, seq.end());
		} break;

		case Symbols::CDR: {
			CANE_LOG(LOG_INFO, sym2str(Symbols::CDR));

			if (seq.empty())
				lx.error(Phases::INTERNAL, tok.view, STR_UNREACHABLE);

			if (seq.size() > 1)
				seq.erase(seq.begin());
		} break;


		case Symbols::DBG: {
			CANE_LOG(LOG_INFO, sym2str(Symbols::DBG));

			auto mini = minify(seq);
			size_t count = seq.size() / mini.size();

			lx.notice(Phases::SEMANTIC, encompass(expr_v, tok.view), STR_DEBUG, mini, count, seq.size());
		} break;

		default: {
			lx.error(Phases::SYNTACTIC, tok.view, STR_SEQ_OPERATOR);
		} break;
	}

	return seq;
}

inline Sequence seq_expr(Context& ctx, Lexer& lx, View expr_v, size_t bp) {
	CANE_LOG(LOG_WARN);

	Sequence seq = seq_prefix(ctx, lx, expr_v, 0);
	Token tok = lx.peek();

	// Continue while we have a non-prefix operator.
	while (
		is_seq_infix(tok.kind) or
		is_seq_postfix(tok.kind)
	) {
		CANE_LOG(LOG_INFO, sym2str(tok.kind));

		// Handle postfix operators
		if (is_seq_postfix(tok.kind)) {
			auto [lbp, rbp] = binding_power(lx, tok, OpFix::SEQ_POSTFIX);

			if (lbp < bp)
				break;

			seq = seq_postfix(ctx, lx, expr_v, std::move(seq), 0);
		}

		// Handle infix operators
		else if (is_seq_infix(tok.kind)) {
			auto [lbp, rbp] = binding_power(lx, tok, OpFix::SEQ_INFIX);

			if (lbp < bp)
				break;

			if (is_seq_infix_lit(tok.kind))
				seq = seq_infix_lit(ctx, lx, expr_v, std::move(seq), rbp);

			else if (is_seq_infix_expr(tok.kind))
				seq = seq_infix_expr(ctx, lx, expr_v, std::move(seq), rbp);

			else
				lx.error(Phases::SYNTACTIC, tok.view, STR_SEQ_OPERATOR);
		}

		// Error if not operator
		else
			lx.error(Phases::SYNTACTIC, tok.view, STR_SEQ_OPERATOR);

		tok = lx.peek();
	}

	return seq;
}

inline Channel channel(Context& ctx, Lexer& lx) {
	CANE_LOG(LOG_INFO);

	Channel chan = CHANNEL_MIN;
	Token tok = lx.peek();

	// Sink can be either a literal number or an alias defined previously.
	if (is_lit(tok.kind))
		chan = literal(ctx, lx, lx.peek().view);

	else if (lx.peek().kind == Symbols::IDENT) {
		lx.next();  // skip identifier

		auto it = ctx.channels.find(tok.view);

		if (it == ctx.channels.end())
			lx.error(Phases::SEMANTIC, tok.view, STR_UNDEFINED, tok.view);

		chan = it->second;
	}

	else
		lx.error(Phases::SYNTACTIC, tok.view, STR_IDENT_LITERAL);

	if (chan > CHANNEL_MAX or chan < CHANNEL_MIN)
		lx.error(Phases::SEMANTIC, tok.view, STR_BETWEEN, CHANNEL_MIN, CHANNEL_MAX);

	return chan - 1;
}

inline Timeline chan_prefix(Context& ctx, Lexer& lx, View chan_v, size_t bp) {
	CANE_LOG(LOG_INFO);

	Timeline tl {};
	Token tok = lx.peek();
	CANE_LOG(LOG_INFO, sym2str(tok.kind));

	if (is_seq_expr(tok.kind)) {
		Sequence seq = seq_expr(ctx, lx, tok.view, 0);

		lx.expect(equal(Symbols::SINK), lx.peek().view, STR_EXPECT, sym2str(Symbols::SINK));
		lx.next();  // skip `~>`

		Channel chan = channel(ctx, lx);

		lx.expect(equal(Symbols::BPM), lx.peek().view, STR_EXPECT, sym2str(Symbols::BPM));
		lx.next();  // skip `@`

		// BPM
		Literal bpm = lit_expr(ctx, lx, lx.peek().view, 0);

		// Note mapping
		lx.expect(equal(Symbols::LBRACKET), lx.peek().view, STR_EXPECT, sym2str(Symbols::LBRACKET));
		lx.next();  // skip `[`

		lx.expect(is_lit_primary, lx.peek().view, STR_LITERAL);

		std::vector<Literal> notes;
		std::vector<Literal> velocities;

		while (lx.peek().kind != Symbols::RBRACKET) {
			Literal note = lit_expr(ctx, lx, lx.peek().view, 0);
			Literal velocity = VELOCITY_DEFAULT;

			if (lx.peek().kind == Symbols::SEP) {
				lx.next();  // skip `:`
				velocity = lit_expr(ctx, lx, lx.peek().view, 0);
			}

			notes.emplace_back(note);
			velocities.emplace_back(velocity);
		}

		lx.expect(equal(Symbols::RBRACKET), lx.peek().view, STR_EXPECT, sym2str(Symbols::RBRACKET));
		lx.next();  // skip `]`

		// Compile sequence to timeline.
		auto time_per_note = ONE_MIN / bpm;
		auto time = Unit::zero();

		for (size_t i = 0; i != seq.size(); ++i) {
			if (seq[i]) {  // Note on
				size_t index = i % notes.size();

				tl.emplace_back(time, midi2int(Midi::NOTE_ON) | chan, notes[index], velocities[index]);
				tl.emplace_back(time + time_per_note, midi2int(Midi::NOTE_OFF) | chan, notes[index], velocities[index]);
			}

			time += time_per_note;
		}

		tl.duration = time;
	}

	else if (tok.kind == Symbols::IDENT) {
		tl = chan_ref(ctx, lx, lx.peek().view);
	}

	else if (tok.kind == Symbols::LBRACE) {
		lx.next();  // skip `{`

		tl = chan_expr(ctx, lx, chan_v, 0);  // Reset binding power.

		lx.expect(equal(Symbols::RBRACE), lx.peek().view, STR_EXPECT, sym2str(Symbols::RBRACE));
		lx.next();  // skip `}`
	}

	else
		lx.error(Phases::SYNTACTIC, lx.peek().view, STR_CHAN_EXPR);

	return tl;
}

inline Timeline chan_infix_expr(Context& ctx, Lexer& lx, View chan_v, Timeline tl, size_t bp) {
	CANE_LOG(LOG_INFO);

	Token tok = lx.next();
	CANE_LOG(LOG_INFO, sym2str(tok.kind));

	switch (tok.kind) {
		case Symbols::WITH: {
			Timeline rhs = chan_expr(ctx, lx, chan_v, bp);
			tl.duration = std::max(tl.duration, rhs.duration);
			tl.insert(tl.end(), rhs.begin(), rhs.end());
		} break;

		case Symbols::JOIN: {
			Timeline rhs = chan_expr(ctx, lx, chan_v, bp);

			for (Event& ev: rhs)
				ev.time += tl.duration;

			tl.duration += rhs.duration;
			tl.insert(tl.end(), rhs.begin(), rhs.end());
		} break;

		default: {
			lx.error(Phases::SYNTACTIC, tok.view, STR_CHAN_OPERATOR);
		} break;
	}

	return tl;
}

inline Timeline chan_infix_lit(Context& ctx, Lexer& lx, View chan_v, Timeline tl, size_t bp) {
	CANE_LOG(LOG_INFO);

	Token tok = lx.next();
	CANE_LOG(LOG_INFO, sym2str(tok.kind));

	switch (tok.kind) {
		case Symbols::LOOP: {
			tl = chan_repeat(tl, lit_expr(ctx, lx, lx.peek().view, 0));
		} break;

		default: {
			lx.error(Phases::SYNTACTIC, tok.view, STR_CHAN_OPERATOR);
		} break;
	}

	return tl;
}

inline Timeline chan_expr(Context& ctx, Lexer& lx, View chan_v, size_t bp) {
	CANE_LOG(LOG_INFO);

	Timeline tl = chan_prefix(ctx, lx, chan_v, 0);
	Token tok = lx.peek();

	while (is_chan_infix(tok.kind)) {
		CANE_LOG(LOG_INFO, sym2str(tok.kind));

		auto [lbp, rbp] = binding_power(lx, tok, OpFix::CHAN_INFIX);

		if (lbp < bp)
			break;

		if (is_chan_infix_lit(tok.kind))
			tl = chan_infix_lit(ctx, lx, chan_v, std::move(tl), rbp);

		else if (is_chan_infix_expr(tok.kind))
			tl = chan_infix_expr(ctx, lx, chan_v, std::move(tl), rbp);

		else
			lx.error(Phases::SYNTACTIC, tok.view, STR_CHAN_OPERATOR);

		tok = lx.peek();
	}

	return tl;
}

inline void statement(Context& ctx, Lexer& lx, View stat_v) {
	CANE_LOG(LOG_WARN);

	Token tok = lx.peek();

	// Alias a sink.
	if (tok.kind == Symbols::ALIAS) {
		CANE_LOG(LOG_INFO, sym2str(Symbols::ALIAS));
		lx.next();  // skip `alias`

		lx.expect(equal(Symbols::IDENT), lx.peek().view, STR_IDENT);
		auto [view, kind] = lx.next();  // get identifier

		Channel chan = literal(ctx, lx, lx.peek().view);

		if (chan > CHANNEL_MAX or chan < CHANNEL_MIN)
			lx.error(Phases::SEMANTIC, lx.prev().view, STR_BETWEEN, CHANNEL_MIN, CHANNEL_MAX);

		// Assign or warn if re-assigned.
		if (auto [it, succ] = ctx.symbols.emplace(view); not succ)
			lx.error(Phases::SEMANTIC, view, STR_CONFLICT, view);

		if (auto [it, succ] = ctx.channels.try_emplace(view, chan); not succ)
			lx.error(Phases::SEMANTIC, view, STR_REDEFINED, view);
	}

	else if (tok.kind == Symbols::LET) {
		CANE_LOG(LOG_INFO, sym2str(Symbols::LET));
		lx.next();  // skip `let`

		lx.expect(equal(Symbols::IDENT), lx.peek().view, STR_IDENT);
		auto [view, kind] = lx.next();  // get identifier

		Literal lit = lit_expr(ctx, lx, lx.peek().view, 0);

		// Assign or warn if re-assigned.
		if (auto [it, succ] = ctx.symbols.emplace(view); not succ)
			lx.error(Phases::SEMANTIC, view, STR_CONFLICT, view);

		if (auto [it, succ] = ctx.constants.try_emplace(view, lit); not succ)
			lx.error(Phases::SEMANTIC, view, STR_REDEFINED, view);
	}

	else if (tok.kind == Symbols::DEF) {
		CANE_LOG(LOG_INFO, sym2str(Symbols::DEF));
		lx.next();  // skip `def`

		lx.expect(equal(Symbols::IDENT), lx.peek().view, STR_IDENT);
		auto [view, kind] = lx.next();  // get identifier

		Timeline tl = chan_expr(ctx, lx, lx.peek().view, 0);

		// Assign or warn if re-assigned.
		if (auto [it, succ] = ctx.symbols.emplace(view); not succ)
			lx.error(Phases::SEMANTIC, view, STR_CONFLICT, view);

		if (auto [it, succ] = ctx.definitions.try_emplace(view, tl); not succ)
			lx.error(Phases::SEMANTIC, view, STR_REDEFINED, view);
	}

	else if (is_chan_primary(tok.kind)) {
		Timeline tl = chan_expr(ctx, lx, lx.peek().view, 0);

		Unit time = Unit::zero();

		for (Event& ev: tl) {
			ev.time += ctx.base_time;
			time = ev.time;
		}

		ctx.timeline.insert(ctx.timeline.begin(), tl.begin(), tl.end());

		ctx.base_time += tl.duration;
		ctx.timeline.duration += tl.duration;
	}

	else
		lx.error(Phases::SYNTACTIC, tok.view, STR_STATEMENT);

	// Chains are scoped to channel expressions.
	// We technically don't need to run this after every
	// statement but it simplifies things by not having
	// to duplicate the code for primary expressions and
	// definitions.
	for (auto& [k, v]: ctx.chains) {
		if (auto it = ctx.symbols.find(k); it != ctx.symbols.end())
			ctx.symbols.erase(it);
	}

	ctx.chains.clear();
}

inline Timeline compile(Lexer& lx, size_t bpm_global, size_t note_global) {
	CANE_LOG(LOG_WARN);

	Context ctx;
	ctx.bpm_global = bpm_global;
	ctx.note_global = note_global;

	// Compile
	while (lx.peek().kind != Symbols::TERMINATOR)
		statement(ctx, lx, lx.peek().view);

	Timeline tl = std::move(ctx.timeline);

	// Active sensing
	Unit t = Unit::zero();
	while (t < ctx.base_time) {
		tl.emplace_back(t, midi2int(Midi::ACTIVE_SENSE), 0, 0);
		t += ACTIVE_SENSING_INTERVAL;
	}

	// MIDI clock pulse
	// We fire off a MIDI tick 24 times
	// for every quarter note
	Unit clock_freq = std::chrono::duration_cast<cane::Unit>(std::chrono::minutes { 1 }) / (bpm_global * 24);
	t = Unit::zero();
	while (t < ctx.base_time) {
		tl.emplace_back(t, midi2int(Midi::TIMING_CLOCK), 0, 0);
		t += clock_freq;
	}

	// Sort sequence by timestamps
	std::stable_sort(tl.begin(), tl.end(), [] (auto& a, auto& b) {
		return a.time < b.time;
	});

	// Start/Stop
	tl.emplace(tl.begin(), Unit::zero(), midi2int(Midi::START), 0, 0);
	tl.emplace(tl.end(), ctx.base_time, midi2int(Midi::STOP), 0, 0);

	// Reset state of MIDI devices
	for (size_t i = CHANNEL_MIN; i != CHANNEL_MAX; ++i) {
		tl.emplace(tl.begin(), Unit::zero(), midi2int(Midi::CHANNEL_MODE), ALL_SOUND_OFF, 0);
		tl.emplace(tl.begin(), Unit::zero(), midi2int(Midi::CHANNEL_MODE), ALL_NOTES_OFF, 0);
		tl.emplace(tl.begin(), Unit::zero(), midi2int(Midi::CHANNEL_MODE), ALL_RESET_CC, 0);
	}

	return tl;
}

}

#endif
