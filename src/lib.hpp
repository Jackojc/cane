#ifndef CANE_LIB_HPP
#define CANE_LIB_HPP

#include <iostream>
#include <vector>
#include <array>
#include <unordered_map>
#include <algorithm>
#include <numeric>

#include <locale.hpp>
#include <print.hpp>
#include <view.hpp>
#include <unicode.hpp>
#include <log.hpp>
#include <report.hpp>

namespace cane {

// Constants
constexpr size_t CHANNEL_MIN = 1u;
constexpr size_t CHANNEL_MAX = 16u;
constexpr size_t BPM_MIN     = 0u;
constexpr size_t DEFAULT_BPM = 120u;


// Errors/Warnings/Notices
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
	X(IDENT, "ident") \
	X(INT,   "int") \
	X(HEX,   "hex") \
	X(BIN,   "bin") \
	\
	/* Sequence Keywords */ \
	X(CLEAR, "clear") \
	X(WAIT,  "wait") \
	X(ALIAS, "alias") \
	X(SYNC,  "sync") \
	X(FIT,   "fit") \
	\
	/* Grouping */ \
	X(LPAREN, "(") \
	X(RPAREN, ")") \
	\
	/* Literal Operators */ \
	X(ADD, "+") \
	X(SUB, "-") \
	X(MUL, "*") \
	X(DIV, "/") \
	X(MOD, "%") \
	\
	/* Literal Keywords */ \
	X(LEN, "len") \
	X(LET, "let") \
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
	\
	/* Postfix Sequence Operators */ \
	X(ROTL, "<") \
	X(ROTR, ">") \
	X(DBG,  "?")

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

	inline Token next() {
		Token tok {};

		auto& [sbegin, send] = src;
		auto& [view, kind] = tok;
		auto& [begin, end] = view;

		uint32_t c = cane::as_char(src);

		// Skip whitespace.
		cane::consume_char(src, c, cane::is_whitespace);
		view = cane::as_view(src); // Set view to first character.

		if (src.is_eof()) {
			kind = Symbols::TERMINATOR;
			view = View {};  // set to eof
		}

		else if (c == '#') {
			cane::consume_char(src, c, not_equal((uint32_t)'\n'));  // Skip until \n and then return next token.
			return next();
		}

		else if (c == '(') { kind = Symbols::LPAREN; src = cane::iter_next_char(src, c); }
		else if (c == ')') { kind = Symbols::RPAREN; src = cane::iter_next_char(src, c); }

		else if (c == '!') { kind = Symbols::BEAT; src = cane::iter_next_char(src, c); }
		else if (c == '.') { kind = Symbols::SKIP; src = cane::iter_next_char(src, c); }
		else if (c == ':') { kind = Symbols::SEP;  src = cane::iter_next_char(src, c); }

		else if (c == '?')  { kind = Symbols::DBG;  src = cane::iter_next_char(src, c); }
		else if (c == '\'') { kind = Symbols::REV;  src = cane::iter_next_char(src, c); }
		else if (c == '@')  { kind = Symbols::BPM;  src = cane::iter_next_char(src, c); }
		else if (c == '|')  { kind = Symbols::OR;   src = cane::iter_next_char(src, c); }
		else if (c == '&')  { kind = Symbols::AND;  src = cane::iter_next_char(src, c); }
		else if (c == '^')  { kind = Symbols::XOR;  src = cane::iter_next_char(src, c); }
		else if (c == ',')  { kind = Symbols::CAT;  src = cane::iter_next_char(src, c); }
		else if (c == '#')  { kind = Symbols::REPN; src = cane::iter_next_char(src, c); }

		else if (c == '+') { kind = Symbols::ADD; src = cane::iter_next_char(src, c); }
		else if (c == '-') { kind = Symbols::SUB; src = cane::iter_next_char(src, c); }
		else if (c == '/') { kind = Symbols::DIV; src = cane::iter_next_char(src, c); }

		else if (c == '*') {
			kind = Symbols::MUL;
			src = cane::iter_next_char(src, c);

			if (as_char(src) == '*') {
				kind = Symbols::REPN;
				view = overlap(view, as_view(src));
				src = cane::iter_next_char(src, c);
			}
		}

		else if (c == '<') {
			kind = Symbols::ROTL;
			src = cane::iter_next_char(src, c);

			if (as_char(src) == '<') {
				kind = Symbols::ROTLN;
				view = overlap(view, as_view(src));
				src = cane::iter_next_char(src, c);
			}
		}

		else if (c == '>') {
			kind = Symbols::ROTR;
			src = cane::iter_next_char(src, c);

			if (as_char(src) == '>') {
				kind = Symbols::ROTRN;
				view = overlap(view, as_view(src));
				src = cane::iter_next_char(src, c);
			}
		}

		else if (c == '~') {
			kind = Symbols::INVERT;
			src = cane::iter_next_char(src, c);

			if (as_char(src) == '>') {
				kind = Symbols::SINK;
				view = overlap(view, as_view(src));
				src = cane::iter_next_char(src, c);
			}
		}

		else if (c == '=') {
			src = cane::iter_next_char(src, c);

			if (as_char(src) == '>') {
				kind = Symbols::CHAIN;
				view = overlap(view, as_view(src));
				src = cane::iter_next_char(src, c);
			}
		}

		else if (cane::is_number(c)) {
			const auto lbegin = begin;  // Save starting position of token so we include "0x" or "0b"

			if (c == '0') {
				src = cane::iter_next_char(src, c);

				// Hex literal
				if (as_char(src) == 'x') {
					kind = Symbols::HEX;
					src = cane::iter_next_char(src, c);
					view = cane::consume_char(src, c, [&] (uint32_t c) {
						return
							(c >= 'A' and c <= 'Z') or
							(c >= 'a' and c <= 'z') or
							(c >= '0' and c <= '9')
						;
					});
				}

				// Binary literal
				else if (as_char(src) == 'b') {
					kind = Symbols::BIN;
					src = cane::iter_next_char(src, c);
					view = cane::consume_char(src, c, [&] (uint32_t c) {
						return c == '0' or c == '1';
					});
				}

			}

			kind = Symbols::INT;
			view = cane::consume_char(src, c, cane::is_number);

			begin = lbegin;  // Make sure to set the begin pointer here so that we handle the lone 0 case aswell.
		}

		else if (cane::is_visible(c)) {
			kind = Symbols::IDENT;
			view = cane::consume_char(src, c, cane::is_visible);

			if (view == "sync"_sv)
				kind = Symbols::SYNC;

			else if (view == "clear"_sv)
				kind = Symbols::CLEAR;

			else if (view == "wait"_sv)
				kind = Symbols::WAIT;

			else if (view == "alias"_sv)
				kind = Symbols::ALIAS;

			else if (view == "fit"_sv)
				kind = Symbols::FIT;

			else if (view == "len"_sv)
				kind = Symbols::LEN;

			else if (view == "let"_sv)
				kind = Symbols::LET;
		}

		// If the kind is still NONE by this point, we can assume we didn't find
		// a valid // token. The reason this check is disconnected from the above
		// if-else chain is because some checks are nested and only fail after
		// succeeding with the original check so we wouldn't fall through if this
		// check was connected.
		if (kind == Symbols::NONE)
			report_error(Phases::LEXICAL, original, view, STR_UNKNOWN_CHAR, view);

		Token out = peek_;
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
	sv = cane::next_char(sv, 2);  // skip `0x`

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
	sv = cane::next_char(sv, 2);  // skip `0b`

	for (auto ptr = sv.begin; ptr != sv.end; ++ptr)
		n = (n * 2) + (*ptr - '0');

	return n;
}


// Context
using Step = uint8_t;

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

using Channel = uint8_t;

struct Sequence: public std::vector<Step> {
	size_t bpm;

	Sequence(size_t bpm_):
		std::vector<Step>::vector {}, bpm(bpm_) {}
};

inline std::ostream& operator<<(std::ostream& os, Sequence& s) {
	for (Step x: s)
		out(os, step2colour(x), step2str(x));

	return out(os, CANE_ANSI_RESET);
}

using Literal = int64_t;


#define MIDI \
	X(MIDI_NOTE_OFF,         0b1000) \
	X(MIDI_NOTE_ON,          0b1001) \
	X(MIDI_KEY_PRESSURE,     0b1010) \
	X(MIDI_CONTROL_CHANGE,   0b1011) \
	X(MIDI_CHANNEL_PRESSURE, 0b1101) \
	X(MIDI_PITCH_BEND,       0b1110)

	#define X(name, value) name = value,
		enum { MIDI };
	#undef X

	#define X(name, value) value,
		constexpr uint8_t MIDI_TO_INT[] = { MIDI };
	#undef X

	constexpr decltype(auto) midi2int(uint8_t m) {
		return MIDI_TO_INT[m];
	}

	#define X(name, value) x == value ? #name:
		constexpr decltype(auto) int2midi(uint8_t x) {
			return MIDI 0;
		}
	#undef X

#undef MIDI

struct Event {
	size_t time;
	uint8_t status;
	uint8_t note;
	uint8_t velocity;

	constexpr Event(size_t time_, uint8_t kind, uint8_t channel, uint8_t note_, uint8_t velocity_):
		time(time_), status((kind << 4) | channel), note(note_), velocity(velocity_) {}

	constexpr decltype(auto) message() const {
		return std::array { status, note, velocity };
	}
};


struct Context {
	std::unordered_map<View, Sequence> symbols;
	std::unordered_map<View, Literal> constants;
	std::unordered_map<View, size_t> aliases;
	std::unordered_map<size_t, size_t> times;

	std::vector<Event> timeline;
	size_t default_bpm = DEFAULT_BPM;
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
inline size_t literal (Context&, Lexer&);

// Sequence expressions
inline Sequence sequence  (Context&, Lexer&);
inline Sequence euclide   (Context&, Lexer&);

inline Literal lit_reference (Context&, Lexer&);
inline Literal lit_prefix     (Context&, Lexer&, size_t);
inline Literal lit_infix      (Context&, Lexer&, Literal, size_t);
inline Literal lit_expression (Context&, Lexer&, size_t);

inline Sequence seq_reference (Context&, Lexer&);
inline Sequence seq_prefix        (Context&, Lexer&, size_t);
inline Sequence seq_infix_expr    (Context&, Lexer&, Sequence, size_t);
inline Sequence seq_infix_literal (Context&, Lexer&, Sequence, size_t);
inline Sequence seq_postfix       (Context&, Lexer&, Sequence, size_t);
inline Sequence seq_expression    (Context&, Lexer&, size_t);

// Statements
inline Sequence sink      (Context&, Lexer&, Sequence);
inline void     statement (Context&, Lexer&);

inline Context compile (Lexer&);


// Predicates
constexpr auto is_literal = partial_eq_any(Symbols::INT, Symbols::HEX, Symbols::BIN);
constexpr auto is_step = partial_eq_any(Symbols::SKIP, Symbols::BEAT);

constexpr auto is_lit_prefix = partial_eq_any(Symbols::ADD, Symbols::SUB, Symbols::LEN);
constexpr auto is_lit_infix = partial_eq_any(
	Symbols::ADD,
	Symbols::SUB,
	Symbols::MUL,
	Symbols::DIV,
	Symbols::MOD
);

constexpr auto is_seq_prefix = partial_eq_any(Symbols::INVERT, Symbols::REV);
constexpr auto is_seq_postfix = partial_eq_any(Symbols::ROTL, Symbols::ROTR, Symbols::DBG);

constexpr auto is_seq_infix_expr = partial_eq_any(
	Symbols::OR,
	Symbols::AND,
	Symbols::XOR,
	Symbols::CAT,
	Symbols::SYNC,
	Symbols::FIT
);

constexpr auto is_seq_infix_literal = partial_eq_any(
	Symbols::ROTLN,
	Symbols::ROTRN,
	Symbols::REPN,
	Symbols::BPM,
	Symbols::CHAIN
);

constexpr auto is_seq_infix = [] (auto x) {
	return is_seq_infix_expr(x) or is_seq_infix_literal(x);
};

constexpr auto is_primary = [] (auto x) {
	return is_step(x) or eq_any(x,
		Symbols::INT,
		Symbols::HEX,
		Symbols::BIN,
		Symbols::IDENT,
		Symbols::LPAREN
	);
};

constexpr auto is_seq_expr = [] (auto x) {
	return is_seq_prefix(x) or is_primary(x);
};


// Operator precedence lookup
inline decltype(auto) lit_prefix_bp(Lexer& lx, Token tok) {
	// Left value is unused.
	if (eq_any(tok.kind,
		Symbols::ADD,
		Symbols::SUB
	))
		return std::pair { 0u, 201u };

	else if (eq_any(tok.kind,
		Symbols::LEN
	))
		return std::pair { 0u, 202u };

	lx.error(Phases::INTERNAL, tok.view, STR_UNREACHABLE);
}

inline decltype(auto) lit_infix_bp(Lexer& lx, Token tok) {
	if (eq_any(tok.kind,
		Symbols::ADD,
		Symbols::SUB
	))
		return std::pair { 103u, 104u };

	else if (eq_any(tok.kind,
		Symbols::MUL,
		Symbols::DIV,
		Symbols::MOD
	))
		return std::pair { 105u, 106u };

	lx.error(Phases::INTERNAL, tok.view, STR_UNREACHABLE);
}


inline decltype(auto) seq_prefix_bp(Lexer& lx, Token tok) {
	// Left value is unused.
	if (eq_any(tok.kind,
		Symbols::REV,
		Symbols::INVERT
	))
		return std::pair { 0u, 201u };

	lx.error(Phases::INTERNAL, tok.view, STR_UNREACHABLE);
}

inline decltype(auto) seq_infix_bp(Lexer& lx, Token tok) {
	if (eq_any(tok.kind,
		Symbols::CHAIN
	))
		return std::pair { 103u, 104u };

	else if (eq_any(tok.kind,
		Symbols::CAT,
		Symbols::OR,
		Symbols::AND,
		Symbols::XOR,
		Symbols::BPM,
		Symbols::ROTLN,
		Symbols::ROTRN,
		Symbols::REPN
	))
		return std::pair { 105u, 106u };

	else if (eq_any(tok.kind,
		Symbols::SYNC,
		Symbols::FIT
	))
		return std::pair { 107u, 108u };

	lx.error(Phases::INTERNAL, tok.view, STR_UNREACHABLE);
}

inline decltype(auto) seq_postfix_bp(Lexer& lx, Token tok) {
	// Right value is unused.
	if (eq_any(tok.kind,
		Symbols::DBG
	))
		return std::pair { 1u, 0u };

	else if (eq_any(tok.kind,
		Symbols::ROTL,
		Symbols::ROTR
	))
		return std::pair { 2u, 0u };

	lx.error(Phases::INTERNAL, tok.view, STR_UNREACHABLE);
}


// Parser
inline size_t literal(Context& ctx, Lexer& lx) {
	CANE_LOG(LOG_INFO);

	lx.expect(is_literal, lx.peek().view, STR_LITERAL);
	auto [view, kind] = lx.next();

	size_t n = 0;

	switch (kind) {
		case Symbols::INT: { n = b10_decode(view); } break;
		case Symbols::HEX: { n = b16_decode(view); } break;
		case Symbols::BIN: { n = b2_decode(view);  } break;
		default: break;
	}

	return n;
}

inline Sequence sequence(Context& ctx, Lexer& lx) {
	CANE_LOG(LOG_INFO);

	lx.expect(is_step, lx.peek().view, STR_STEP);
	Sequence seq { ctx.default_bpm };

	while (is_step(lx.peek().kind))
		seq.emplace_back(sym2step(lx.next().kind));

	return seq;
}

inline Sequence euclide(Context& ctx, Lexer& lx) {
	CANE_LOG(LOG_INFO);

	View lv = lx.peek().view;

	size_t steps = 0;
	size_t beats = lit_expression(ctx, lx, 0);

	lx.expect(equal(Symbols::SEP), lx.peek().view, STR_EXPECT, sym2str(Symbols::SEP));
	lx.next();  // skip `/`

	steps = lit_expression(ctx, lx, 0);

	if (beats > steps)
		lx.error(Phases::SEMANTIC, lv, STR_LESSER_EQ, steps);

	Sequence seq { ctx.default_bpm };

	for (size_t i = 0; i != steps; ++i)
		seq.emplace_back(((i * beats) % steps) < beats);

	return seq;
}

inline Sequence seq_reference(Context& ctx, Lexer& lx) {
	CANE_LOG(LOG_INFO);

	lx.expect(equal(Symbols::IDENT), lx.peek().view, STR_IDENT);
	auto [view, kind] = lx.next();

	// Lookup symbol
	if (auto it = ctx.symbols.find(view); it != ctx.symbols.end())
		return it->second;

	lx.error(Phases::SEMANTIC, view, STR_UNDEFINED, view);
}

inline Literal lit_reference(Context& ctx, Lexer& lx) {
	CANE_LOG(LOG_INFO);

	lx.expect(equal(Symbols::IDENT), lx.peek().view, STR_IDENT);
	auto [view, kind] = lx.next();

	// Lookup constant
	if (auto it = ctx.constants.find(view); it != ctx.constants.end())
		return it->second;

	lx.error(Phases::SEMANTIC, view, STR_UNDEFINED, view);
}

// Literals
inline Literal lit_prefix(Context& ctx, Lexer& lx, size_t bp) {
	CANE_LOG(LOG_INFO);

	Literal lit {};
	Token tok = lx.peek();

	if (is_lit_prefix(tok.kind)) {
		auto [lbp, rbp] = lit_prefix_bp(lx, tok);
		lx.next();  // skip operator

		switch (tok.kind) {
			case Symbols::ADD: { lit *=  1; } break;
			case Symbols::SUB: { lit *= -1; } break;

			case Symbols::LEN: {
				lit = seq_expression(ctx, lx, 0).size();
			} break;

			default: {
				lx.error(Phases::SYNTACTIC, tok.view, STR_LIT_PREFIX);
			} break;
		}
	}

	else if (is_literal(tok.kind)) {
		lit = literal(ctx, lx);
	}

	else if (tok.kind == Symbols::IDENT) {
		lit = lit_reference(ctx, lx);
	}

	else {
		lx.error(Phases::SYNTACTIC, tok.view, STR_LIT_PREFIX_LITERAL);
	}

	return lit;
}

inline Literal lit_infix(Context& ctx, Lexer& lx, Literal lit, size_t bp) {
	CANE_LOG(LOG_INFO);

	Token tok = lx.next();

	switch (tok.kind) {
		case Symbols::ADD: { lit = lit + lit_expression(ctx, lx, bp); } break;
		case Symbols::SUB: { lit = lit - lit_expression(ctx, lx, bp); } break;
		case Symbols::MUL: { lit = lit * lit_expression(ctx, lx, bp); } break;
		case Symbols::DIV: { lit = lit / lit_expression(ctx, lx, bp); } break;
		case Symbols::MOD: { lit = lit % lit_expression(ctx, lx, bp); } break;

		default: {
			lx.error(Phases::SYNTACTIC, tok.view, STR_LIT_INFIX);
		} break;
	}

	return lit;
}

inline Literal lit_expression(Context& ctx, Lexer& lx, size_t bp) {
	CANE_LOG(LOG_WARN);

	Literal lit = lit_prefix(ctx, lx, 0);
	Token tok = lx.peek();

	while (is_lit_infix(tok.kind)) {
		// Handle postfix operators
		// For future use...
		// if (is_lit_postfix(tok.kind)) {
		// 	auto [lbp, rbp] = lit_postfix_bp(lx, tok);

		// 	if (lbp < bp)
		// 		break;

		// 	lit = lit_postfix(ctx, lx, lit, 0);
		// }

		// Handle infix operators
		if (is_lit_infix(tok.kind)) {
			auto [lbp, rbp] = lit_infix_bp(lx, tok);

			if (lbp < bp)
				break;

			lit = lit_infix(ctx, lx, lit, rbp);
		}

		// Error if not operator
		else {
			lx.error(Phases::SYNTACTIC, tok.view, STR_LIT_EXPR);
		}

		tok = lx.peek();
	}

	return lit;
}

// Sequences
inline Sequence seq_prefix(Context& ctx, Lexer& lx, size_t bp) {
	CANE_LOG(LOG_INFO);

	Sequence seq { ctx.default_bpm };
	Token tok = lx.peek();

	// Prefix operators.
	if (is_seq_prefix(tok.kind)) {
		auto [lbp, rbp] = seq_prefix_bp(lx, tok);
		lx.next();  // skip operator

		switch (tok.kind) {
			case Symbols::REV:    { seq = reverse (seq_expression(ctx, lx, rbp)); } break;
			case Symbols::INVERT: { seq = invert  (seq_expression(ctx, lx, rbp)); } break;

			default: {
				lx.error(Phases::SYNTACTIC, tok.view, STR_SEQ_PREFIX);
			} break;
		}
	}

	// Primary expressions.
	else {
		switch (tok.kind) {
			case Symbols::INT:
			case Symbols::HEX:
			case Symbols::BIN:   { seq = euclide       (ctx, lx); } break;
			case Symbols::IDENT: { seq = seq_reference (ctx, lx); } break;

			case Symbols::SKIP:
			case Symbols::BEAT: { seq = sequence(ctx, lx); } break;

			case Symbols::LPAREN: {
				lx.next();  // skip `(`

				seq = seq_expression(ctx, lx, 0);  // Reset binding power.

				lx.expect(equal(Symbols::RPAREN), lx.peek().view, STR_EXPECT, sym2str(Symbols::RPAREN));
				lx.next();  // skip `)`
			} break;

			default: {
				lx.error(Phases::SYNTACTIC, tok.view, STR_SEQ_EXPR);
			} break;
		}
	}

	return seq;
}

inline Sequence seq_infix_expr(Context& ctx, Lexer& lx, Sequence lhs, size_t bp) {
	CANE_LOG(LOG_INFO);

	Token tok = lx.next();  // skip operator.

	switch (tok.kind) {
		case Symbols::CAT: { lhs = cat            (std::move(lhs), seq_expression(ctx, lx, bp)); } break;
		case Symbols::OR:  { lhs = disjunction    (std::move(lhs), seq_expression(ctx, lx, bp)); } break;
		case Symbols::AND: { lhs = conjunction    (std::move(lhs), seq_expression(ctx, lx, bp)); } break;
		case Symbols::XOR: { lhs = ex_disjunction (std::move(lhs), seq_expression(ctx, lx, bp)); } break;


		case Symbols::SYNC: {
			// When it comes to polyrhythms or polymeters, sequences will tend
			// to come in and out of alignment and re-align after a certain
			// amount of time. Here, we calculate how long it will take for
			// two sequences to re-align so we can avoid cutting either of
			// them short. The algorithm is fairly simple: we just calculate
			// the absolute time it takes for each sequence to complete and
			// find the lowest common multiple. We then divide the LCM by
			// the absolute length to find the number of repetitions for the
			// `lhs` in order to come back in phase with the `rhs`.
			Sequence rhs = seq_expression(ctx, lx, bp);

			size_t lhs_length = (1000 * 60) / lhs.bpm * lhs.size();
			size_t rhs_length = (1000 * 60) / rhs.bpm * rhs.size();

			size_t lcm = std::lcm(lhs_length, rhs_length);

			size_t lhs_reps = lcm / lhs_length;
			size_t rhs_reps = lcm / rhs_length;

			lhs = repeat(std::move(lhs), lhs_reps);
		} break;

		case Symbols::FIT: {
			// This operator instead stretches or shrinks the sequence
			// to fit within the time of the other sequence so you can
			// form polyrhythms. It is somewhat similar to SYNC but it
			// doesn't align the sequences by repetition.
			Sequence rhs = seq_expression(ctx, lx, bp);
			lhs.bpm = rhs.bpm * lhs.size() / rhs.size();
		} break;


		default: {
			lx.error(Phases::SYNTACTIC, tok.view, STR_SEQ_INFIX_EXPR);
		} break;
	}

	return lhs;
}

inline Sequence seq_infix_literal(Context& ctx, Lexer& lx, Sequence seq, size_t bp) {
	CANE_LOG(LOG_INFO);

	Token tok = lx.next();  // skip operator.

	switch (tok.kind) {
		// Infix with literal
		case Symbols::ROTLN: { seq = rotl(std::move(seq), lit_expression(ctx, lx, 0)); } break;
		case Symbols::ROTRN: { seq = rotr(std::move(seq), lit_expression(ctx, lx, 0)); } break;


		case Symbols::REPN: {
			View lv = lx.peek().view;
			size_t n = lit_expression(ctx, lx, 0);

			// We don't want to shrink the sequence, it can only grow.
			if (n == 0)
				lx.error(Phases::SEMANTIC, lv, STR_GREATER, 0);

			seq = repeat(std::move(seq), n);
		} break;


		case Symbols::BPM: {
			View bpm_v = lx.peek().view;
			size_t bpm = lit_expression(ctx, lx, 0);

			if (bpm == BPM_MIN)
				lx.error(Phases::SEMANTIC, bpm_v, STR_GREATER, BPM_MIN);

			seq.bpm = bpm;
		} break;


		case Symbols::CHAIN: {
			lx.expect(equal(Symbols::IDENT), lx.peek().view, STR_IDENT);
			auto [view, kind] = lx.next();  // get identifier

			// Assign or warn if re-assigned.
			if (auto [it, succ] = ctx.symbols.try_emplace(view, seq); not succ) {
				lx.warning(Phases::SEMANTIC, view, STR_REDEFINED, view);
				it->second = seq;
			}
		} break;


		default: {
			lx.error(Phases::SYNTACTIC, tok.view, STR_SEQ_INFIX_LITERAL);
		} break;
	}

	return seq;
}

inline Sequence seq_postfix(Context& ctx, Lexer& lx, Sequence seq, size_t bp) {
	CANE_LOG(LOG_INFO);

	Token tok = lx.next();  // skip operator.

	switch (tok.kind) {
		case Symbols::ROTL:   { seq = rotl    (std::move(seq)); } break;
		case Symbols::ROTR:   { seq = rotr    (std::move(seq)); } break;

		case Symbols::DBG: {
			lx.notice(Phases::SEMANTIC, tok.view, STR_DEBUG, seq, seq.bpm, ((60 * 1000) * seq.size() / seq.bpm) / 1000.f);
		} break;

		default: {
			lx.error(Phases::SYNTACTIC, tok.view, STR_SEQ_POSTFIX);
		} break;
	}

	return seq;
}

inline Sequence seq_expression(Context& ctx, Lexer& lx, size_t bp) {
	CANE_LOG(LOG_WARN);

	Sequence seq = seq_prefix(ctx, lx, 0);
	Token tok = lx.peek();

	// Continue while we have a non-prefix operator.
	while (
		is_seq_infix_literal(tok.kind) or
		is_seq_infix_expr(tok.kind) or
		is_seq_postfix(tok.kind)
	) {
		// Handle postfix operators
		if (is_seq_postfix(tok.kind)) {
			auto [lbp, rbp] = seq_postfix_bp(lx, tok);

			if (lbp < bp)
				break;

			seq = seq_postfix(ctx, lx, std::move(seq), 0);
		}

		// Handle infix operators
		else if (is_seq_infix(tok.kind)) {
			auto [lbp, rbp] = seq_infix_bp(lx, tok);

			if (lbp < bp)
				break;

			if (is_seq_infix_literal(tok.kind))
				seq = seq_infix_literal(ctx, lx, std::move(seq), rbp);

			else if (is_seq_infix_expr(tok.kind))
				seq = seq_infix_expr(ctx, lx, std::move(seq), rbp);

			else
				lx.error(Phases::SYNTACTIC, tok.view, STR_SEQ_INFIX);
		}

		// Error if not operator
		else {
			lx.error(Phases::SYNTACTIC, tok.view, STR_SEQ_EXPR);
		}

		tok = lx.peek();
	}

	return seq;
}

inline Sequence sink(Context& ctx, Lexer& lx, Sequence seq) {
	CANE_LOG(LOG_INFO);

	lx.expect(equal(Symbols::SINK), lx.peek().view, STR_EXPECT, sym2str(Symbols::SINK));
	lx.next();  // skip `~>`

	View view = lx.peek().view;
	Channel channel = 1;


	// Sink can be either a literal number or an alias defined previously.
	if (is_literal(lx.peek().kind)) {
		channel = literal(ctx, lx);
	}

	else if (lx.peek().kind == Symbols::IDENT) {
		lx.next();  // skip identifier

		auto it = ctx.aliases.find(view);
		if (it == ctx.aliases.end())
			lx.error(Phases::SEMANTIC, view, STR_UNDEFINED, view);

		channel = it->second + 1;
	}

	else {
		lx.error(Phases::SYNTACTIC, lx.peek().view, STR_IDENT_LITERAL);
	}


	// Generate timeline events.
	if (channel > CHANNEL_MAX or channel < CHANNEL_MIN)
		lx.error(Phases::SEMANTIC, view, STR_BETWEEN, CHANNEL_MIN, CHANNEL_MAX);

	channel--; // 0-15 range

	size_t ms_per_note = 60'000 / seq.bpm;

	auto [it, succ] = ctx.times.try_emplace(channel, 0);
	size_t& time = it->second;

	for (Step s: seq) {
		if (s) { // Note on
			ctx.timeline.emplace_back(time, 0b1001, channel, 52, 0b01111111);
			ctx.timeline.emplace_back(time + ms_per_note, 0b1000, channel, 52, 0b01111111);
		}

		time += ms_per_note;
	}

	// This is important to make sure the timeline is ordered based on
	// timestamp or else we get a garbled song.
	std::stable_sort(ctx.timeline.begin(), ctx.timeline.end(), [] (auto& a, auto& b) {
		return a.time < b.time;
	});

	return seq;
}

inline void statement(Context& ctx, Lexer& lx) {
	CANE_LOG(LOG_WARN);

	Token tok = lx.peek();

	// Clear the timeline.
	if (tok.kind == Symbols::CLEAR) {
		lx.next();  // skip `clear`

		ctx.timeline.clear();
		ctx.times.clear();
	}

	// Synchronise all sinks.
	else if (tok.kind == Symbols::WAIT) {
		lx.next();  // skip `wait`

		// Find the channel with the current maximum time
		// and then set every channels timer to it so they
		// sychronise.
		auto it = std::max_element(ctx.times.begin(), ctx.times.end(), [] (auto& a, auto& b) {
			return a.second < b.second;
		});

		if (it == ctx.times.end())
			return;

		size_t max_time = it->second;
		for (auto& [channel, time]: ctx.times)
			time = max_time;
	}

	// Alias a sink.
	else if (tok.kind == Symbols::ALIAS) {
		lx.next();  // skip `alias`

		lx.expect(equal(Symbols::IDENT), lx.peek().view, STR_IDENT);
		auto [view, kind] = lx.next();  // get identifier

		lx.expect(equal(Symbols::SINK), lx.peek().view, STR_EXPECT, sym2str(Symbols::SINK));
		lx.next();  // skip `~>`

		View chan_v = lx.peek().view;
		Channel channel = literal(ctx, lx);

		if (channel > CHANNEL_MAX or channel < CHANNEL_MIN)
			lx.error(Phases::SEMANTIC, chan_v, STR_BETWEEN, CHANNEL_MIN, CHANNEL_MAX);

		channel--;  // 0-15 range

		// Assign or warn if re-assigned.
		if (auto [it, succ] = ctx.aliases.try_emplace(view, channel); not succ) {
			lx.warning(Phases::SEMANTIC, view, STR_REDEFINED, view);
			it->second = channel;
		}
	}

	else if (tok.kind == Symbols::LET) {
		lx.next();  // skip `const`

		lx.expect(equal(Symbols::IDENT), lx.peek().view, STR_IDENT);
		auto [view, kind] = lx.next();  // get identifier

		Literal lit = lit_expression(ctx, lx, 0);

		// Assign or warn if re-assigned.
		if (auto [it, succ] = ctx.constants.try_emplace(view, lit); not succ) {
			lx.warning(Phases::SEMANTIC, view, STR_REDEFINED, view);
			it->second = lit;
		}
	}

	else if (is_seq_expr(tok.kind)) {
		Sequence seq = seq_expression(ctx, lx, 0);

		while (lx.peek().kind == Symbols::SINK)
			seq = sink(ctx, lx, std::move(seq));
	}

	else {
		lx.error(Phases::SYNTACTIC, tok.view, STR_STATEMENT);
	}
}

inline Context compile(Lexer& lx) {
	CANE_LOG(LOG_WARN);

	Context ctx;

	while (lx.peek().kind != Symbols::TERMINATOR)
		statement(ctx, lx);

	// Send "all notes off" message to channels in use.
	for (auto& [channel, time]: ctx.times)
		ctx.timeline.emplace_back(time, 0b1011, channel, 123, 0);

	return ctx;
}

}

#endif
