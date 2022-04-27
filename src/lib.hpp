#ifndef CANE_LIB_HPP
#define CANE_LIB_HPP

#include <iostream>
#include <string>
#include <string_view>
#include <vector>
#include <array>
#include <unordered_map>
#include <unordered_set>
#include <algorithm>

#include <locale.hpp>
#include <print.hpp>
#include <view.hpp>
#include <unicode.hpp>
#include <log.hpp>
#include <report.hpp>

namespace cane {

struct Error {};

template <typename... Ts>
[[noreturn]] inline void halt(Ts&&... args) {
	report(std::cerr, std::forward<Ts>(args)...);
	throw Error {};
}

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
	/* Keywords & Grouping */ \
	X(MIDI,   "midi") \
	X(LPAREN, "(") \
	X(RPAREN, ")") \
	X(LSEQ,   "[") \
	X(RSEQ,   "]") \
	\
	/* Operators */ \
	X(CHAIN,  "=>") \
	X(SEP,    "/") \
	X(OFFSET, "+") \
	X(SKIP,   ".") \
	X(BEAT,   "!") \
	\
	/* Argument Operators */ \
	X(LSHN, "<<") \
	X(RSHN, ">>") \
	X(REPN, "*") \
	X(BPM,  "@") \
	\
	/* Sequence Operators */ \
	X(OR,  "|") \
	X(AND, "&") \
	X(XOR, "^") \
	X(CAT, ",") \
	X(NOT, "~") \
	X(LSH, "<") \
	X(RSH, ">")

	#define X(name, str) name,
		enum class Symbols { SYMBOL_TYPES };
	#undef X

	#define X(name, str) str##_sv,
		constexpr View SYMBOL_TO_STRING[] = { SYMBOL_TYPES };
	#undef X

	constexpr decltype(auto) sym2str(Symbols sym) {
		return SYMBOL_TO_STRING[(int)sym];
	}

#undef SYMBOL_TYPES

inline std::ostream& operator<<(std::ostream& os, Symbols k) {
	return (os << sym2str(k));
}

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
			halt(Phases::PHASE_SYNTACTIC, original, sv, std::forward<Ts>(args)...);
	}

	template <typename... Ts>
	void error(Phases phase, View sv, Ts&&... args) {
		halt(phase, original, sv, std::forward<Ts>(args)...);
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

		else if (c == '/') { kind = Symbols::SEP;    src = cane::iter_next_char(src, c); }
		else if (c == '@') { kind = Symbols::BPM;    src = cane::iter_next_char(src, c); }
		else if (c == '+') { kind = Symbols::OFFSET; src = cane::iter_next_char(src, c); }
		else if (c == '|') { kind = Symbols::OR;     src = cane::iter_next_char(src, c); }
		else if (c == '&') { kind = Symbols::AND;    src = cane::iter_next_char(src, c); }
		else if (c == '^') { kind = Symbols::XOR;    src = cane::iter_next_char(src, c); }
		else if (c == ',') { kind = Symbols::CAT;    src = cane::iter_next_char(src, c); }
		else if (c == '~') { kind = Symbols::NOT;    src = cane::iter_next_char(src, c); }
		else if (c == '*') { kind = Symbols::REPN;   src = cane::iter_next_char(src, c); }
		else if (c == '!') { kind = Symbols::BEAT;   src = cane::iter_next_char(src, c); }
		else if (c == '.') { kind = Symbols::SKIP;   src = cane::iter_next_char(src, c); }

		else if (c == '[') { kind = Symbols::LSEQ;   src = cane::iter_next_char(src, c); }
		else if (c == ']') { kind = Symbols::RSEQ;   src = cane::iter_next_char(src, c); }

		else if (c == '(') { kind = Symbols::LPAREN; src = cane::iter_next_char(src, c); }
		else if (c == ')') { kind = Symbols::RPAREN; src = cane::iter_next_char(src, c); }

		else if (c == '<') {
			kind = Symbols::LSH;
			src = cane::iter_next_char(src, c);

			if (as_char(src) == '<') {
				kind = Symbols::LSHN;
				view = overlap(view, as_view(src));
				src = cane::iter_next_char(src, c);
			}
		}

		else if (c == '>') {
			kind = Symbols::RSH;
			src = cane::iter_next_char(src, c);

			if (as_char(src) == '>') {
				kind = Symbols::RSHN;
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
			if (c == '0') {
				const auto lbegin = begin;  // Save starting position of token so we include "0x" or "0b"
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

				begin = lbegin;
			}

			kind = Symbols::INT;
			view = cane::consume_char(src, c, cane::is_number);
		}

		else if (cane::is_letter(c)) {
			kind = Symbols::IDENT;
			view = cane::consume_char(src, c, cane::is_alphanumeric);

			if (view == "midi"_sv)
				kind = Symbols::MIDI;
		}

		// If the kind is still NONE by this point, we can assume we didn't find
		// a valid // token. The reason this check is disconnected from the above
		// if-else chain is because some checks are nested and only fail after
		// succeeding with the original check so we wouldn't fall through if this
		// check was connected.
		if (kind == Symbols::NONE) {
			halt(Phases::PHASE_LEXICAL, original, view, STR_UNKNOWN_CHAR, view);
		}

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


using Sequence = std::vector<uint8_t>;
using Notes = std::vector<uint8_t>;
using Channel = uint8_t;

struct Pattern {
	Sequence seq;
	Notes notes;
	size_t bpm;

	Pattern(const Sequence& seq_, const Notes& notes_, size_t bpm_):
		seq(seq_), notes(notes_), bpm(bpm_) {}
};

using Chain = std::vector<Pattern>;

struct Context {
	std::array<Chain, 16> chains;
	std::unordered_map<View, Sequence> symbols;
};


// Literal expressions return a size_t.
// Sequence expressions return a sequence.
// Statements return void.

// Literal expressions
inline size_t literal (Context&, Lexer&);
inline size_t step    (Context&, Lexer&);

// Sequence expressions
inline Sequence sequence   (Context&, Lexer&);
inline Sequence euclide    (Context&, Lexer&);
inline Sequence reference  (Context&, Lexer&);

inline Sequence& infix      (Context&, Lexer&, Sequence&);
inline Sequence& postfix    (Context&, Lexer&, Sequence&);

inline Sequence expression (Context&, Lexer&);

// Statements
inline void midi      (Context&, Lexer&, Sequence&);
inline void assign    (Context&, Lexer&, Sequence&);

inline void statement (Context&, Lexer&);

inline Context compile (Lexer&);


// Predicates
constexpr auto is_literal = partial_eq_any(
	Symbols::INT,
	Symbols::HEX,
	Symbols::BIN
);

constexpr auto is_infix = partial_eq_any(
	Symbols::OR,
	Symbols::AND,
	Symbols::XOR,
	Symbols::CAT,
	Symbols::LSHN,
	Symbols::RSHN,
	Symbols::REPN
);

constexpr auto is_postfix = partial_eq_any(
	Symbols::NOT,
	Symbols::LSH,
	Symbols::RSH
);

constexpr auto is_step = partial_eq_any(
	Symbols::SKIP,
	Symbols::BEAT
);

constexpr auto is_operator = [] (auto x) {
	return is_infix(x) or is_postfix(x);
};


// Defs
inline size_t literal(Context& ctx, Lexer& lx) {
	// CANE_LOG(LOG_INFO, "literal");

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

inline size_t step(Context& ctx, Lexer& lx) {
	// CANE_LOG(LOG_INFO, "step");

	lx.expect(is_step, lx.peek().view, STR_STEP);
	Symbols step = lx.next().kind;

	return step == Symbols::BEAT;
}

inline Sequence sequence(Context& ctx, Lexer& lx) {
	// CANE_LOG(LOG_INFO, "seq");

	lx.expect(equal(Symbols::LSEQ), lx.peek().view, STR_EXPECT, sym2str(Symbols::LSEQ));
	lx.next();  // skip `[`

	Sequence seq;

	while (lx.peek().kind != Symbols::RSEQ) {
		size_t s = step(ctx, lx);
		seq.emplace_back(s);
	}

	lx.expect(equal(Symbols::RSEQ), lx.peek().view, STR_EXPECT, sym2str(Symbols::RSEQ));
	lx.next();  // skip `]`

	return seq;
}

inline Sequence euclide(Context& ctx, Lexer& lx) {
	// CANE_LOG(LOG_INFO, "euclide");

	size_t offset = 0;
	size_t steps = 0;
	size_t beats = literal(ctx, lx);

	lx.expect(equal(Symbols::SEP), lx.peek().view, STR_EXPECT, sym2str(Symbols::SEP));
	lx.next();  // skip `/`

	steps = literal(ctx, lx);

	if (lx.peek().kind == Symbols::OFFSET) {
		lx.next();  // skip `+`
		offset = literal(ctx, lx);
	}

	Sequence seq;

	for (size_t i = 0; i != steps; ++i)
		seq.emplace_back((((i + offset) * beats) % steps) < beats);

	return seq;
}

inline Sequence reference(Context& ctx, Lexer& lx) {
	CANE_LOG(LOG_INFO, "reference");

	lx.expect(equal(Symbols::IDENT), lx.peek().view, STR_IDENT);
	auto [view, kind] = lx.next();

	// Lookup symbol
	auto it = ctx.symbols.find(view);

	if (it == ctx.symbols.end())
		lx.error(Phases::PHASE_SEMANTIC, view, STR_UNDEFINED, view);

	return it->second;
}

inline Sequence& infix(Context& ctx, Lexer& lx, Sequence& seq) {
	// CANE_LOG(LOG_INFO, "infix");

	lx.expect(is_infix, lx.peek().view, STR_INFIX);
	Symbols kind = lx.next().kind;  // skip operator

	switch (kind) {
		// Infix literal
		case Symbols::LSHN: {
			size_t n = literal(ctx, lx);
			std::rotate(seq.begin(), seq.begin() + n, seq.end());
		} break;

		case Symbols::RSHN: {
			size_t n = literal(ctx, lx);
			std::rotate(seq.rbegin(), seq.rbegin() + n, seq.rend());
		} break;

		case Symbols::REPN: {
			View lv = lx.peek().view;
			size_t n = literal(ctx, lx);

			if (n == 0)
				lx.error(Phases::PHASE_SEMANTIC, lv, STR_GREATER, 0);

			size_t cnt = seq.size();

			seq.reserve(seq.capacity() + n * cnt);

			while (--n)
				std::copy_n(seq.begin(), cnt, std::back_inserter(seq));
		} break;

		// Infix expr
		case Symbols::OR: {
			Sequence rhs = expression(ctx, lx);

			if (rhs.size() > seq.size())
				std::swap(rhs, seq);

			std::transform(rhs.begin(), rhs.end(), seq.begin(), seq.begin(), std::bit_or<>{});
		} break;

		case Symbols::AND: {
			Sequence rhs = expression(ctx, lx);

			if (rhs.size() > seq.size())
				std::swap(rhs, seq);

			std::transform(rhs.begin(), rhs.end(), seq.begin(), seq.begin(), std::bit_and<>{});
		} break;

		case Symbols::XOR: {
			Sequence rhs = expression(ctx, lx);

			if (rhs.size() > seq.size())
				std::swap(rhs, seq);

			std::transform(rhs.begin(), rhs.end(), seq.begin(), seq.begin(), std::bit_xor<>{});
		} break;

		case Symbols::CAT: {
			Sequence rhs = expression(ctx, lx);

			if (rhs.size() > seq.size())
				std::swap(rhs, seq);

			seq.insert(seq.end(), rhs.begin(), rhs.end());
		} break;

		default: {
			lx.error(Phases::PHASE_SYNTACTIC, lx.peek().view, STR_INFIX);
		} break;
	}

	return seq;
}

inline Sequence& postfix(Context& ctx, Lexer& lx, Sequence& seq) {
	// CANE_LOG(LOG_INFO, "postfix");

	lx.expect(is_postfix, lx.peek().view, STR_POSTFIX);
	Symbols kind = lx.next().kind;  // skip operator

	switch (kind) {
		case Symbols::LSH: {
			std::rotate(seq.begin(), seq.begin() + 1, seq.end());
		} break;

		case Symbols::RSH: {
			std::rotate(seq.rbegin(), seq.rbegin() + 1, seq.rend());
		} break;

		case Symbols::NOT: {
			std::transform(seq.begin(), seq.end(), seq.begin(), std::logical_not<>{});
		} break;

		default: {
			lx.error(Phases::PHASE_SYNTACTIC, lx.peek().view, STR_POSTFIX);
		} break;
	}

	return seq;
}

inline Sequence expression(Context& ctx, Lexer& lx) {
	// CANE_LOG(LOG_INFO, "expr");

	Sequence seq;

	// Nud (Nothing to the left, prefix position)
	switch (lx.peek().kind) {
		// Euclidean sequence
		case Symbols::INT:
		case Symbols::HEX:
		case Symbols::BIN: {
			seq = euclide(ctx, lx);
		} break;

		// Variable ref
		case Symbols::IDENT: {
			seq = reference(ctx, lx);
		} break;

		// Sequence
		case Symbols::LSEQ: {
			seq = sequence(ctx, lx);
		} break;

		// Grouped expression
		case Symbols::LPAREN: {
			seq = expression(ctx, lx);
		} break;

		default: {
			lx.error(Phases::PHASE_SYNTACTIC, lx.peek().view, STR_EXPR);
		} break;
	}

	while (is_operator(lx.peek().kind)) {
		CANE_LOG(LOG_WARN, sym2str(lx.peek().kind));

		switch (lx.peek().kind) {
			case Symbols::LSH:
			case Symbols::RSH:
			case Symbols::NOT: {
				seq = postfix(ctx, lx, seq);
			} break;

			case Symbols::LSHN:
			case Symbols::RSHN:
			case Symbols::REPN:
			case Symbols::CAT:
			case Symbols::OR:
			case Symbols::AND:
			case Symbols::XOR: {
				seq = infix(ctx, lx, seq);
			} break;

			default: {
				lx.error(Phases::PHASE_SYNTACTIC, lx.peek().view, STR_OPERATOR);
			} break;
		}
	}

	return seq;
}

inline void assign(Context& ctx, Lexer& lx, Sequence& seq) {
	CANE_LOG(LOG_INFO, "assign");

	lx.expect(equal(Symbols::IDENT), lx.peek().view, STR_IDENT);
	auto [view, kind] = lx.next();

	// Assign or error if re-assigned.
	if (auto [it, succ] = ctx.symbols.try_emplace(view, seq); not succ)
		lx.error(Phases::PHASE_SEMANTIC, view, STR_REDEFINED, view);
}

inline void midi(Context& ctx, Lexer& lx, Sequence& seq) {
	CANE_LOG(LOG_INFO, "midi");

	lx.expect(equal(Symbols::MIDI), lx.peek().view, STR_MIDI);
	lx.next();  // skip `midi`

	size_t channel = literal(ctx, lx);

	lx.expect(equal(Symbols::BPM), lx.peek().view, STR_EXPECT, sym2str(Symbols::BPM));
	lx.next();  // skip `@`

	size_t bpm = literal(ctx, lx);

	ctx.chains[channel].emplace_back(seq, Notes{}, bpm);
}

inline void statement(Context& ctx, Lexer& lx) {
	CANE_LOG(LOG_INFO, "statement");

	Sequence seq = expression(ctx, lx);

	if (lx.peek().kind == Symbols::CHAIN) {
		CANE_LOG(LOG_INFO, "chain");

		lx.next();  // skip `=>`

		switch (lx.peek().kind) {
			case Symbols::IDENT: assign(ctx, lx, seq); break;
			case Symbols::MIDI:  midi(ctx, lx, seq); break;

			default: {
				lx.error(Phases::PHASE_SYNTACTIC, lx.peek().view, STR_CHAIN);
			} break;
		}
	}
}



inline Context compile(Lexer& lx) {
	// CANE_LOG(LOG_INFO, "compile");

	Context ctx;

	while (lx.peek().kind != Symbols::TERMINATOR)
		statement(ctx, lx);

	return ctx;
}

}

#endif
