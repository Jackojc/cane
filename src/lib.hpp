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

struct Error {};

template <typename... Ts>
[[noreturn]] inline void halt(Ts&&... args) {
	report(std::cerr, std::forward<Ts>(args)...);
	throw Error {};
}

template <typename... Ts>
inline void warn(Ts&&... args) {
	report<Report::WARNING>(std::cerr, std::forward<Ts>(args)...);
}

template <typename... Ts>
inline void note(Ts&&... args) {
	report<Report::NOTICE>(std::cerr, std::forward<Ts>(args)...);
}

#define SYMBOL_TYPES \
	X(NONE,       "none") \
	X(TERMINATOR, "eof") \
	\
	/* Special */ \
	X(SYNC,  "sync") \
	X(IDENT, "ident") \
	X(INT,   "int") \
	X(HEX,   "hex") \
	X(BIN,   "bin") \
	\
	/* Keywords & Grouping */ \
	X(LPAREN, "(") \
	X(RPAREN, ")") \
	X(LSEQ,   "[") \
	X(RSEQ,   "]") \
	\
	/* Operators */ \
	X(CHAIN,  "=>") \
	X(SINK,   "~>") \
	X(SEP,    "/") \
	X(SKIP,   ".") \
	X(BEAT,   "!") \
	\
	/* Argument Infix Operators */ \
	X(ROTLN, "<<") \
	X(ROTRN, ">>") \
	X(REPN,  "*") \
	X(BPM,   "@") \
	\
	/* Binary Sequence Operators */ \
	X(OR,  "|") \
	X(AND, "&") \
	X(XOR, "^") \
	X(CAT, ",") \
	\
	/* Postfix Sequence Operators */ \
	X(INVERT, "~") \
	X(ROTL,   "<") \
	X(ROTR,   ">") \
	X(REV,    "'") \
	X(DBG,    "?")

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

	template <typename... Ts>
	void warning(Phases phase, View sv, Ts&&... args) {
		warn(phase, original, sv, std::forward<Ts>(args)...);
	}

	template <typename... Ts>
	void notice(Phases phase, View sv, Ts&&... args) {
		note(phase, original, sv, std::forward<Ts>(args)...);
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

		else if (c == '?')  { kind = Symbols::DBG;    src = cane::iter_next_char(src, c); }
		else if (c == '\'') { kind = Symbols::REV;    src = cane::iter_next_char(src, c); }
		else if (c == '/')  { kind = Symbols::SEP;    src = cane::iter_next_char(src, c); }
		else if (c == '@')  { kind = Symbols::BPM;    src = cane::iter_next_char(src, c); }
		else if (c == '|')  { kind = Symbols::OR;     src = cane::iter_next_char(src, c); }
		else if (c == '&')  { kind = Symbols::AND;    src = cane::iter_next_char(src, c); }
		else if (c == '^')  { kind = Symbols::XOR;    src = cane::iter_next_char(src, c); }
		else if (c == ',')  { kind = Symbols::CAT;    src = cane::iter_next_char(src, c); }
		else if (c == '*')  { kind = Symbols::REPN;   src = cane::iter_next_char(src, c); }
		else if (c == '!')  { kind = Symbols::BEAT;   src = cane::iter_next_char(src, c); }
		else if (c == '.')  { kind = Symbols::SKIP;   src = cane::iter_next_char(src, c); }

		else if (c == '[') { kind = Symbols::LSEQ;   src = cane::iter_next_char(src, c); }
		else if (c == ']') { kind = Symbols::RSEQ;   src = cane::iter_next_char(src, c); }

		else if (c == '(') { kind = Symbols::LPAREN; src = cane::iter_next_char(src, c); }
		else if (c == ')') { kind = Symbols::RPAREN; src = cane::iter_next_char(src, c); }

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

		else if (cane::is_letter(c)) {
			kind = Symbols::IDENT;
			view = cane::consume_char(src, c, cane::is_alphanumeric);

			if (view == "sync"_sv)
				kind = Symbols::SYNC;
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

using Step = uint8_t;
using Channel = uint8_t;

struct Sequence: public std::vector<Step> {
	using std::vector<Step>::vector;
};

inline std::ostream& operator<<(std::ostream& os, Sequence& s) {
	out(os, "[");

	for (auto x: s) {
		out(os,
			x ? CANE_ANSI_FG_BRIGHT_YELLOW: CANE_ANSI_FG_BLUE,
			x ? sym2str(Symbols::BEAT): sym2str(Symbols::SKIP)
		);
	}

	return out(os, CANE_ANSI_RESET "]");
}

struct Pattern {
	Sequence seq;
	size_t bpm;
	size_t channel;

	Pattern(const Sequence& seq_, size_t bpm_, size_t channel_):
		seq(seq_), bpm(bpm_), channel(channel_) {}
};

struct Context {
	std::vector<Pattern> channels;
	std::unordered_map<View, Sequence> symbols;
};


// Operations
// Transform sequence in place using function object.
template <typename V, typename F>
constexpr void transform_seq_in_place(V& seq, const F& fn, V& other) {
	// Step stretching, we pick the largest sequence to transform.
	if (other.size() > seq.size())
		std::swap(other, seq);

	other.resize(seq.size());

	std::transform(other.cbegin(), other.cend(), seq.begin(), seq.begin(), fn);
}

template <typename V> constexpr decltype(auto) reverse(V v) {
	std::reverse(v.begin(), v.end());
	return std::move(v);
}

template <typename V> constexpr decltype(auto) rotl(V v, size_t n = 1) {
	std::rotate(v.begin(), v.begin() + n, v.end());
	return std::move(v);
}

template <typename V> constexpr decltype(auto) rotr(V v, size_t n = 1) {
	std::rotate(v.rbegin(), v.rbegin() + n, v.rend());
	return std::move(v);
}

template <typename V> constexpr decltype(auto) invert(V v) {
	std::transform(v.begin(), v.end(), v.begin(), std::logical_not<>{});
	return std::move(v);
}

template <typename V> constexpr decltype(auto) repeat(V v, size_t n = 1) {
	// Copy sequence N times to the end of itself.
	// turns i.e. `[a b c]` where N=3 into `[a b c a b c a b c]`.

	if (n == 0)
		return std::move(v);

	size_t count = v.size();
	v.reserve(v.capacity() + n * count);

	while (--n)
		std::copy_n(v.begin(), count, std::back_inserter(v));

	return std::move(v);
}

template <typename V1, typename V2> constexpr decltype(auto) cat(V1 a, V2 b) {
	a.insert(a.end(), b.begin(), b.end());
	return std::move(a);
}

template <typename V1, typename V2> constexpr decltype(auto) disjunction(V1 a, V2 b) {
	transform_seq_in_place(a, std::bit_or<>{}, b);
	return std::move(a);
}

template <typename V1, typename V2> constexpr decltype(auto) conjunction(V1 a, V2 b) {
	transform_seq_in_place(a, std::bit_and<>{}, b);
	return std::move(a);
}

template <typename V1, typename V2> constexpr decltype(auto) ex_disjunction(V1 a, V2 b) {
	transform_seq_in_place(a, std::bit_xor<>{}, b);
	return std::move(a);
}


// Literal expressions
inline size_t literal (Context&, Lexer&);

// Sequence expressions
inline Sequence  sequence   (Context&, Lexer&);
inline Sequence  euclide    (Context&, Lexer&);
inline Sequence  reference  (Context&, Lexer&);
inline Sequence& chain      (Context&, Lexer&, Sequence&);
inline Sequence  expression (Context&, Lexer&, size_t = 0);

// Statements
inline Sequence& sink      (Context&, Lexer&, Sequence&);
inline void      sync      (Context&, Lexer&);
inline void      statement (Context&, Lexer&);

inline Context compile (Lexer&);


// Predicates
constexpr auto is_expr = partial_eq_any(
	Symbols::REV,
	Symbols::ROTL,
	Symbols::ROTR,
	Symbols::INVERT,
	Symbols::INT,
	Symbols::HEX,
	Symbols::BIN,
	Symbols::IDENT,
	Symbols::LSEQ,
	Symbols::LPAREN
);

constexpr auto is_literal = partial_eq_any(
	Symbols::INT,
	Symbols::HEX,
	Symbols::BIN
);

constexpr auto is_prefix = partial_eq_any(
	Symbols::INVERT,
	Symbols::ROTL,
	Symbols::ROTR,
	Symbols::REV
);

constexpr auto is_infix = partial_eq_any(
	Symbols::CHAIN,
	Symbols::OR,
	Symbols::AND,
	Symbols::XOR,
	Symbols::CAT,
	Symbols::ROTLN,
	Symbols::ROTRN,
	Symbols::REPN
);

constexpr auto is_postfix = partial_eq_any(
	Symbols::INVERT,
	Symbols::ROTL,
	Symbols::ROTR,
	Symbols::REV,
	Symbols::DBG
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

	lx.expect(equal(Symbols::LSEQ), lx.peek().view, STR_EXPECT, sym2str(Symbols::LSEQ));
	lx.next();  // skip `[`

	Sequence seq;

	while (lx.peek().kind != Symbols::RSEQ) {
		lx.expect(is_step, lx.peek().view, STR_STEP);
		Symbols step = lx.next().kind;
		seq.emplace_back(step == Symbols::BEAT);
	}

	lx.expect(equal(Symbols::RSEQ), lx.peek().view, STR_EXPECT, sym2str(Symbols::RSEQ));
	lx.next();  // skip `]`

	return seq;
}

inline Sequence euclide(Context& ctx, Lexer& lx) {
	CANE_LOG(LOG_INFO);

	View lv = lx.peek().view;

	size_t steps = 0;
	size_t beats = literal(ctx, lx);

	lx.expect(equal(Symbols::SEP), lx.peek().view, STR_EXPECT, sym2str(Symbols::SEP));
	lx.next();  // skip `/`

	steps = literal(ctx, lx);

	if (beats > steps)
		lx.error(Phases::PHASE_SEMANTIC, lv, STR_LESSER_EQ, steps);

	Sequence seq;

	for (size_t i = 0; i != steps; ++i)
		seq.emplace_back(((i * beats) % steps) < beats);

	return seq;
}

inline Sequence reference(Context& ctx, Lexer& lx) {
	CANE_LOG(LOG_INFO);

	lx.expect(equal(Symbols::IDENT), lx.peek().view, STR_IDENT);
	auto [view, kind] = lx.next();

	// Lookup symbol
	auto it = ctx.symbols.find(view);

	if (it == ctx.symbols.end())
		lx.error(Phases::PHASE_SEMANTIC, view, STR_UNDEFINED, view);

	return it->second;
}

inline Sequence& chain(Context& ctx, Lexer& lx, Sequence& seq) {
	CANE_LOG(LOG_INFO);

	lx.expect(equal(Symbols::CHAIN), lx.peek().view, STR_EXPECT, sym2str(Symbols::CHAIN));
	lx.next();  // skip `=>`

	lx.expect(equal(Symbols::IDENT), lx.peek().view, STR_IDENT);
	auto [view, kind] = lx.next();

	// Assign or error if re-assigned.
	if (auto [it, succ] = ctx.symbols.try_emplace(view, seq); not succ)
		lx.error(Phases::PHASE_SEMANTIC, view, STR_REDEFINED, view);

	return seq;
}

// Lookup table for prefix operator precedence.
constexpr size_t prefix_precedence(Symbols kind) {
	size_t prec = 0;

	switch (kind) {
		case Symbols::REV:
		case Symbols::ROTL:
		case Symbols::ROTR:
		case Symbols::INVERT: {
			prec = 4;
		} break;

		default: {
			prec = 0;
		} break;
	}

	return prec;
}

// Lookup table for infix operator precedence.
constexpr size_t infix_precedence(Symbols kind) {
	size_t prec = 0;

	switch (kind) {
		case Symbols::DBG:
		case Symbols::CHAIN: {
			prec = 1;
		} break;

		case Symbols::CAT:
		case Symbols::REV:
		case Symbols::ROTL:
		case Symbols::ROTR:
		case Symbols::INVERT: {
			prec = 2;
		} break;

		case Symbols::ROTLN:
		case Symbols::ROTRN:
		case Symbols::REPN:
		case Symbols::OR:
		case Symbols::AND:
		case Symbols::XOR: {
			prec = 3;
		} break;

		default: {
			prec = 0;
		} break;
	}

	return prec;
}

inline Sequence expression(Context& ctx, Lexer& lx, size_t bp) {
	CANE_LOG(LOG_WARN);

	Sequence seq;

	// Nud (Nothing to the left, prefix position)
	size_t prec = prefix_precedence(lx.peek().kind);

	switch (lx.peek().kind) {
		// Prefix operators
		case Symbols::REV: {
			lx.next();  // skip operator.
			seq = reverse(expression(ctx, lx, prec));
		} break;

		case Symbols::ROTL: {
			lx.next();  // skip operator.
			seq = rotl(expression(ctx, lx, prec));
		} break;

		case Symbols::ROTR: {
			lx.next();  // skip operator.
			seq = rotr(expression(ctx, lx, prec));
		} break;

		case Symbols::INVERT: {
			lx.next();  // skip operator.
			seq = invert(expression(ctx, lx, prec));
		} break;


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
			lx.next();  // skip `(`

			seq = expression(ctx, lx);

			lx.expect(equal(Symbols::RPAREN), lx.peek().view, STR_EXPECT, sym2str(Symbols::RPAREN));
			lx.next();  // skip `)`
		} break;

		default: {
			lx.error(Phases::PHASE_SYNTACTIC, lx.peek().view, STR_NUD);
		} break;
	}

	while (is_operator(lx.peek().kind)) {
		size_t prec = infix_precedence(lx.peek().kind);

		if (any(prec <= bp, prec == 0))
			break;

		switch (lx.peek().kind) {
			// Infix with identifier
			case Symbols::CHAIN: {
				seq = chain(ctx, lx, seq);
			} break;


			// Postfix
			case Symbols::DBG: {
				View dbg_v = lx.next().view;  // skip operator
				lx.notice(Phases::PHASE_SEMANTIC, dbg_v, STR_DEBUG, seq);
			} break;

			case Symbols::REV: {
				lx.next();  // skip operator
				seq = reverse(std::move(seq));
			} break;

			case Symbols::ROTL: {
				lx.next();  // skip operator
				seq = rotl(std::move(seq));
			} break;

			case Symbols::ROTR: {
				lx.next();  // skip operator
				seq = rotr(std::move(seq));
			} break;

			case Symbols::INVERT: {
				lx.next();  // skip operator
				seq = invert(std::move(seq));
			} break;


			// Infix with literal
			case Symbols::ROTLN: {
				lx.next();  // skip operator
				seq = rotl(std::move(seq), literal(ctx, lx));
			} break;

			case Symbols::ROTRN: {
				lx.next();  // skip operator
				seq = rotr(std::move(seq), literal(ctx, lx));
			} break;

			case Symbols::REPN: {
				lx.next();  // skip operator

				View lv = lx.peek().view;
				size_t n = literal(ctx, lx);

				// We don't want to shrink the sequence, it can only grow.
				if (n == 0)
					lx.error(Phases::PHASE_SEMANTIC, lv, STR_GREATER, 0);

				seq = repeat(std::move(seq), n);
			} break;


			// Infix with expr
			case Symbols::CAT: {
				lx.next();  // skip operator
				seq = cat(std::move(seq), expression(ctx, lx, prec));
			} break;

			case Symbols::OR: {
				lx.next();  // skip operator
				seq = disjunction(std::move(seq), expression(ctx, lx, prec));
			} break;

			case Symbols::AND: {
				lx.next();  // skip operator
				seq = conjunction(std::move(seq), expression(ctx, lx, prec));
			} break;

			case Symbols::XOR: {
				lx.next();  // skip operator
				seq = ex_disjunction(std::move(seq), expression(ctx, lx, prec));
			} break;


			default: {
				lx.error(Phases::PHASE_SYNTACTIC, lx.peek().view, STR_OPERATOR);
			} break;
		}
	}

	return seq;
}

inline Sequence& sink(Context& ctx, Lexer& lx, Sequence& seq) {
	CANE_LOG(LOG_INFO);

	lx.expect(equal(Symbols::SINK), lx.peek().view, STR_EXPECT, sym2str(Symbols::SINK));
	lx.next();  // skip `~>`

	View chan_v = lx.peek().view;
	size_t channel = literal(ctx, lx);

	size_t bpm = 120;

	if (lx.peek().kind == Symbols::BPM) {
		lx.next();  // skip `@`

		View bpm_v = lx.peek().view;
		bpm = literal(ctx, lx);

		if (bpm == 0)
			lx.error(Phases::PHASE_SEMANTIC, bpm_v, STR_GREATER, 0);
	}

	if (channel > 15)
		lx.error(Phases::PHASE_SEMANTIC, chan_v, STR_BETWEEN, 0, 15);

	ctx.channels.emplace_back(seq, bpm, channel);

	return seq;
}

inline void sync(Context& ctx, Lexer& lx) {
	CANE_LOG(LOG_INFO);
	lx.next();  // skip `sync`

	View count_v = lx.peek().view;
	size_t count = literal(ctx, lx);

	if (count == 0)
		lx.error(Phases::PHASE_SEMANTIC, count_v, STR_GREATER, 0);

	auto& channels = ctx.channels;

	std::stable_sort(channels.begin(), channels.end(), [] (const auto& a, const auto& b) {
		return a.channel < b.channel;
	});

	size_t lcm = 1;

	struct Info {
		size_t offset;
		size_t length;
		size_t steps;

		Info(size_t offset_, size_t length_, size_t steps_):
			offset(offset_), length(length_), steps(steps_) {}
	};

	std::vector<Info> ranges;

	for (auto it = channels.begin(); it != channels.end();) {
		const auto begin = it;

		size_t offset = std::distance(channels.begin(), it);
		size_t steps = 0;
		size_t channel = it->channel;

		for (; it != channels.end(); ++it) {
			if (it->channel != channel)
				break;

			steps += 60'000 / it->bpm * it->seq.size();
		}

		size_t length = std::distance(begin, it);

		lcm = std::lcm(lcm, steps);
		ranges.emplace_back(offset, length, steps);
	}

	for (auto [offset, length, steps]: ranges) {
		size_t reps = lcm / steps;
		size_t count = channels.size();

		channels.reserve(channels.capacity() + reps * count);

		while (--reps)
			std::copy_n(channels.begin() + offset, length, std::back_inserter(channels));
	}

	// channels.clear();
}

inline void statement(Context& ctx, Lexer& lx) {
	CANE_LOG(LOG_WARN);

	// Parse expression with optional sink
	if (is_expr(lx.peek().kind)) {
		Sequence seq = expression(ctx, lx);

		while (lx.peek().kind == Symbols::SINK)
			seq = sink(ctx, lx, seq);
	}

	// Parse sync with optional literal
	else if (lx.peek().kind == Symbols::SYNC)
		sync(ctx, lx);

	else
		lx.error(Phases::PHASE_SYNTACTIC, lx.peek().view, STR_STATEMENT);
}

inline Context compile(Lexer& lx) {
	Context ctx;

	while (lx.peek().kind != Symbols::TERMINATOR)
		statement(ctx, lx);

	return ctx;
}

}

#endif
