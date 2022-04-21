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


struct Op {
	Symbols kind = Symbols::NONE;

	View sv = ""_sv;

	size_t x = 0u;
	size_t y = 0u;
	size_t z = 0u;

	constexpr Op(Symbols kind_, View sv_, size_t x_ = 0u, size_t y_ = 0u, size_t z_ = 0u):
		kind(kind_), sv(sv_), x(x_), y(y_), z(z_) {}

	constexpr Op():
		kind(Symbols::NONE) {}
};

struct Instructions: public std::vector<Op> {
	template <typename... Ts> decltype(auto) instruction(Ts&&... args) {
		return this->emplace_back(std::forward<Ts>(args)...);
	}
};


// Decls
inline size_t       compile_literal (Instructions&, Lexer&);
inline size_t       compile_step    (Instructions&, Lexer&);
inline void         compile_seq     (Instructions&, Lexer&);
inline void         compile_ident   (Instructions&, Lexer&);
inline void         compile_euclide (Instructions&, Lexer&);
inline void         compile_midi    (Instructions&, Lexer&);
inline void         compile_chain   (Instructions&, Lexer&);
inline void         compile_infix   (Instructions&, Lexer&);
inline void         compile_postfix (Instructions&, Lexer&);
inline void         compile_expr    (Instructions&, Lexer&);
inline Instructions compile         (Lexer&);


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
inline size_t compile_literal(Instructions& is, Lexer& lx) {
	CANE_LOG(LOG_LEVEL_1, "literal");

	lx.expect(is_literal, lx.peek().view, STR_LITERAL);
	auto [view, kind] = lx.next();

	size_t n = 0;

	// TODO: emit literals
	switch (kind) {
		case Symbols::INT: break;
		case Symbols::HEX: break;
		case Symbols::BIN: break;
		default: break;
	}

	return n;
}

inline size_t compile_step(Instructions& is, Lexer& lx) {
	CANE_LOG(LOG_LEVEL_1, "step");

	lx.expect(is_step, lx.peek().view, STR_STEP);
	Symbols step = lx.next().kind;

	return
		(step == Symbols::BEAT) +
		(step == Symbols::SKIP)
	;
}

inline void compile_seq(Instructions& is, Lexer& lx) {
	CANE_LOG(LOG_LEVEL_1, "seq");

	lx.expect(equal(Symbols::LSEQ), lx.peek().view, STR_EXPECT, sym2str(Symbols::LSEQ));
	lx.next();  // skip `[`

	while (lx.peek().kind != Symbols::RSEQ) {
		size_t s = compile_step(is, lx);
		// TODO: emit sequences
	}

	lx.expect(equal(Symbols::RSEQ), lx.peek().view, STR_EXPECT, sym2str(Symbols::RSEQ));
	lx.next();  // skip `]`
}

inline void compile_ident(Instructions& is, Lexer& lx) {
	CANE_LOG(LOG_LEVEL_1, "ident");

	lx.expect(equal(Symbols::IDENT), lx.peek().view, STR_IDENT);
	auto [view, kind] = lx.next();

	// TODO: emit references to identifiers and resolve definitions
}

inline void compile_euclide(Instructions& is, Lexer& lx) {
	CANE_LOG(LOG_LEVEL_1, "euclide");

	size_t offset = 0;
	size_t steps = 0;
	size_t beats = compile_literal(is, lx);

	lx.expect(equal(Symbols::SEP), lx.peek().view, STR_EXPECT, sym2str(Symbols::SEP));
	lx.next();  // skip `/`

	steps = compile_literal(is, lx);

	if (lx.peek().kind == Symbols::OFFSET) {
		lx.next();  // skip `+`

		offset = compile_literal(is, lx);
	}

	// TODO: emit euclidean sequences (lower to normal sequences first)
}

inline void compile_midi(Instructions& is, Lexer& lx) {
	CANE_LOG(LOG_LEVEL_1, "midi");

	lx.expect(equal(Symbols::MIDI), lx.peek().view, STR_MIDI);
	lx.next();  // skip `midi`

	size_t channel = compile_literal(is, lx);

	lx.expect(equal(Symbols::BPM), lx.peek().view, STR_EXPECT, sym2str(Symbols::BPM));
	lx.next();  // skip `@`

	size_t bpm = compile_literal(is, lx);
}

inline void compile_chain(Instructions& is, Lexer& lx) {
	CANE_LOG(LOG_LEVEL_1, "chain");

	lx.expect(equal(Symbols::CHAIN), lx.peek().view, STR_EXPECT, sym2str(Symbols::CHAIN));
	lx.next();  // skip `=>`

	switch (lx.peek().kind) {
		case Symbols::IDENT: compile_ident(is, lx); break;
		case Symbols::MIDI:  compile_midi(is, lx); break;

		default: {
			lx.error(Phases::PHASE_SYNTACTIC, lx.peek().view, STR_CHAIN);
		} break;
	}
}

inline void compile_infix(Instructions& is, Lexer& lx) {
	CANE_LOG(LOG_LEVEL_1, "infix");

	lx.expect(is_infix, lx.peek().view, STR_INFIX);
	Symbols kind = lx.next().kind;  // skip operator

	switch (kind) {
		// Infix literal
		case Symbols::LSHN:
		case Symbols::RSHN:
		case Symbols::REPN: {
			compile_literal(is, lx);
		} break;

		// Infix expr
		case Symbols::OR:
		case Symbols::AND:
		case Symbols::XOR:
		case Symbols::CAT: {
			compile_expr(is, lx);
		} break;

		default: {
			lx.error(Phases::PHASE_SYNTACTIC, lx.peek().view, STR_INFIX);
		} break;
	}
}

inline void compile_postfix(Instructions& is, Lexer& lx) {
	CANE_LOG(LOG_LEVEL_1, "postfix");

	lx.expect(is_postfix, lx.peek().view, STR_POSTFIX);
	Symbols kind = lx.next().kind;  // skip operator

	switch (kind) {
		case Symbols::RSH: break;
		case Symbols::LSH: break;
		case Symbols::NOT: break;

		default: {
			lx.error(Phases::PHASE_SYNTACTIC, lx.peek().view, STR_POSTFIX);
		} break;
	}
}

inline void compile_expr(Instructions& is, Lexer& lx) {
	CANE_LOG(LOG_LEVEL_1, "expr");

	// Nud (Nothing to the left, prefix position)
	switch (lx.peek().kind) {
		// Euclidean sequence
		case Symbols::INT:
		case Symbols::HEX:
		case Symbols::BIN: {
			compile_euclide(is, lx);
		} break;

		// Variable ref
		case Symbols::IDENT: {
			compile_ident(is, lx);
		} break;

		// Sequence
		case Symbols::LSEQ: {
			compile_seq(is, lx);
		} break;

		// Grouped expression
		case Symbols::LPAREN: {
			compile_expr(is, lx);
		} break;

		default: {
			lx.error(Phases::PHASE_SYNTACTIC, lx.peek().view, STR_EXPR);
		} break;
	}

	while (is_operator(lx.peek().kind)) {
		switch (lx.peek().kind) {
			case Symbols::LSH:
			case Symbols::RSH:
			case Symbols::NOT: {
				compile_postfix(is, lx);
			} break;

			case Symbols::LSHN:
			case Symbols::RSHN:
			case Symbols::REPN:
			case Symbols::CAT:
			case Symbols::OR:
			case Symbols::AND:
			case Symbols::XOR: {
				compile_infix(is, lx);
			} break;

			default: {
				lx.error(Phases::PHASE_SYNTACTIC, lx.peek().view, STR_OPERATOR);
			} break;
		}
	}

	while (lx.peek().kind == Symbols::CHAIN)
		compile_chain(is, lx);
}

inline Instructions compile(Lexer& lx) {
	CANE_LOG(LOG_LEVEL_1, "compile");

	Instructions is;

	while (lx.peek().kind != Symbols::TERMINATOR)
		compile_expr(is, lx);

	return is;
}

}

#endif
