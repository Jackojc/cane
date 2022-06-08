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

#include <constants.hpp>
#include <types.hpp>

namespace cane {

// Lexer
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

		else if (view == "!"_sv) { kind = Symbols::BEAT; src = cane::next(src); }
		else if (view == "."_sv) { kind = Symbols::SKIP; src = cane::next(src); }
		else if (view == ":"_sv) { kind = Symbols::SEP;  src = cane::next(src); }

		else if (view == "?"_sv) { kind = Symbols::DBG;    src = cane::next(src); }
		else if (view == "'"_sv) { kind = Symbols::REV;    src = cane::next(src); }
		else if (view == "~"_sv) { kind = Symbols::INVERT; src = cane::next(src); }
		else if (view == "|"_sv) { kind = Symbols::OR;     src = cane::next(src); }
		else if (view == "^"_sv) { kind = Symbols::XOR;    src = cane::next(src); }
		else if (view == ","_sv) { kind = Symbols::CAT;    src = cane::next(src); }
		else if (view == "$"_sv) { kind = Symbols::WITH;   src = cane::next(src); }

		else if (view == "+"_sv) { kind = Symbols::ADD; src = cane::next(src); }
		else if (view == "-"_sv) { kind = Symbols::SUB; src = cane::next(src); }
		else if (view == "/"_sv) { kind = Symbols::DIV; src = cane::next(src); }
		else if (view == "&"_sv) { kind = Symbols::AND; src = cane::next(src); }

		else if (view == "@"_sv) { kind = Symbols::BPM; src = cane::next(src); }

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

			if      (view == "map"_sv)   kind = Symbols::MAP;
			else if (view == "fit"_sv)   kind = Symbols::FIT;
			else if (view == "sync"_sv)  kind = Symbols::SYNC;
			else if (view == "send"_sv)  kind = Symbols::SEND;
			else if (view == "def"_sv)   kind = Symbols::DEF;
			else if (view == "loop"_sv)  kind = Symbols::LOOP;
			else if (view == "alias"_sv) kind = Symbols::ALIAS;
			else if (view == "len"_sv)   kind = Symbols::LEN_OF;
			else if (view == "beats"_sv) kind = Symbols::BEATS_OF;
			else if (view == "skips"_sv) kind = Symbols::SKIPS_OF;
			else if (view == "let"_sv)   kind = Symbols::LET;
			else if (view == "car"_sv)   kind = Symbols::CAR;
			else if (view == "cdr"_sv)   kind = Symbols::CDR;
			else if (view == "bpm"_sv)   kind = Symbols::GLOBAL_BPM;
			else if (view == "note"_sv)  kind = Symbols::GLOBAL_NOTE;
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

[[nodiscard]] inline Channel channel (Context&, Lexer&);

// Statements
inline void                   stat    (Context&, Lexer&, View);
[[nodiscard]] inline Sequence compile (Lexer&, size_t, size_t);


// Predicates
constexpr auto is_step = partial_eq_any(Symbols::BEAT, Symbols::SKIP);
constexpr auto is_lit = partial_eq_any(Symbols::INT, Symbols::HEX, Symbols::BIN);

constexpr auto is_lit_prefix = partial_eq_any(
	Symbols::ADD,
	Symbols::SUB,
	Symbols::LEN_OF,
	Symbols::BEATS_OF,
	Symbols::SKIPS_OF
);

constexpr auto is_lit_infix = partial_eq_any(
	Symbols::ADD,
	Symbols::SUB,
	Symbols::MUL,
	Symbols::DIV,
	Symbols::MOD
);

constexpr auto is_lit_primary = [] (auto x) {
	return is_lit(x) or is_lit_prefix(x) or eq_any(x,
		Symbols::IDENT
	);
};

constexpr auto is_lit_expr = [] (auto x) {
	return is_lit_prefix(x) or is_lit_primary(x);
};

constexpr auto is_seq_prefix = partial_eq_any(
	Symbols::INVERT,
	Symbols::REV,
	Symbols::SEND
);

constexpr auto is_seq_postfix = partial_eq_any(
	Symbols::CAR,
	Symbols::CDR,
	Symbols::DBG
);

constexpr auto is_seq_infix_expr = partial_eq_any(
	Symbols::OR,
	Symbols::AND,
	Symbols::XOR,
	Symbols::CAT,
	Symbols::WITH,
	Symbols::FIT,
	Symbols::SYNC
);

constexpr auto is_seq_infix_lit = partial_eq_any(
	Symbols::ROTLN,
	Symbols::ROTRN,
	Symbols::REPN,
	Symbols::MAP,
	Symbols::LOOP,
	Symbols::BPM
);

constexpr auto is_seq_infix = [] (auto x) {
	return is_seq_infix_expr(x) or is_seq_infix_lit(x);
};

constexpr auto is_seq_primary = [] (auto x) {
	return is_lit(x) or is_seq_prefix(x) or is_step(x) or eq_any(x,
		Symbols::IDENT,
		Symbols::LPAREN,
		Symbols::SEP
	);
};

constexpr auto is_seq_expr = [] (auto x) {
	return is_seq_prefix(x) or is_seq_primary(x);
};


// Precedence table
enum class OpFix {
	LIT_PREFIX,
	LIT_INFIX,

	SEQ_PREFIX,
	SEQ_INFIX,
	SEQ_POSTFIX,
};

inline std::pair<size_t, size_t> binding_power(Lexer& lx, Token tok, OpFix fix) {
	auto [view, kind] = tok;

	enum { LEFT = 1, RIGHT = 0, };

	enum {
		LOOP,
		SEND,

		MAP,
		BPM = MAP,

		DBG,

		CAR,
		CDR = CAR,

		CAT,
		WITH  = CAT,
		SYNC  = CAT,
		FIT   = CAT,
		OR    = CAT,
		AND   = CAT,
		XOR   = CAT,
		ROTLN = CAT,
		ROTRN = CAT,
		REPN  = CAT,

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
		BEATS_OF = LEN_OF,
		SKIPS_OF = LEN_OF,
	};

	switch (fix) {
		// Literals
		case OpFix::LIT_PREFIX: switch (kind) {
			case Symbols::ADD:      return { 0u, POS      + RIGHT };
			case Symbols::SUB:      return { 0u, NEG      + RIGHT };
			case Symbols::LEN_OF:   return { 0u, LEN_OF   + RIGHT };
			case Symbols::BEATS_OF: return { 0u, BEATS_OF + RIGHT };
			case Symbols::SKIPS_OF: return { 0u, SKIPS_OF + RIGHT };
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
			case Symbols::SEND:   return { 0u, SEND   + RIGHT };
			case Symbols::REV:    return { 0u, REV    + RIGHT };
			case Symbols::INVERT: return { 0u, INVERT + RIGHT };
			default: break;
		} break;

		case OpFix::SEQ_INFIX: switch (kind) {
			case Symbols::WITH: return { WITH,  WITH + LEFT };
			case Symbols::SYNC: return { SYNC,  SYNC + LEFT };
			case Symbols::FIT:  return { FIT,   FIT  + LEFT };
			case Symbols::CAT:  return { CAT,   CAT  + LEFT };
			case Symbols::OR:   return { OR,    OR   + LEFT };
			case Symbols::AND:  return { AND,   AND  + LEFT };
			case Symbols::XOR:  return { XOR,   XOR  + LEFT };

			case Symbols::REPN:  return { REPN,  REPN  + LEFT };
			case Symbols::ROTLN: return { ROTLN, ROTLN + LEFT };
			case Symbols::ROTRN: return { ROTRN, ROTRN + LEFT };
			case Symbols::MAP:   return { MAP,   MAP   + LEFT };
			case Symbols::LOOP:  return { LOOP,  LOOP  + LEFT };
			case Symbols::BPM:   return { BPM,   BPM   + LEFT };

			default: break;
		} break;

		case OpFix::SEQ_POSTFIX: switch(kind) {
			case Symbols::DBG:  return { DBG,  DBG  + LEFT };
			case Symbols::CAR:  return { CAR,  CAR  + LEFT };
			case Symbols::CDR:  return { CDR,  CDR  + LEFT };
			default: break;
		} break;
	}

	lx.error(Phases::INTERNAL, view, STR_UNREACHABLE);
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

	while (is_step(lx.peek().kind)) {
		auto [view, kind] = lx.next();

		if (kind == Symbols::BEAT)
			seq.emplace_back(Events::BEAT);

		else if (kind == Symbols::SKIP)
			seq.emplace_back(Events::SKIP);

		else
			lx.error(Phases::SYNTACTIC, view, STR_STEP);
	}

	if (seq.empty())
		lx.error(Phases::SEMANTIC, encompass(expr_v, lx.prev().view), STR_UNREACHABLE);

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

	for (size_t i = 0; i != static_cast<size_t>(steps); ++i) {
		size_t step = ((i * beats) % steps) < static_cast<size_t>(beats);

		if (step == 1)
			seq.emplace_back(Events::BEAT);

		else if (step == 0)
			seq.emplace_back(Events::SKIP);

		else
			lx.error(Phases::SYNTACTIC, encompass(expr_v, lx.prev().view), STR_UNREACHABLE);
	}

	if (seq.empty())
		lx.error(Phases::SEMANTIC, encompass(expr_v, lx.prev().view), STR_EMPTY);

	return seq;
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

// Literals
inline Literal lit_prefix(Context& ctx, Lexer& lx, View lit_v, size_t bp) {
	CANE_LOG(LOG_INFO);

	Literal lit {};
	Token tok = lx.peek();

	CANE_LOG(LOG_INFO, sym2str(tok.kind));

	if (is_lit_prefix(tok.kind)) {
		auto [lbp, rbp] = binding_power(lx, tok, OpFix::LIT_PREFIX);
		lx.next();  // skip operator;

		switch (tok.kind) {
			case Symbols::ADD: { lit = std::abs(lit); } break;
			case Symbols::SUB: { lit = -lit;          } break;

			case Symbols::LEN_OF: {
				lit = seq_expr(ctx, lx, tok.view, 0).size();
			} break;

			case Symbols::BEATS_OF: {
				auto seq = seq_expr(ctx, lx, tok.view, 0);
				lit = std::count_if(seq.begin(), seq.end(), [] (auto& x) {
					return x.kind == Events::BEAT;
				});
			} break;

			case Symbols::SKIPS_OF: {
				auto seq = seq_expr(ctx, lx, tok.view, 0);
				lit = std::count_if(seq.begin(), seq.end(), [] (auto& x) {
					return x.kind == Events::SKIP;
				});
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

	else if (tok.kind == Symbols::GLOBAL_BPM) {
		lx.next();  // skip `bpm`
		lit = ctx.global_bpm;
	}

	else if (tok.kind == Symbols::GLOBAL_NOTE) {
		lx.next();  // skip `note`
		lit = ctx.global_note;
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
inline Sequence seq_ref(Context& ctx, Lexer& lx, View expr_v) {
	CANE_LOG(LOG_INFO);

	lx.expect(equal(Symbols::IDENT), lx.peek().view, STR_IDENT);
	auto [view, kind] = lx.next();

	// Lookup symbol
	if (auto it = ctx.definitions.find(view); it != ctx.definitions.end())
		return it->second;

	lx.error(Phases::SEMANTIC, encompass(expr_v, view), STR_UNDEFINED, view);
}

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
			case Symbols::REV: {
				seq = seq_expr(ctx, lx, expr_v, rbp);
				std::reverse(seq.begin(), seq.end());
			} break;

			case Symbols::INVERT: {
				seq = seq_expr(ctx, lx, expr_v, rbp);
				std::transform(seq.begin(), seq.end(), seq.begin(), [] (auto& x) {
					if (x.kind == Events::BEAT)
						x.kind = Events::SKIP;

					else if (x.kind == Events::SKIP)
						x.kind = Events::BEAT;

					return x;
				});
			} break;

			case Symbols::SEND: {
				Channel chan = channel(ctx, lx);
				Sequence seq = seq_expr(ctx, lx, tok.view, 0);
				std::transform(seq.begin(), seq.end(), seq.begin(), [&] (auto& x) {
					x.channel = chan;
					return x;
				});
			} break;

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

		case Symbols::SEP: {
			lx.next();  // skip `:`
			seq = euclide(ctx, lx, lx.peek().view);
		} break;

		case Symbols::SKIP:
		case Symbols::BEAT: {
			seq = sequence(ctx, lx, lx.peek().view);
		} break;

		case Symbols::IDENT: {
			seq = seq_ref(ctx, lx, tok.view);
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
		// case Symbols::WITH: { lhs = with(std::move(lhs), seq_expr(ctx, lx, expr_v, bp)); } break;
		// case Symbols::SYNC: { lhs = with(std::move(lhs), seq_expr(ctx, lx, expr_v, bp)); } break;
		// case Symbols::FIT:  { lhs = with(std::move(lhs), seq_expr(ctx, lx, expr_v, bp)); } break;
		// case Symbols::CAT:  { lhs = cat(std::move(lhs), seq_expr(ctx, lx, expr_v, bp)); } break;
		// case Symbols::OR:   { lhs = disjunction(std::move(lhs), seq_expr(ctx, lx, expr_v, bp)); } break;
		// case Symbols::AND:  { lhs = conjunction(std::move(lhs), seq_expr(ctx, lx, expr_v, bp)); } break;
		// case Symbols::XOR:  { lhs = ex_disjunction(std::move(lhs), seq_expr(ctx, lx, expr_v, bp)); } break;

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
		// case Symbols::ROTLN: { seq = rotl(std::move(seq), lit_expr(ctx, lx, tok.view, 0)); } break;
		// case Symbols::ROTRN: { seq = rotr(std::move(seq), lit_expr(ctx, lx, tok.view, 0)); } break;

		case Symbols::REPN: {
			// View before_v = lx.peek().view;
			// Literal n = lit_expr(ctx, lx, before_v, 0);

			// // We don't want to shrink the sequence, it can only grow.
			// if (n == 0)
			// 	lx.error(Phases::SEMANTIC, encompass(before_v, lx.prev().view), STR_GREATER, 0);

			// seq = repeat(std::move(seq), n);
		} break;

		case Symbols::LOOP: {

		}

		case Symbols::MAP: {

		}

		case Symbols::BPM: {

		}

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
			// CANE_LOG(LOG_INFO, sym2str(Symbols::CAR));

			// auto it = seq.begin();

			// if (it == seq.end())
			// 	lx.error(Phases::INTERNAL, tok.view, STR_UNREACHABLE);

			// seq.insert(seq.begin(), *it);
			// seq.erase(seq.begin() + 1, seq.end());
		} break;

		case Symbols::CDR: {
			// CANE_LOG(LOG_INFO, sym2str(Symbols::CDR));

			// if (seq.empty())
			// 	lx.error(Phases::INTERNAL, tok.view, STR_UNREACHABLE);

			// if (seq.size() > 1)
			// 	seq.erase(seq.begin());
		} break;


		case Symbols::DBG: {
			// CANE_LOG(LOG_INFO, sym2str(Symbols::DBG));

			// auto mini = minify(seq);
			// size_t count = seq.size() / mini.size();

			// lx.notice(Phases::SEMANTIC, encompass(expr_v, tok.view), STR_DEBUG, mini, count, seq.size());
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

		Sequence seq = seq_expr(ctx, lx, lx.peek().view, 0);

		// Assign or warn if re-assigned.
		if (auto [it, succ] = ctx.symbols.emplace(view); not succ)
			lx.error(Phases::SEMANTIC, view, STR_CONFLICT, view);

		if (auto [it, succ] = ctx.definitions.try_emplace(view, seq); not succ)
			lx.error(Phases::SEMANTIC, view, STR_REDEFINED, view);
	}

	else if (is_seq_primary(tok.kind)) {
		Sequence seq = seq_expr(ctx, lx, lx.peek().view, 0);
		seq.time += ctx.time;

		ctx.seq.insert(ctx.seq.begin(), seq.begin(), seq.end());

		ctx.time += seq.duration;
		ctx.seq.duration += seq.duration;
	}

	else
		lx.error(Phases::SYNTACTIC, tok.view, STR_STATEMENT);
}

inline Sequence compile(Lexer& lx, size_t bpm, size_t note) {
	CANE_LOG(LOG_WARN);

	Context ctx;

	ctx.global_bpm  = bpm;
	ctx.global_note = note;

	// Compile
	while (lx.peek().kind != Symbols::TERMINATOR)
		statement(ctx, lx, lx.peek().view);

	return std::move(ctx.seq);
}

}

#endif
