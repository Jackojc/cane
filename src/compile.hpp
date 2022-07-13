#ifndef CANE_COMPILE_HPP
#define CANE_COMPILE_HPP

namespace cane {

constexpr decltype(auto) is(Symbols kind) {
	return [=] (Token other) { return kind == other.kind; };
}

[[nodiscard]] inline double   literal_expr  (Context&, Lexer&, View, size_t);
[[nodiscard]] inline Sequence sequence_expr (Context&, Lexer&, View, size_t);

constexpr bool is_literal(Token x) {
	return cmp_any(x.kind,
		Symbols::INT);
}

constexpr bool is_step(Token x) {
	return cmp_any(x.kind,
		Symbols::SKIP,
		Symbols::BEAT);
}

constexpr bool is_literal_prefix(Token x) {
	return cmp_any(x.kind,
		Symbols::LEN_OF,
		Symbols::BEAT_OF,
		Symbols::SKIP_OF);
}

constexpr bool is_literal_infix(Token x) {
	return cmp_any(x.kind,
		Symbols::ADD,
		Symbols::SUB,
		Symbols::MUL,
		Symbols::DIV);
}

constexpr bool is_sequence_prefix(Token x) {
	return cmp_any(x.kind,
		Symbols::INVERT,
		Symbols::REV);
}

constexpr bool is_sequence_postfix(Token x) {
	return cmp_any(x.kind,
		Symbols::CAR,
		Symbols::CDR,
		Symbols::DBG);
}

constexpr bool is_sequence_infix(Token x) {
	return cmp_any(x.kind,
		Symbols::OR,
		Symbols::AND,
		Symbols::XOR,
		Symbols::CAT,
		Symbols::ROTL,
		Symbols::ROTR,
		Symbols::REP,
		Symbols::BPM,
		Symbols::MAP,
		Symbols::VEL,
		Symbols::CHAIN);
}

constexpr bool is_sequence_primary(Token x) {
	return cmp_any(x.kind,
		Symbols::IDENT,
		Symbols::LPAREN,
		Symbols::SEP) or
		is_literal(x) or
		is_step(x);
}

constexpr bool is_literal_primary(Token x) {
	return cmp_any(x.kind,
		Symbols::IDENT,
		Symbols::LPAREN,
		Symbols::GLOBAL_BPM,
		Symbols::GLOBAL_NOTE) or
		is_literal(x);
}

enum class OpFix {
	LIT_PREFIX,
	LIT_INFIX,
	SEQ_PREFIX,
	SEQ_INFIX,
	SEQ_POSTFIX,
};

inline std::pair<size_t, size_t> binding_power(Context& ctx, Lexer& lx, Token tok, OpFix fix) {
	auto [view, kind] = tok;

	enum { LEFT = 1, RIGHT = 0, };

	enum {
		DBG,
		CHAIN = DBG,
		MAP   = DBG,
		VEL   = DBG,
		CAT   = DBG,

		BPM,

		CAR,
		CDR = CAR,

		OR,
		AND  = OR,
		XOR  = OR,
		ROTL = OR,
		ROTR = OR,
		REP  = OR,

		REV,
		INVERT = REV,

		ADD,
		SUB = ADD,

		MUL,
		DIV = MUL,

		LEN_OF,
		BEAT_OF = LEN_OF,
		SKIP_OF = LEN_OF,
	};

	switch (fix) {
		case OpFix::LIT_PREFIX: switch (kind) {
			case Symbols::LEN_OF:  return { 0u, LEN_OF  + RIGHT };
			case Symbols::BEAT_OF: return { 0u, BEAT_OF + RIGHT };
			case Symbols::SKIP_OF: return { 0u, SKIP_OF + RIGHT };
			default: break;
		} break;

		case OpFix::LIT_INFIX: switch (kind) {
  			case Symbols::ADD: return { ADD, ADD + LEFT };
			case Symbols::SUB: return { SUB, SUB + LEFT };
			case Symbols::MUL: return { MUL, MUL + LEFT };
			case Symbols::DIV: return { DIV, DIV + LEFT };
			default: break;
		} break;

		case OpFix::SEQ_PREFIX: switch (kind) {
			case Symbols::REV:    return { 0u, REV    + RIGHT };
			case Symbols::INVERT: return { 0u, INVERT + RIGHT };
			default: break;
		} break;

		case OpFix::SEQ_INFIX: switch (kind) {
			case Symbols::MAP:   return { MAP,   MAP   + LEFT };
			case Symbols::VEL:   return { VEL,   VEL   + LEFT };
			case Symbols::CHAIN: return { CHAIN, CHAIN + LEFT };
			case Symbols::CAT:   return { CAT,   CAT   + LEFT };
			case Symbols::OR:    return { OR,    OR    + LEFT };
			case Symbols::AND:   return { AND,   AND   + LEFT };
			case Symbols::XOR:   return { XOR,   XOR   + LEFT };
			case Symbols::REP:   return { REP,   REP   + LEFT };
			case Symbols::ROTL:  return { ROTL,  ROTL  + LEFT };
			case Symbols::ROTR:  return { ROTR,  ROTR  + LEFT };
			case Symbols::BPM:   return { BPM,   BPM   + LEFT };
			default: break;
		} break;

		case OpFix::SEQ_POSTFIX: switch(kind) {
			case Symbols::DBG: return { DBG, DBG + LEFT };
			case Symbols::CAR: return { CAR, CAR + LEFT };
			case Symbols::CDR: return { CDR, CDR + LEFT };
			default: break;
		} break;
	}

	lx.error(ctx, Phases::INTERNAL, view, STR_UNREACHABLE, sym2str(kind));
}

inline double literal(Context& ctx, Lexer& lx, View lit_v) {
	CANE_LOG(LogLevel::INF);

	lx.expect(ctx, is_literal, lx.peek.view, STR_LITERAL);
	auto [view, kind] = lx.next();

	return b10_decode(view);
}

inline Sequence sequence(Context& ctx, Lexer& lx, View expr_v, Sequence seq) {
	CANE_LOG(LogLevel::INF);

	lx.expect(ctx, is_step, lx.peek.view, STR_STEP);

	while (is_step(lx.peek))
		seq.emplace_back(sym2step(lx.next().kind));

	return seq;
}

inline Sequence euclide(Context& ctx, Lexer& lx, View expr_v, Sequence seq) {
	CANE_LOG(LogLevel::INF);

	uint64_t steps = 0;
	uint64_t beats = 0;

	if (lx.peek.kind == Symbols::SEP) {
		lx.next();  // skip `:`
		beats = literal_expr(ctx, lx, lx.peek.view, 0);
	}

	else
		beats = literal(ctx, lx, lx.peek.view);

	lx.expect(ctx, is(Symbols::SEP), lx.peek.view, STR_EXPECT, sym2str(Symbols::SEP));
	lx.next();  // skip `:`

	steps = literal_expr(ctx, lx, lx.peek.view, 0);

	if (beats > steps)
		lx.error(ctx, Phases::SEMANTIC, encompass(expr_v, lx.prev.view), STR_LESSER_EQ, steps);

	for (size_t i = 0; i != static_cast<size_t>(steps); ++i)
		seq.emplace_back(((i * beats) % steps) < static_cast<size_t>(beats));

	if (seq.empty())
		lx.error(ctx, Phases::SEMANTIC, encompass(expr_v, lx.prev.view), STR_EMPTY);

	return seq;
}

inline double literal_const(Context& ctx, Lexer& lx, View lit_v) {
	CANE_LOG(LogLevel::INF);

	lx.expect(ctx, is(Symbols::IDENT), lx.peek.view, STR_IDENT);
	auto [view, kind] = lx.next();

	if (auto it = ctx.constants.find(view); it != ctx.constants.end())
		return it->second;

	lx.error(ctx, Phases::SEMANTIC, view, STR_UNDEFINED, view);
}

inline Sequence sequence_const(Context& ctx, Lexer& lx, View expr_v) {
	CANE_LOG(LogLevel::INF);

	lx.expect(ctx, is(Symbols::IDENT), lx.peek.view, STR_IDENT);
	auto [view, kind] = lx.next();

	if (auto it = ctx.chains.find(view); it != ctx.chains.end())
		return it->second;

	lx.error(ctx, Phases::SEMANTIC, view, STR_UNDEFINED, view);
}

inline double literal_primary(Context& ctx, Lexer& lx, View lit_v, double lit, size_t bp) {
	CANE_LOG(LogLevel::INF);

	Token tok = lx.peek;
	CANE_LOG(LogLevel::INF, sym2str(tok.kind));

	switch (tok.kind) {
		case Symbols::INT: {
			lit = literal(ctx, lx, lx.peek.view);
		} break;

		case Symbols::IDENT: {
			lit = literal_const(ctx, lx, lx.peek.view);
		} break;

		case Symbols::LPAREN: {
			lx.next();  // skip `(`

			lit = literal_expr(ctx, lx, lit_v, 0);  // Reset binding power.

			lx.expect(ctx, is(Symbols::RPAREN), lx.peek.view, STR_EXPECT, sym2str(Symbols::RPAREN));
			lx.next();  // skip `)`
		} break;

		default: { lx.error(ctx, Phases::SYNTACTIC, tok.view, STR_LIT_PRIMARY); } break;
	}

	return lit;
}

inline double literal_prefix(Context& ctx, Lexer& lx, View lit_v, double lit, size_t bp) {
	CANE_LOG(LogLevel::INF);

	Token tok = lx.next();
	CANE_LOG(LogLevel::INF, sym2str(tok.kind));

	switch (tok.kind) {
		case Symbols::LEN_OF:  { lit = sequence_len   (sequence_expr(ctx, lx, tok.view, bp)); } break;
		case Symbols::BEAT_OF: { lit = sequence_beats (sequence_expr(ctx, lx, tok.view, bp)); } break;
		case Symbols::SKIP_OF: { lit = sequence_skips (sequence_expr(ctx, lx, tok.view, bp)); } break;

		default: { lx.error(ctx, Phases::SYNTACTIC, tok.view, STR_LIT_OPERATOR); } break;
	}

	return lit;
}

inline double literal_infix(Context& ctx, Lexer& lx, View lit_v, double lit, size_t bp) {
	CANE_LOG(LogLevel::INF);

	Token tok = lx.next();
	CANE_LOG(LogLevel::INF, sym2str(tok.kind));

	switch (tok.kind) {
		case Symbols::ADD: { lit = lit + literal_expr(ctx, lx, lit_v, bp); } break;
		case Symbols::SUB: { lit = lit - literal_expr(ctx, lx, lit_v, bp); } break;
		case Symbols::MUL: { lit = lit * literal_expr(ctx, lx, lit_v, bp); } break;
		case Symbols::DIV: { lit = lit / literal_expr(ctx, lx, lit_v, bp); } break;

		default: { lx.error(ctx, Phases::SYNTACTIC, tok.view, STR_LIT_OPERATOR); } break;
	}

	return lit;
}

inline double literal_expr(Context& ctx, Lexer& lx, View lit_v, size_t bp) {
	CANE_LOG(LogLevel::WRN);

	double lit = 0;
	Token tok = lx.peek;

	if (is_literal_prefix(tok)) {
		auto [lbp, rbp] = binding_power(ctx, lx, tok, OpFix::LIT_PREFIX);
		lit = literal_prefix(ctx, lx, lit_v, lit, rbp);
	}

	else if (is_literal_primary(tok))
		lit = literal_primary(ctx, lx, lit_v, lit, 0);

	else
		lx.error(ctx, Phases::SYNTACTIC, tok.view, STR_LIT_PRIMARY);

	tok = lx.peek;

	while (is_literal_infix(tok)) {
		CANE_LOG(LogLevel::INF, sym2str(tok.kind));
		auto [lbp, rbp] = binding_power(ctx, lx, tok, OpFix::LIT_INFIX);

		if (lbp < bp)
			break;

		lit = literal_infix(ctx, lx, lit_v, lit, rbp);
		tok = lx.peek;
	}

	return lit;
}

inline Sequence sequence_primary(Context& ctx, Lexer& lx, View expr_v, Sequence seq, size_t bp) {
	CANE_LOG(LogLevel::INF);

	Token tok = lx.peek;
	CANE_LOG(LogLevel::INF, sym2str(tok.kind));

	switch (tok.kind) {
		case Symbols::INT:
		case Symbols::SEP: {
			seq = euclide(ctx, lx, lx.peek.view, std::move(seq));
		} break;

		case Symbols::SKIP:
		case Symbols::BEAT: {
			seq = sequence(ctx, lx, lx.peek.view, std::move(seq));
		} break;

		case Symbols::IDENT: {
			seq = sequence_const(ctx, lx, lx.peek.view);
		} break;

		case Symbols::LPAREN: {
			lx.next();  // skip `(`

			seq = sequence_expr(ctx, lx, expr_v, 0);  // Reset binding power.

			lx.expect(ctx, is(Symbols::RPAREN), lx.peek.view, STR_EXPECT, sym2str(Symbols::RPAREN));
			lx.next();  // skip `)`
		} break;

		default: { lx.error(ctx, Phases::SYNTACTIC, tok.view, STR_SEQ_PRIMARY); } break;
	}

	return seq;
}

inline Sequence sequence_prefix(Context& ctx, Lexer& lx, View expr_v, Sequence seq, size_t bp) {
	CANE_LOG(LogLevel::INF);

	Token tok = lx.next();
	CANE_LOG(LogLevel::INF, sym2str(tok.kind));

	switch (tok.kind) {
		case Symbols::REV:    { seq = sequence_reverse (sequence_expr(ctx, lx, expr_v, bp)); } break;
		case Symbols::INVERT: { seq = sequence_invert  (sequence_expr(ctx, lx, expr_v, bp)); } break;

		default: { lx.error(ctx, Phases::SYNTACTIC, tok.view, STR_SEQ_OPERATOR); } break;
	}

	return seq;
}

inline Sequence sequence_infix(Context& ctx, Lexer& lx, View expr_v, Sequence seq, size_t bp) {
	CANE_LOG(LogLevel::INF);

	Token tok = lx.next();  // skip operator.
	CANE_LOG(LogLevel::INF, sym2str(tok.kind));

	switch (tok.kind) {
		case Symbols::CAT: { seq = sequence_cat (std::move(seq), sequence_expr(ctx, lx, expr_v, bp)); } break;
		case Symbols::OR:  { seq = sequence_or  (std::move(seq), sequence_expr(ctx, lx, expr_v, bp)); } break;
		case Symbols::AND: { seq = sequence_and (std::move(seq), sequence_expr(ctx, lx, expr_v, bp)); } break;
		case Symbols::XOR: { seq = sequence_xor (std::move(seq), sequence_expr(ctx, lx, expr_v, bp)); } break;

		case Symbols::ROTL: { seq = sequence_rotl (std::move(seq), literal_expr(ctx, lx, tok.view, 0)); } break;
		case Symbols::ROTR: { seq = sequence_rotr (std::move(seq), literal_expr(ctx, lx, tok.view, 0)); } break;

		case Symbols::REP: {
			View before_v = lx.peek.view;
			uint64_t reps = literal_expr(ctx, lx, before_v, 0);

			// We don't want to shrink the sequence, it can only grow.
			if (reps == 0)
				lx.error(ctx, Phases::SEMANTIC, encompass(before_v, lx.prev.view), STR_GREATER, 0);

			seq = sequence_repeat(std::move(seq), reps);
		} break;

		case Symbols::BPM: {
			lx.expect(ctx, is_literal_primary, lx.peek.view, STR_LIT_EXPR);

			std::vector<Unit> durs;
			while (is_literal_primary(lx.peek)) {
				View before_v = lx.peek.view;
				uint64_t bpm = literal_expr(ctx, lx, before_v, 0);

				if (bpm < BPM_MIN)
					lx.error(ctx, Phases::SEMANTIC, encompass(before_v, lx.prev.view), STR_GREATER, BPM_MIN);

				durs.emplace_back(ONE_MIN / bpm);
			}

			size_t index = 0;
			for (auto& [dur, note, vel, kind]: seq) {
				dur = durs[index];
				index = (index + 1) % durs.size();
			}

			seq.flags |= SEQ_DURATION;
		} break;

		case Symbols::MAP: {
			lx.expect(ctx, is_literal_primary, lx.peek.view, STR_LIT_EXPR);

			std::vector<uint64_t> notes;
			while (is_literal_primary(lx.peek)) {
				View before_v = lx.peek.view;
				uint64_t note = literal_expr(ctx, lx, before_v, 0);

				if (note > NOTE_MAX or note < NOTE_MIN)
					lx.error(ctx, Phases::SEMANTIC, encompass(before_v, lx.prev.view), STR_BETWEEN, NOTE_MIN, NOTE_MAX);

				notes.emplace_back(note);
			}

			size_t index = 0;
			for (auto& [dur, note, vel, kind]: seq) {
				if (kind == BEAT) {
					note = notes[index];
					index = (index + 1) % notes.size();
				}
			}

			seq.flags |= SEQ_NOTE;
		} break;

		case Symbols::VEL: {
			lx.expect(ctx, is_literal_primary, lx.peek.view, STR_LIT_EXPR);

			std::vector<uint64_t> velocities;
			while (is_literal_primary(lx.peek)) {
				View before_v = lx.peek.view;
				uint64_t vel = literal_expr(ctx, lx, before_v, 0);

				if (vel > VELOCITY_MAX or vel < VELOCITY_MIN)
					lx.error(ctx, Phases::SEMANTIC, encompass(before_v, lx.prev.view), STR_BETWEEN, VELOCITY_MIN, VELOCITY_MAX);

				velocities.emplace_back(vel);
			}

			size_t index = 0;
			for (auto& [dur, note, vel, kind]: seq) {
				if (kind == BEAT) {
					vel = velocities[index];
					index = (index + 1) % velocities.size();
				}
			}
		} break;

		case Symbols::CHAIN: {
			lx.expect(ctx, is(Symbols::IDENT), lx.peek.view, STR_IDENT);
			auto [view, kind] = lx.next();

			// Assign or warn if re-assigned.
			if (auto [it, succ] = ctx.symbols.emplace(view); not succ)
				lx.error(ctx, Phases::SEMANTIC, view, STR_CONFLICT, view);

			if (auto [it, succ] = ctx.chains.try_emplace(view, seq); not succ)
	    		lx.error(ctx, Phases::SEMANTIC, view, STR_REDEFINED, view);
		} break;

		default: { lx.error(ctx, Phases::SYNTACTIC, tok.view, STR_SEQ_OPERATOR); } break;
	}

	return seq;
}

inline Sequence sequence_postfix(Context& ctx, Lexer& lx, View expr_v, Sequence seq, size_t bp) {
	CANE_LOG(LogLevel::INF);

	Token tok = lx.next();  // skip operator.
	CANE_LOG(LogLevel::INF, sym2str(tok.kind));

	switch (tok.kind) {
		case Symbols::CAR: { seq = sequence_car(std::move(seq)); } break;
		case Symbols::CDR: { seq = sequence_cdr(std::move(seq)); } break;

		case Symbols::DBG: {
			auto mini = sequence_minify(seq);
			size_t count = seq.size() / mini.size();

			Unit length = Unit::zero();

			for (auto& [dur, note, vel, kind]: seq)
				length += dur;

			size_t seconds = std::chrono::duration_cast<UnitSeconds>(length).count();

			lx.notice(ctx, Phases::SEMANTIC, encompass(expr_v, tok.view), STR_DEBUG, mini, count, seq.size(), seconds);
		} break;

		default: { lx.error(ctx, Phases::SYNTACTIC, tok.view, STR_SEQ_OPERATOR); } break;
	}

	return seq;
}

inline Sequence sequence_expr(Context& ctx, Lexer& lx, View expr_v, size_t bp) {
	CANE_LOG(LogLevel::WRN);

	Sequence seq {};
	Token tok = lx.peek;

	if (is_sequence_prefix(tok)) {
		auto [lbp, rbp] = binding_power(ctx, lx, tok, OpFix::SEQ_PREFIX);
		seq = sequence_prefix(ctx, lx, expr_v, std::move(seq), rbp);
	}

	else if (is_sequence_primary(tok))
		seq = sequence_primary(ctx, lx, expr_v, std::move(seq), 0);

	else
		lx.error(ctx, Phases::SYNTACTIC, tok.view, STR_SEQ_PRIMARY);

	tok = lx.peek;

	while (
		is_sequence_infix(tok) or
		is_sequence_postfix(tok)
	) {
		CANE_LOG(LogLevel::INF, sym2str(tok.kind));

		if (is_sequence_postfix(tok)) {
			auto [lbp, rbp] = binding_power(ctx, lx, tok, OpFix::SEQ_POSTFIX);
			if (lbp < bp)
				break;

			seq = sequence_postfix(ctx, lx, expr_v, std::move(seq), 0);
		}

		else if (is_sequence_infix(tok)) {
			auto [lbp, rbp] = binding_power(ctx, lx, tok, OpFix::SEQ_INFIX);
			if (lbp < bp)
				break;

			seq = sequence_infix(ctx, lx, expr_v, std::move(seq), rbp);
		}

		else
			lx.error(ctx, Phases::SYNTACTIC, tok.view, STR_SEQ_OPERATOR);

		tok = lx.peek;
	}

	return seq;
}

inline Timeline sequence_compile(Sequence seq, uint8_t chan, Unit time) {
	CANE_LOG(LogLevel::INF);

	Timeline tl {};

	auto ON = midi2int(Midi::NOTE_ON) | (chan - 1);
	auto OFF = midi2int(Midi::NOTE_OFF) | (chan - 1);

	for (auto& [dur, note, vel, kind]: seq) {
		if (kind == BEAT) {
			tl.emplace_back(time, ON, note, vel);
			tl.emplace_back(time + dur, OFF, note, vel);
		}

		time += dur;
	}

	tl.duration = time;

	return tl;
}

inline void timeline(Context& ctx, Lexer& lx, View stat_v, Unit orig) {
	Sequence seq = sequence_expr(ctx, lx, lx.peek.view, 0);

	if (lx.peek.kind == Symbols::SEND) {
		lx.next();  // skip `~>`

		View before_v = lx.peek.view;
		uint64_t chan = literal_expr(ctx, lx, before_v, 0);

		if (chan > CHANNEL_MAX or chan < CHANNEL_MIN)
			lx.error(ctx, Phases::SEMANTIC, encompass(before_v, lx.prev.view), STR_BETWEEN, CHANNEL_MIN, CHANNEL_MAX);


		if ((seq.flags & SEQ_NOTE) != SEQ_NOTE)
			lx.error(ctx, Phases::SEMANTIC, encompass(stat_v, lx.prev.view), STR_NO_NOTE);

		if ((seq.flags & SEQ_DURATION) != SEQ_DURATION)
			lx.error(ctx, Phases::SEMANTIC, encompass(stat_v, lx.prev.view), STR_NO_BPM);


		Timeline tl = sequence_compile(std::move(seq), chan, orig);

		ctx.time = std::max(tl.duration, ctx.time);
		ctx.tl.duration = std::max(tl.duration, ctx.tl.duration);

		ctx.tl.insert(ctx.tl.end(), tl.begin(), tl.end());

		while (lx.peek.kind == Symbols::WITH) {
			lx.next();  // skip `$`
			timeline(ctx, lx, lx.peek.view, orig);
		}
	}
}

inline void statement(Context& ctx, Lexer& lx, View stat_v) {
	CANE_LOG(LogLevel::WRN);

	Token tok = lx.peek;

	if (tok.kind == Symbols::LET) {
		CANE_LOG(LogLevel::INF, sym2str(Symbols::LET));
		lx.next();  // skip `let`

		lx.expect(ctx, is(Symbols::IDENT), lx.peek.view, STR_IDENT);
		while (lx.peek.kind == Symbols::IDENT) {
			auto [view, kind] = lx.next();  // get identifier
			double lit = literal_expr(ctx, lx, lx.peek.view, 0);

			// Assign or warn if re-assigned.
			if (auto [it, succ] = ctx.symbols.emplace(view); not succ)
				lx.error(ctx, Phases::SEMANTIC, view, STR_CONFLICT, view);

			if (auto [it, succ] = ctx.constants.try_emplace(view, lit); not succ)
				lx.error(ctx, Phases::SEMANTIC, view, STR_REDEFINED, view);
		}
	}

	else if (is_sequence_primary(tok) or is_sequence_prefix(tok))
		timeline(ctx, lx, stat_v, ctx.time);

	else
		lx.error(ctx, Phases::SYNTACTIC, tok.view, STR_STATEMENT);
}

inline Timeline compile(
	View src,
	Handler&& error_handler,
	Handler&& warning_handler,
	Handler&& notice_handler
) {
	CANE_LOG(LogLevel::WRN);

	Context ctx { std::move(error_handler), std::move(warning_handler), std::move(notice_handler) };
	Lexer lx { src, ctx };

	lx.next(); // important

	if (not cane::validate(src))
		lx.error(ctx, cane::Phases::ENCODING, src, cane::STR_ENCODING);

	while (lx.peek.kind != Symbols::TERMINATOR)
		statement(ctx, lx, lx.peek.view);

	Timeline tl = std::move(ctx.tl);

	if (tl.empty())
		return tl;

	// Active sensing
	Unit t = Unit::zero();
	while (t < tl.duration) {
		tl.emplace_back(t, midi2int(Midi::ACTIVE_SENSE), 0, 0);
		t += ACTIVE_SENSING_INTERVAL;
	}

	// Sort sequence by timestamps
	std::stable_sort(tl.begin(), tl.end(), [] (auto& a, auto& b) {
		return a.time < b.time;
	});

	return tl;
}

}

#endif
