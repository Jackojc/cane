#ifndef CANE_LEXER_HPP
#define CANE_LEXER_HPP

namespace cane {

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

		else if (view == "("_sv) { kind = Symbols::LPAREN; src = cane::next(src); }
		else if (view == ")"_sv) { kind = Symbols::RPAREN; src = cane::next(src); }

		else if (view == "!"_sv) { kind = Symbols::BEAT; src = cane::next(src); }
		else if (view == "."_sv) { kind = Symbols::SKIP; src = cane::next(src); }
		else if (view == ":"_sv) { kind = Symbols::SEP;  src = cane::next(src); }

		else if (view == "?"_sv) { kind = Symbols::DBG;    src = cane::next(src); }
		else if (view == "~"_sv) { kind = Symbols::INVERT; src = cane::next(src); }
		else if (view == "'"_sv) { kind = Symbols::REV;    src = cane::next(src); }
		else if (view == "|"_sv) { kind = Symbols::OR;     src = cane::next(src); }
		else if (view == "^"_sv) { kind = Symbols::XOR;    src = cane::next(src); }
		else if (view == ","_sv) { kind = Symbols::CAT;    src = cane::next(src); }

		else if (view == "+"_sv) { kind = Symbols::ADD; src = cane::next(src); }
		else if (view == "-"_sv) { kind = Symbols::SUB; src = cane::next(src); }
		else if (view == "/"_sv) { kind = Symbols::DIV; src = cane::next(src); }
		else if (view == "&"_sv) { kind = Symbols::AND; src = cane::next(src); }

		else if (view == "<"_sv) { kind = Symbols::ROTL; src = cane::next(src); }
		else if (view == ">"_sv) { kind = Symbols::ROTR; src = cane::next(src); }

		else if (view == "@"_sv) { kind = Symbols::BPM;  src = cane::next(src); }
		else if (view == "$"_sv) { kind = Symbols::WITH; src = cane::next(src); }

		else if (view == "*"_sv) {
			kind = Symbols::MUL;
			src = cane::next(src);

			if (cane::peek(src) == "*"_sv) {
				kind = Symbols::REP;
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
			kind = Symbols::INT;
			view = cane::consume_decode(src, cane::is_number);
		}

		else if (cane::is_letter(decode(cane::peek(src))) or view == "_"_sv) {
			kind = Symbols::IDENT;
			view = cane::consume_decode(src, [] (cp c) {
				return cane::is_alphanumeric(c) or c == '_';
			});

			if      (view == "map"_sv)   kind = Symbols::MAP;
			else if (view == "send"_sv)  kind = Symbols::SEND;
			else if (view == "alias"_sv) kind = Symbols::ALIAS;
			else if (view == "len"_sv)   kind = Symbols::LEN_OF;
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

}

#endif
