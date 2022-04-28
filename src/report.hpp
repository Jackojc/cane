#ifndef CANE_REPORT_HPP
#define CANE_REPORT_HPP

#include <util.hpp>
#include <view.hpp>
#include <print.hpp>
#include <log.hpp>


namespace cane {

	#define PHASES \
		X(PHASE_ENCODING, encoding) \
		X(PHASE_LEXICAL, lexical) \
		X(PHASE_SYNTACTIC, syntactic) \
		X(PHASE_SEMANTIC, semantic)

		#define X(name, str) name,
			enum class Phases { PHASES };
		#undef X

		#define X(name, str) #str##_sv,
			constexpr View PHASE_TO_STRING[] = { PHASES };
		#undef X
	#undef PHASES

	inline std::ostream& operator<<(std::ostream& os, Phases k) {
		return (os << PHASE_TO_STRING[(int)k]);
	}


	enum class Report {
		ERROR,
		WARNING,
		NOTICE,
	};

	template <Report R = Report::ERROR, typename... Ts>
	inline std::ostream& report(
		std::ostream& os,
		Phases phase,
		const View src,
		View sv,
		Ts&&... args
	) {
		const bool ansi_enabled = true;

		const auto focused_line = extend_to_line(src, sv);
		const auto before = cane::before(focused_line, sv);
		const auto after = cane::after(focused_line, sv);

		const auto column_n = utf_length(before) + 1;
		const auto line_n = count_lines(overlap(src, sv)) + 1;

		const auto digits = count_digits(line_n);

		auto highlight = colour(ANSI_FG_RED,    ansi_enabled);
		auto yellow    = colour(ANSI_FG_YELLOW, ansi_enabled);
		auto cyan      = colour(ANSI_FG_CYAN,   ansi_enabled);
		auto bold      = colour(ANSI_BOLD,      ansi_enabled);
		auto reset     = colour(ANSI_RESET,     ansi_enabled);

		if (R == Report::WARNING)
			highlight = colour(ANSI_FG_BLUE, ansi_enabled);

		else if (R == Report::NOTICE)
			highlight = colour(ANSI_FG_YELLOW, ansi_enabled);

		const auto phase_name = PHASE_TO_STRING[(int)phase];

		const auto padding = [&] {
			for (size_t i = 0; i < digits + 1; i++)
				out(os, " ");
		};

		// Error type.
		if (R == Report::ERROR)
			outfmt(os, "{}{}{} error{}: ", highlight, bold, phase_name, reset);

		else if (R == Report::WARNING)
			outfmt(os, "{}{}{} warning{}: ", highlight, bold, phase_name, reset);

		else if (R == Report::NOTICE)
			outfmt(os, "{}{}{} notice{}: ", highlight, bold, phase_name, reset);

		// Overview.
		outlnfmt(os, std::forward<Ts>(args)...);

		// File, line and column.
		padding();
		outfmt(os, "{}-->{} <stdin>:", cyan, reset);

		if (not sv.is_eof()) {
			// Line and column.
			outlnfmt(os, "{}:{}", line_n, column_n);

			// Source snippet.
			padding();
			outlnfmt(os, " {}|{}", cyan, reset);

			outfmt(os, " {}{} |{} ", cyan, line_n, reset);
			outlnfmt(os, "{}{}{}{}{}", before, highlight, sv, reset, after);

			padding();
			outlnfmt(os, " {}|{}\n", cyan, reset);
		}

		// EOF
		else
			outln(os, "eof\n");

		return os;
	}

}

#endif
