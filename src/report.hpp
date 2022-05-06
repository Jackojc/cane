#ifndef CANE_REPORT_HPP
#define CANE_REPORT_HPP

#include <util.hpp>
#include <view.hpp>
#include <print.hpp>
#include <log.hpp>


namespace cane {

	#define PHASES \
		X(INTERNAL,  internal) \
		X(ENCODING,  encoding) \
		X(LEXICAL,   lexical) \
		X(SYNTACTIC, syntactic) \
		X(SEMANTIC,  semantic)

		#define X(name, str) name,
			enum class Phases: int { PHASES };
		#undef X

		#define X(name, str) #str##_sv,
			constexpr View PHASE_TO_STRING[] = { PHASES };
		#undef X

		constexpr decltype(auto) phase2str(Phases p) {
			return PHASE_TO_STRING[(int)p];
		}

	#undef PHASES

	inline std::ostream& operator<<(std::ostream& os, Phases p) {
		return (os << phase2str(p));
	}


	#define REPORTS \
		X(ERROR,   error,   CANE_ANSI_FG_RED) \
		X(WARNING, warning, CANE_ANSI_FG_BLUE) \
		X(NOTICE,  notice,  CANE_ANSI_FG_YELLOW)

		#define X(name, str, colour) name,
			enum class Reports: int { REPORTS };
		#undef X

		#define X(name, str, colour) #str##_sv,
			constexpr View REPORT_TO_STRING[] = { REPORTS };
		#undef X

		#define X(name, str, colour) colour,
			constexpr View REPORT_TO_COLOUR[] = { REPORTS };
		#undef X

		constexpr decltype(auto) report2str(Reports r) {
			return REPORT_TO_STRING[(int)r];
		}

		constexpr decltype(auto) report2colour(Reports r) {
			return REPORT_TO_COLOUR[(int)r];
		}

	#undef REPORTS

	inline std::ostream& operator<<(std::ostream& os, Reports r) {
		return (os << report2str(r));
	}


	template <Reports R = Reports::ERROR, typename... Ts>
	inline std::ostream& general_report(std::ostream& os, Ts&&... args) {
		auto highlight = report2colour(R);
		outfmt(os, "{}" CANE_ANSI_BOLD "{}" CANE_ANSI_RESET " =>" CANE_ANSI_RESET " ", highlight, report2str(R));
		return outlnfmt(os, std::forward<Ts>(args)...);
	}


	template <Reports R = Reports::ERROR, typename... Ts>
	inline std::ostream& report(
		std::ostream& os,
		Phases phase,
		const View src,
		View sv,
		Ts&&... args
	) {
		const auto focused_line = extend_to_line(src, sv);

		const auto before = cane::before(focused_line, sv);
		const auto after = cane::after(focused_line, sv);

		const auto column_n = utf_length(before) + 1;
		const auto line_n = count_lines(overlap(src, sv)) + 1;

		const auto digits = count_digits(line_n);

		auto highlight = report2colour(R);

		const auto padding = [&] {
			for (size_t i = 0; i < digits + 1; i++)
				out(os, " ");
		};

		outfmt(
			os,
			"{}" CANE_ANSI_BOLD "{} {}: " CANE_ANSI_RESET "{}:{} " CANE_ANSI_BOLD "=>" CANE_ANSI_RESET " ",
			highlight, phase2str(phase), report2str(R), line_n, column_n
		);

		// Overview.
		outlnfmt(os, std::forward<Ts>(args)...);

		padding();
		outln(os);

		padding();
		outlnfmt(os, " " CANE_ANSI_FG_CYAN "|" CANE_ANSI_RESET);

		outfmt(os, " " CANE_ANSI_FG_CYAN "{}" CANE_ANSI_RESET " " CANE_ANSI_FG_CYAN "|" CANE_ANSI_RESET " ", line_n);

		if (sv.is_eof())
			outlnfmt(os, "{}EOF{}", highlight, CANE_ANSI_RESET);
		else
			outlnfmt(os, "{}{}{}{}{}", before, highlight, sv, CANE_ANSI_RESET, after);

		padding();
		outlnfmt(os, " {}|{}\n", CANE_ANSI_FG_CYAN, CANE_ANSI_RESET);

		return os;
	}

}

#endif
