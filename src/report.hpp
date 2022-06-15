#ifndef CANE_REPORT_HPP
#define CANE_REPORT_HPP

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

	struct Error {};

	template <Reports R = Reports::ERROR, typename... Ts>
	inline std::ostream& report(
		std::ostream& os,
		Phases phase,
		const View src,
		View sv,
		Ts&&... args
	) {
		if (not overlapping_intervals(src.begin, src.end, sv.begin, sv.end))
			return report(os, Phases::INTERNAL, ""_sv, ""_sv, "`sv` does not exist in the range of `src`"_sv);

		const auto focused_line = extend_to_line(src, sv);

		const auto before = cane::before(focused_line, sv);
		const auto after = cane::after(focused_line, sv);

		const auto column_n = length(before) + 1;
		const auto line_n = count_lines(encompass(src, sv)) + 1;

		auto highlight = report2colour(R);

		outfmt(
			os,
			"{}" CANE_ANSI_BOLD "{} {}: " CANE_ANSI_RESET "{}:{} " CANE_ANSI_BOLD "=>" CANE_ANSI_RESET " ",
			highlight, phase2str(phase), report2str(R), line_n, column_n
		);

		// Overview.
		outlnfmt(os, std::forward<Ts>(args)...);

		outln(os);

		if (sv.is_eof())
			outlnfmt(os, "{}EOF{}"_sv, highlight, CANE_ANSI_RESET);

		else {
			out(os, "  " CANE_ANSI_FG_CYAN "|" CANE_ANSI_RESET "  ", before, highlight);

			while (not sv.is_eof()) {
				View line = cane::consume(sv, not_equal("\n"_sv));

				if (not sv.is_eof()) {
					View chr = take(sv);

					outln(os, line);
					out(os, "  " CANE_ANSI_FG_CYAN "|" CANE_ANSI_RESET "  ", highlight);
				}

				else
					outln(os, line, CANE_ANSI_RESET, after);
			}
		}

		return outln(os);
	}

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

}

#endif
