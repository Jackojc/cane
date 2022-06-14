#ifndef CANE_COLOUR_HPP
#define CANE_COLOUR_HPP

namespace cane {

	#ifndef NDEBUG  // Debug
		#define CANE_ENABLE_ASSERT
	#else  // Release
		#define CANE_DISABLE_ASSERT
	#endif

	#define CANE_ANSI_RESET  "\x1b[0m"
	#define CANE_ANSI_BOLD   "\x1b[1m"

	#define CANE_ANSI_FG_BLACK    "\x1b[30m"
	#define CANE_ANSI_FG_RED      "\x1b[31m"
	#define CANE_ANSI_FG_GREEN    "\x1b[32m"
	#define CANE_ANSI_FG_YELLOW   "\x1b[33m"
	#define CANE_ANSI_FG_BLUE     "\x1b[34m"
	#define CANE_ANSI_FG_MAGENTA  "\x1b[35m"
	#define CANE_ANSI_FG_CYAN     "\x1b[36m"
	#define CANE_ANSI_FG_WHITE    "\x1b[37m"

	#define CANE_ANSI_FG_BRIGHT_BLACK    "\x1b[30;1m"
	#define CANE_ANSI_FG_BRIGHT_RED      "\x1b[31;1m"
	#define CANE_ANSI_FG_BRIGHT_GREEN    "\x1b[32;1m"
	#define CANE_ANSI_FG_BRIGHT_YELLOW   "\x1b[33;1m"
	#define CANE_ANSI_FG_BRIGHT_BLUE     "\x1b[34;1m"
	#define CANE_ANSI_FG_BRIGHT_MAGENTA  "\x1b[35;1m"
	#define CANE_ANSI_FG_BRIGHT_CYAN     "\x1b[36;1m"
	#define CANE_ANSI_FG_BRIGHT_WHITE    "\x1b[37;1m"

	#define CANE_ANSI_BG_BLACK    "\x1b[40m"
	#define CANE_ANSI_BG_RED      "\x1b[41m"
	#define CANE_ANSI_BG_GREEN    "\x1b[42m"
	#define CANE_ANSI_BG_YELLOW   "\x1b[43m"
	#define CANE_ANSI_BG_BLUE     "\x1b[44m"
	#define CANE_ANSI_BG_MAGENTA  "\x1b[45m"
	#define CANE_ANSI_BG_CYAN     "\x1b[46m"
	#define CANE_ANSI_BG_WHITE    "\x1b[47m"

	#define CANE_ANSI_BG_BRIGHT_BLACK    "\x1b[40;1m"
	#define CANE_ANSI_BG_BRIGHT_RED      "\x1b[41;1m"
	#define CANE_ANSI_BG_BRIGHT_GREEN    "\x1b[42;1m"
	#define CANE_ANSI_BG_BRIGHT_YELLOW   "\x1b[43;1m"
	#define CANE_ANSI_BG_BRIGHT_BLUE     "\x1b[44;1m"
	#define CANE_ANSI_BG_BRIGHT_MAGENTA  "\x1b[45;1m"
	#define CANE_ANSI_BG_BRIGHT_CYAN     "\x1b[46;1m"
	#define CANE_ANSI_BG_BRIGHT_WHITE    "\x1b[47;1m"

	enum {
		ANSI_RESET,
		ANSI_BOLD,

		ANSI_FG_BLACK,
		ANSI_FG_RED,
		ANSI_FG_GREEN,
		ANSI_FG_YELLOW,
		ANSI_FG_BLUE,
		ANSI_FG_MAGENTA,
		ANSI_FG_CYAN,
		ANSI_FG_WHITE,

		ANSI_FG_BRIGHT_BLACK,
		ANSI_FG_BRIGHT_RED,
		ANSI_FG_BRIGHT_GREEN,
		ANSI_FG_BRIGHT_YELLOW,
		ANSI_FG_BRIGHT_BLUE,
		ANSI_FG_BRIGHT_MAGENTA,
		ANSI_FG_BRIGHT_CYAN,
		ANSI_FG_BRIGHT_WHITE,

		ANSI_BG_BLACK,
		ANSI_BG_RED,
		ANSI_BG_GREEN,
		ANSI_BG_YELLOW,
		ANSI_BG_BLUE,
		ANSI_BG_MAGENTA,
		ANSI_BG_CYAN,
		ANSI_BG_WHITE,

		ANSI_BG_BRIGHT_BLACK,
		ANSI_BG_BRIGHT_RED,
		ANSI_BG_BRIGHT_GREEN,
		ANSI_BG_BRIGHT_YELLOW,
		ANSI_BG_BRIGHT_BLUE,
		ANSI_BG_BRIGHT_MAGENTA,
		ANSI_BG_BRIGHT_CYAN,
		ANSI_BG_BRIGHT_WHITE,

		ANSI_TOTAL,
	};

	#define CANE_INFO_STYLE  CANE_ANSI_RESET     "[-]"
	#define CANE_WARN_STYLE  CANE_ANSI_FG_BLUE   "[*]"
	#define CANE_ERR_STYLE   CANE_ANSI_FG_RED    "[!]"
	#define CANE_SUCC_STYLE  CANE_ANSI_FG_GREEN  "[^]"

	enum {
		LOG_INFO,
		LOG_WARN,
		LOG_ERR,
		LOG_SUCC,
	};

	namespace detail {
		constexpr auto lvl_to_style(size_t lvl) {
			switch (lvl) {
				case LOG_INFO: return CANE_INFO_STYLE;
				case LOG_WARN: return CANE_WARN_STYLE;
				case LOG_ERR:  return CANE_ERR_STYLE;
				case LOG_SUCC: return CANE_SUCC_STYLE;
			}

			return "";
		}
	}

	#ifndef CANE_DISABLE_ASSERT
		#define CANE_DEBUG_RUN(expr) \
			do { \
				( (expr) ); \
			} while (0)

		// We call this function in order to evaluate `expr` only once in case
		// it has side effects. If we used a macro for this, it would be evaluated
		// twice. Once for printing the result and once for returning it.
		namespace detail {
			template <typename T>
			inline decltype(auto) print_debug_impl(const char* file, const char* line, const char* expr_s, T&& expr) {
				cane::errlnfmt("[{}:{}] {} = {}", file, line, expr_s, std::forward<T>(expr));
				return std::forward<T>(expr);
			}
		}

		#define CANE_DEBUG(expr) \
			( cane::detail::print_debug_impl(__FILE__, CANE_STR(__LINE__), CANE_STR(expr), (expr)) )

	#else
		#define CANE_DEBUG_RUN(expr) do {} while (0)
		#define CANE_DEBUG(expr) ( (expr) )
	#endif

	#define CANE_LOG(...) \
		do { [CANE_VAR(fn_name) = __func__] (size_t CANE_VAR(lvl), auto&&... CANE_VAR(args)) { \
			CANE_DEBUG_RUN(( cane::err(CANE_TRACE, cane::detail::lvl_to_style(CANE_VAR(lvl)), " ") )); \
			CANE_DEBUG_RUN(( cane::err("`", CANE_VAR(fn_name), "`") )); \
			\
			if constexpr(sizeof...(CANE_VAR(args)) > 0) { \
				CANE_DEBUG_RUN( (cane::err(" => ")) ); \
				CANE_DEBUG_RUN( (cane::errfmt(std::forward<decltype(CANE_VAR(args))>(CANE_VAR(args))...)) ); \
			} \
			CANE_DEBUG_RUN(( cane::errln( CANE_ANSI_RESET ) )); \
		} ( __VA_ARGS__ ); } while (0)
}

#endif
