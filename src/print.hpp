#ifndef CANE_PRINT_HPP
#define CANE_PRINT_HPP

// I/O
namespace cane {
	template <typename... Ts>
	inline std::ostream& print(std::ostream& os, Ts&&... args) {
		return ((os << std::forward<Ts>(args)), ...);
	}

	template <typename... Ts>
	inline std::ostream& println(std::ostream& os, Ts&&... args) {
		return ((os << std::forward<Ts>(args)), ..., (os << '\n'));
	}

	namespace detail {
		template <typename T>
		inline std::ostream& fmt2(std::ostream& os, View& f, T&& arg, bool replace = true) {
			View before       = take_while(f, [] (View sv) { return sv != "%"_sv; });
			View placeholders = take_while(f, [] (View sv) { return sv == "%"_sv; });

			print(os, before);

			for (size_t i = 0; i != placeholders.size() / 2; ++i)
				print(os, "%");

			if (f.empty())
				return os;

			if (replace and placeholders.size() % 2 == 1)
				print(os, arg);

			if (not replace and placeholders.size() % 2 == 1)
				print(os, "%");

			return detail::fmt2(os, f, std::forward<T>(arg), replace);
		}

		inline std::ostream& fmt(std::ostream& os, View& f) {
			return detail::fmt2(os, f, 0, false);
		}

		template <typename T, typename... Ts>
		inline std::ostream& fmt(std::ostream& os, View& f, T&& arg, Ts&&... args) {
			detail::fmt2(os, f, std::forward<T>(arg));
			return fmt(os, f, std::forward<Ts>(args)...);
		}
	}

	template <typename... Ts>
	inline std::ostream& fmt(std::ostream& os, View f, Ts&&... args) {
		return detail::fmt(os, f, std::forward<Ts>(args)...);
	}

	template <typename... Ts>
	inline std::ostream& fmtln(std::ostream& os, View f, Ts&&... args) {
		return (fmt(os, f, std::forward<Ts>(args)...) << '\n');
	}
}

#endif

