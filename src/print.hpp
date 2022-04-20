#ifndef CANE_PRINT_HPP
#define CANE_PRINT_HPP

#include <iostream>
#include <utility>
#include <cstring>

#include <view.hpp>

namespace cane {

	// generic stream
	template <typename... Ts> inline std::ostream& out(std::ostream& os, Ts&&... args) {
		return ((os << std::forward<Ts>(args)), ...);
	}

	template <typename... Ts> inline std::ostream& outln(std::ostream& os, Ts&&... args) {
		return ((os << std::forward<Ts>(args)), ..., (os << '\n'));
	}


	// stdout
	template <typename... Ts> inline std::ostream& print(Ts&&... args) {
		return (out(std::cout, std::forward<Ts>(args)), ...);
	}

	template <typename... Ts> inline std::ostream& println(Ts&&... args) {
		return (out(std::cout, std::forward<Ts>(args)), ..., out(std::cout, '\n'));
	}


	// stderr
	template <typename... Ts> inline std::ostream& err(Ts&&... args) {
		return (out(std::cerr, std::forward<Ts>(args)), ...);
	}

	template <typename... Ts> inline std::ostream& errln(Ts&&... args) {
		return (out(std::cerr, std::forward<Ts>(args)), ..., out(std::cerr, '\n'));
	}


	// Formatted output.
	// {} placeholders, escape with {{ or }}.
	namespace detail {
		template <typename T>
		inline std::ostream& outfmt(std::ostream& os, View& fmt, T&& arg) {
			View view {};

			bool have_left  = false;
			bool have_right = false;

			View chr = as_view(fmt);

			while (true) {
				if (fmt.is_eof())
					break;

				else if (eq_any(chr, "{", "}")) {
					const View cmp = chr;
					view = consume_view(fmt, chr, equal(cmp));

					// Check if we have any pairs and if we have any
					// valid placeholders.
					size_t has_single = (view.size() % 2) == 1;
					size_t pairs = view.size() / 2;

					have_left  = (has_single and cmp == "{"_sv) or have_left;
					have_right = (has_single and cmp == "}"_sv) or have_right;

					// If we have a pair, print the next variadic argument.
					if (have_left and have_right)
						out(os, std::forward<T>(arg));

					// Print pairs which escape placeholders.
					for (size_t i = 0; i < pairs; ++i)
						out(os, cmp);
				}

				else {
					view = consume_view(fmt, chr, partial_eq_none("{", "}"));
					out(os, view);
				}

				// If we have printed one of the variadic arguments, break so we
				// can consume the next variadic argument. Otherwise we would
				// print the same argument for every placeholder.
				if (have_left and have_right)
					break;
			}

			return os;
		}
	}

	inline std::ostream& outfmt(std::ostream& os, View fmt) {
		return out(os, fmt);
	}

	template <typename T, typename... Ts>
	inline std::ostream& outfmt(std::ostream& os, View fmt, T&& first, Ts&&... args) {
		detail::outfmt(os, fmt, std::forward<T>(first));
		return outfmt(os, fmt, std::forward<Ts>(args)...);
	}

	template <typename... Ts>
	inline std::ostream& outlnfmt(std::ostream& os, View fmt, Ts&&... args) {
		return (outfmt(os, fmt, std::forward<Ts>(args)...) << '\n');
	}


	// Formatted print variants.
	template <typename... Ts> inline std::ostream& printfmt(View fmt, Ts&&... args) {
		return outfmt(std::cout, fmt, std::forward<Ts>(args)...);
	}

	template <typename... Ts> inline std::ostream& printlnfmt(View fmt, Ts&&... args) {
		return outlnfmt(std::cout, fmt, std::forward<Ts>(args)...);
	}


	template <typename... Ts> inline std::ostream& errfmt(View fmt, Ts&&... args) {
		return outfmt(std::cerr, fmt, std::forward<Ts>(args)...);
	}

	template <typename... Ts> inline std::ostream& errlnfmt(View fmt, Ts&&... args) {
		return outlnfmt(std::cerr, fmt, std::forward<Ts>(args)...);
	}

}

#endif

