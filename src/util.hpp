#ifndef CANE_UTIL_HPP
#define CANE_UTIL_HPP

#include <tuple>
#include <cstddef>

namespace cane {
	// Utility macros.
	// Stringify a macro def.
	// i.e. CANE_STR(__LINE__) => "42" as opposed to "__LINE__"
	#define CANE_STR_IMPL_(x) #x
	#define CANE_STR(x) CANE_STR_IMPL_(x)

	// Concatenate macro defs.
	// i.e. CANE_CAT(__FILE__, __LINE__) => "foo.c10" as opposed to "__FILE____LINE__"
	#define CANE_CAT_IMPL_(x, y) x##y
	#define CANE_CAT(x, y) CANE_CAT_IMPL_(x, y)

	// Create a uniquely named variable for use in a macro.
	#define CANE_VAR(x) CANE_CAT(var_, CANE_CAT(x, CANE_CAT(__LINE__, _)))

	// Evaluate expressions at beginning and ending of a scope.
	#define CANE_SCOPE(open, close) \
		for ( \
			size_t CANE_VAR(i) = ((open), 0); \
			!CANE_VAR(i); \
			(CANE_VAR(i)++), (close) \
		)

	// Evaluate expression at end of scope.
	#define CANE_DEFER(close) \
		for ( \
			size_t CANE_VAR(i) = 0; \
			!CANE_VAR(i); \
			(CANE_VAR(i)++), (close) \
		)


	// Print filename and line number `[foo.cpp:12]`
	#define CANE_TRACE "[" __FILE__ ":" CANE_STR(__LINE__) "] "


	// Detect platform.
	#if defined(_WIN64) || defined(_WIN32) || defined(__WINDOWS__)
		#define CANE_PLATFORM_WINDOWS
	#elif defined(__linux) || defined(linux)
		#define CANE_PLATFORM_LINUX
	#elif defined(__DragonFly__) || defined(__FreeBSD__) || defined(__NETBSD__) || defined(__OpenBSD__)
		#define CANE_PLATFORM_BSD
	#elif defined(__APPLE__) || defined(macintosh) || defined(__MACH__)
		#define CANE_PLATFORM_OSX
	#elif defined(__unix) || defined(unix)
		#define CANE_PLATFORM_UNIX
	#else
		#define CANE_PLATFORM_UNKNOWN
	#endif


	// Detect compiler.
	#if defined(__GNUC__)
		#define CANE_COMPILER_GCC
	#elif defined(__INTEL_COMPILER)
		#define CANE_COMPILER_INTEL
	#elif defined(__clang__)
		#define CANE_COMPILER_CLANG
	#elif defined(_MSC_VER)
		#define CANE_COMPILER_MSVC
	#else
		#define CANE_COMPILER_UNKNOWN
	#endif


	// Absolute difference between 2 pointers.
	template <typename T>
	constexpr size_t ptrdiff(const T a, const T b) {
		return
			((b - a) * (b > a)) +  // b > a => b - a
			((a - b) * (a > b));   // a > b => a - b
	}


	// Branchless ternary.
	template <typename T> constexpr auto condition(bool cond, T a, T b) {
		return (T)(cond * (size_t)a) + (!cond * (size_t)b);
	}


	// Branchless absolute function.
	template <typename T> constexpr auto abs(T v) {
		return v * ((v > 0) - (v < 0));
	}


	// Branchless min, max and clamp.
	template <typename T> constexpr auto min(T a, T b) {
		return condition(a < b, a, b);
	}

	template <typename T> constexpr auto max(T a, T b) {
		return condition(a > b, a, b);
	}

	template <typename T> constexpr T clamp(T x, T mn, T mx) {
		return max(mn, min(x, mx));
	}


	// Find the length of a null terminated string.
	// The reason for this function's existence is the fact
	// that the standard provided std::strlen is not constexpr.
	constexpr size_t strlen(const char* const str) {
		const char *s = str;

		while (*s)
			++s;

		return s - str;
	}


	// Swap lvalue with rvalue.
	template <typename T> constexpr T exchange(T& a, T&& b) {
		T tmp = move(a);
		a = std::forward<T>(b);
		return tmp;
	}


	// Count the number of digits in an integer (radix 10).
	template <typename T>
	constexpr size_t count_digits(T x) {
		size_t count = 0;

		// Loop at least once to handle `0`.
		do {
			x = x / 10;
			count++;
		} while (x != 0);

		return count;
	}


	// Check if any arguments are true.
	template <typename T, typename... Ts>
	constexpr bool any(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) or std::forward<Ts>(rest)) or ...);
	}

	// Check if all arguments are true.
	template <typename T, typename... Ts>
	constexpr bool all(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) and std::forward<Ts>(rest)) and ...);
	}

	// Check if all arguments are false.
	template <typename T, typename... Ts>
	constexpr bool none(T&& first, Ts&&... rest) {
		return ((not std::forward<T>(first) and not std::forward<Ts>(rest)) and ...);
	}


	// Check if all arguments are equal to first.
	template <typename T, typename... Ts>
	constexpr bool eq_all(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) == std::forward<Ts>(rest)) and ...);
	}

	// Check if any arguments are equal to first.
	template <typename T, typename... Ts>
	constexpr bool eq_any(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) == std::forward<Ts>(rest)) or ...);
	}

	// Check if none of the arguments are equal to first.
	template <typename T, typename... Ts>
	constexpr bool eq_none(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) != std::forward<Ts>(rest)) and ...);
	}


	// Higher order predicates.
	template <typename F> constexpr decltype(auto) negate(F&& fn) {
		return [fn = std::forward<F>(fn)] (const auto& x) {
			return not fn(x);
		};
	}

	template <typename T> constexpr decltype(auto) equal(T&& a) {
		return [a = std::forward<T>(a)] (const auto& b) {
			return b == a;
		};
	}

	template <typename T> constexpr decltype(auto) not_equal(T&& a) {
		return [a = std::forward<T>(a)] (const auto& b) {
			return b != a;
		};
	}

	template <typename T> constexpr decltype(auto) less(T&& a) {
		return [a = std::forward<T>(a)] (const auto& b) {
			return b < a;
		};
	}

	template <typename T> constexpr decltype(auto) less_equal(T&& a) {
		return [a = std::forward<T>(a)] (const auto& b) {
			return b <= a;
		};
	}

	template <typename T> constexpr decltype(auto) more(T&& a) {
		return [a = std::forward<T>(a)] (const auto& b) {
			return b > a;
		};
	}

	template <typename T> constexpr decltype(auto) more_equal(T&& a) {
		return [a = std::forward<T>(a)] (const auto& b) {
			return b >= a;
		};
	}

	template <typename... Ts>
	constexpr decltype(auto) partial_eq_any(Ts&&... args) {
		return [=] (const auto& x) {
			return eq_any(x, args...);
		};
	}

	template <typename... Ts>
	constexpr decltype(auto) partial_eq_all(Ts&&... args) {
		return [=] (const auto& x) {
			return eq_all(x, args...);
		};
	}

	template <typename... Ts>
	constexpr decltype(auto) partial_eq_none(Ts&&... args) {
		return [=] (const auto& x) {
			return eq_none(x, args...);
		};
	}


	// Count leading zeros.
	constexpr uint32_t countl_zero(uint32_t x) {
		#if defined(BR_COMPILER_CLANG) || defined(BR_COMPILER_GCC)
			return __builtin_clz(x);
		#else
			x = ~x;
			uint32_t count = 0;

			while (x & (1u << 31u)) {
				count++;
				x <<= 1;
			}

			return count;
		#endif
	}

	constexpr uint32_t countl_one(uint32_t x) {
		return countl_zero(~x);
	}


	// Check if an interval overlaps with another.
	template <typename T>
	constexpr bool overlapping_intervals(T a_begin, T a_end, T b_begin, T b_end) {
		return a_begin <= b_end and a_end >= b_begin;
	}


	// FNV-1a hash for a range of bytes.
	template <typename T = uint64_t>
	constexpr auto hash_bytes(const char* begin, const char* const end) {
		T offset_basis = 0;
		T prime = 0;

		if constexpr(sizeof(T) == sizeof(uint64_t)) {
			offset_basis = 14'695'981'039'346'656'037u;
			prime = 1'099'511'628'211u;
		}

		else if constexpr(sizeof(T) == sizeof(uint32_t)) {
			offset_basis = 2'166'136'261u;
			prime = 16'777'619u;
		}

		T hash = offset_basis;

		while (begin != end) {
			hash = (hash ^ static_cast<T>(*begin)) * prime;
			begin++;
		}

		return hash;
	}
}

#endif

