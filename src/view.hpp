#ifndef CANE_VIEW_HPP
#define CANE_VIEW_HPP

#include <iostream>

#include <cstdint>
#include <cstddef>

#include <util.hpp>
#include <unicode.hpp>

namespace cane {

	struct View {
		const char* begin = nullptr;
		const char* end   = nullptr;


		constexpr View() {}

		constexpr View(const char* ptr):
			begin(ptr), end(ptr + strlen(ptr)) {}

		constexpr View(const char* begin_, const char* end_):
			begin(begin_), end(end_) {}

		constexpr View(const char* begin_, size_t length):
			begin(begin_), end(begin_ + length) {}


		// Return size by getting the absolute difference between
		// begin and end pointers.
		constexpr size_t size() const {
			return ptrdiff(begin, end);
		}

		// If the size of the view is 0, it means the pointers
		// are equal and so we are at the end.
		constexpr bool is_eof() const {
			return begin == end;
		}


		// Check if View is initialised.
		constexpr bool is_null() const {
			return begin == nullptr or end == nullptr;
		}
	};


	// Check if 2 Views are equal.
	// We perform a series of checks ranging from least
	// expensive to most expensive.
	// 1. Compare pointers
	// 2. Compare lengths
	// 3. Compare characters
	constexpr bool operator==(View lhs, View rhs) {
		// Compare the pointers.
		if (lhs.begin == rhs.begin and lhs.end == rhs.end)
			return true;

		// Compare the length.
		if (lhs.size() != rhs.size())
			return false;

		// Compare every character.
		for (size_t i = 0; i < lhs.size(); i++) {
			if (*(lhs.begin + i) != *(rhs.begin + i))
				return false;
		}

		return true;
	}

	constexpr bool operator!=(View lhs, View rhs) {
		return not (lhs == rhs);
	}


	std::ostream& operator<<(std::ostream& os, View v) {
		os.write(v.begin, v.size());
		return os;
	}


	constexpr size_t utf_length(View);
	constexpr bool utf_validate(View);

	constexpr uint32_t as_char(View);
	constexpr uint8_t as_byte(View);
	constexpr View as_view(View);

	constexpr uint32_t char_at(View, size_t);
	constexpr uint8_t byte_at(View, size_t);
	constexpr View view_at(View, size_t);

	constexpr uint32_t char_front(View);
	constexpr uint8_t byte_front(View);
	constexpr View view_front(View);

	constexpr uint32_t char_back(View);
	constexpr uint8_t byte_back(View);
	constexpr View view_back(View);

	constexpr uint8_t utf_char_length(const char* const);
	constexpr uint32_t utf_char_decode(const char* const, size_t);

	[[nodiscard]] constexpr View iter_next_char(View, uint32_t&, size_t = 1);
	[[nodiscard]] constexpr View iter_next_byte(View, uint8_t&, size_t = 1);
	[[nodiscard]] constexpr View iter_next_view(View, View&, size_t = 1);

	[[nodiscard]] constexpr View iter_prev_char(View, uint32_t&, size_t = 1);
	[[nodiscard]] constexpr View iter_prev_byte(View, uint8_t&, size_t = 1);
	[[nodiscard]] constexpr View iter_prev_view(View, View&, size_t = 1);

	[[nodiscard]] constexpr const char* utf_char_next(const char* const);
	[[nodiscard]] constexpr const char* utf_char_prev(const char* const);

	[[nodiscard]] constexpr View next_char(View, size_t = 1);
	[[nodiscard]] constexpr View prev_char(View, size_t = 1);
	[[nodiscard]] constexpr View grow_char(View, size_t = 1);
	[[nodiscard]] constexpr View shrink_char(View, size_t = 1);

	[[nodiscard]] constexpr View next_byte(View, size_t = 1);
	[[nodiscard]] constexpr View prev_byte(View, size_t = 1);
	[[nodiscard]] constexpr View grow_byte(View, size_t = 1);
	[[nodiscard]] constexpr View shrink_byte(View, size_t = 1);

	template <typename F> constexpr View consume_view(View&, View&, const F&);
	template <typename F> constexpr View consume_char(View&, View&, const F&);
	template <typename F> constexpr View consume_byte(View&, View&, const F&);

	constexpr View lstrip(View);
	constexpr View rstrip(View);
	constexpr View strip(View);

	constexpr View overlap(View, View);

	constexpr View before(const View, View);
	constexpr View after(const View, View);

	template <typename F> constexpr size_t count(View, const F&);
	template <typename F> constexpr size_t count_consecutive(View, const F&);
	constexpr size_t count_lines(View);

	template <typename F> constexpr View split(View, const F&, size_t);
	constexpr View line(View, size_t);
	constexpr View word(View, size_t);

	template <typename T = size_t> constexpr T to_int(View);

	// Calculate the character size of a UTF-8 encoded string.
	// We iterate character by character until we reach the end
	// pointer.
	constexpr size_t utf_length(View sv) {
		size_t length = 0;

		for (; not sv.is_eof(); sv = next_char(sv))
			length++;

		return length;
	}


	// Validate UTF-8 string.
	// https://bjoern.hoehrmann.de/utf-8/decoder/dfa/
	constexpr uint8_t INTERNAL_UTF_TABLE__[] = {
		// The first part of the table maps bytes to character classes that
		// to reduce the size of the transition table and create bitmasks.
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,  9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
		7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
		8, 8, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
		10,3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 3, 3,  11,6, 6, 6, 5, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,

		// The second part is a transition table that maps a combination
		// of a state of the automaton and a character class to a state.
		0,  12, 24, 36, 60, 96, 84, 12, 12, 12, 48, 72,  12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
		12, 0,  12, 12, 12, 12, 12, 0,  12, 0,  12, 12,  12, 24, 12, 12, 12, 12, 12, 24, 12, 24, 12, 12,
		12, 12, 12, 12, 12, 12, 12, 24, 12, 12, 12, 12,  12, 24, 12, 12, 12, 12, 12, 12, 12, 24, 12, 12,
		12, 12, 12, 12, 12, 12, 12, 36, 12, 36, 12, 12,  12, 36, 12, 12, 12, 12, 12, 36, 12, 36, 12, 12,
		12, 36, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
	};

	#define CANE_UTF_VALID    0
	#define CANE_UTF_INVALID  1

	// Validate a UTF-8 encoded string.
	constexpr bool utf_validate(View sv) {
		uint32_t state = CANE_UTF_VALID;

		for (; not sv.is_eof(); sv = next_byte(sv)) {
			uint32_t type = INTERNAL_UTF_TABLE__[(uint8_t)as_byte(sv)];
			state = INTERNAL_UTF_TABLE__[256 + state + type];

			if (state == CANE_UTF_INVALID)
				break;
		}

		return state == CANE_UTF_VALID;
	}


	// Efficiently calculate the number of bytes in
	// a UTF-8 encoded codepoint.
	// We use the first byte to determine the
	// number of bytes in the codepoint.
	constexpr uint8_t utf_char_length(const char* const ptr) {
		// Cast to uint32_t and shift first byte into most
		// significant position then negate.
		// uint32_t u = ~(((uint32_t)ptr[0]) << 24);

		// Map result of countl_zero(u) to return value.
		// 0 -> 1 byte(s)
		// 2 -> 2 byte(s)
		// 3 -> 3 byte(s)
		// 4 -> 4 byte(s)
		// constexpr uint32_t out[] = {
		// 	1, 1, 2, 3, 4
		// };

		// return out[countl_zero(u)];

		return countl_one((((unsigned)((ptr[0] | 0b10000000) & ~0b01000000 ) | ((ptr[0] & 0b10000000) >> 1)) << 24));
	}

	// Return ptr advanced by one codepoint.
	constexpr const char* utf_char_next(const char* const ptr) {
		return ptr + utf_char_length(ptr);
	}

	// Return ptr reversed by one codepoint.
	constexpr const char* utf_char_prev(const char* ptr) {
		// Walk backwards one byte at a time while we
		// see continuation bytes.
		// 10xxxxxx = 0x80
		// 01xxxxxx = 0x40
		// We want the first bit to be set but _not_
		// the second bit.
		do
			--ptr;
		while ((*ptr & 0b1000'0000) and not (*ptr & 0b0100'0000));

		return ptr;
	}

	// Decode a codepoint and return it as a br_u32.
	constexpr uint32_t utf_char_decode(const char* const ptr, const size_t sz) {
		constexpr auto loop = [] (const char* const ptr, const size_t sz) {
			// The table below has a row for each of the 4 possible sizes of codepoint.
			// The first column is the size mask
			constexpr uint8_t masks[] = {
			//  v-------------------masks--------------------v     |    v----shifts----v
				0b11111111, 0b00000000, 0b00000000, 0b00000000, /* | */  0u,  0u, 0u, 0u, // 1 byte(s)
				0b00011111, 0b00111111, 0b00000000, 0b00000000, /* | */  6u,  0u, 0u, 0u, // 2 byte(s)
				0b00001111, 0b00111111, 0b00111111, 0b00000000, /* | */ 12u,  6u, 0u, 0u, // 3 byte(s)
				0b00000111, 0b00111111, 0b00111111, 0b00111111, /* | */ 18u, 12u, 6u, 0u, // 4 byte(s)
			};

			constexpr size_t max_codepoint_sz = 4u;
			const auto row = ((max_codepoint_sz * 2u) * (sz - 1u)); // Row in the table above to use.

			uint32_t out = 0;

			// Loop through bytes in codepoint and perform 3 operations:
			// 1. We mask out either the starting bytes size specifier _or_ the continuation marker
			// 2. We shift the byte left by `i * 6u`
			// 3. We OR it with `out` to insert the byte in the correct location
			for (size_t i = 0; i != sz; i++)
				out |= (ptr[i] & masks[row + i]) << masks[row + 4u + i];

			return out;
		};

		switch (sz) {
			case 2: return loop(ptr, 2);
			case 3: return loop(ptr, 3);
			case 4: return loop(ptr, 4);
		}

		return loop(ptr, 1);
	}


	// Character iteration.
	constexpr View next_char(View sv, size_t n) {
		if (n == 0)
			return sv;

		// Loop to advance n characters.
		auto ptr = sv.begin;

		while (n--)
			ptr = utf_char_next(ptr);

		// Make sure we don't walk forwards beyond the end pointer.
		ptr = min(ptr, sv.end);

		return { ptr, sv.end };
	}

	constexpr View prev_char(View sv, size_t n) {
		if (n == 0)
			return sv;

		auto ptr = sv.begin;

		while (n--)
			ptr = utf_char_prev(ptr);

		return { ptr, sv.end };
	}

	constexpr View grow_char(View sv, size_t n) {
		if (n == 0)
			return sv;

		auto ptr = sv.end;

		while (n--)
			ptr = utf_char_next(ptr);

		return { sv.begin, ptr };
	}

	constexpr View shrink_char(View sv, size_t n) {
		if (n == 0)
			return sv;

		auto ptr = sv.end;

		while (n--)
			ptr = utf_char_prev(ptr);

		// Make sure we don't walk backwards beyond the beginning pointer.
		ptr = max(ptr, sv.begin);

		return { sv.begin, ptr };
	}


	// Byte iteration.
	constexpr View next_byte(View sv, size_t n) {
		return { sv.begin + n, sv.end };
	}

	constexpr View prev_byte(View sv, size_t n) {
		return { sv.begin - n, sv.end };
	}

	constexpr View grow_byte(View sv, size_t n) {
		return { sv.begin, sv.end + n };
	}

	constexpr View shrink_byte(View sv, size_t n) {
		return { sv.begin, sv.end - n };
	}


	// Mutable iteration. (decode and skip forward together)
	constexpr View iter_next_char(View sv, uint32_t& c, size_t i) {
		// Skip i-1 chars. If i is zero, we add 1 so we dont overflow.
		sv = next_char(sv, i - (i != 0));

		// Get current codepoint length and then decode.
		const auto len = utf_char_length(sv.begin);
		c = utf_char_decode(sv.begin, len);

		// Advance pointer.
		sv.begin += len;

		return sv;
	}

	constexpr View iter_next_view(View sv, View& c, size_t i) {
		// Skip i-1 chars. If i is zero, we add 1 so we dont overflow.
		sv = next_char(sv, i - (i != 0));

		// Get current codepoint length.
		const auto len = utf_char_length(sv.begin);

		// Assign before we increment pointer so
		// we get the current codepoint.
		c = { sv.begin, sv.begin + len };

		// Advance pointer.
		sv.begin += len;

		return sv;
	}

	constexpr View iter_next_byte(View sv, uint8_t& c, size_t i) {
		sv = next_byte(sv, i);
		c = as_byte(sv);
		return sv;
	}


	// Mutable iteration (previous)
	// These versions don't really need to exist because
	// walking backwards doesn't require codepoint length
	// calculation but they do exist for consistancies sake.
	constexpr View iter_prev_char(View sv, uint32_t& c, size_t i) {
		sv = prev_char(sv, i);
		c = as_char(sv);
		return sv;
	}

	constexpr View iter_prev_view(View sv, View& c, size_t i) {
		sv = prev_char(sv, i);
		c = as_view(sv);
		return sv;
	}

	constexpr View iter_prev_byte(View sv, uint8_t& c, size_t i) {
		sv = prev_byte(sv, i);
		c = as_byte(sv);
		return sv;
	}


	// Return a UTF-8 codepoint decoded to a u32 integer.
	constexpr uint32_t as_char(View sv) {
		return utf_char_decode(sv.begin, utf_char_length(sv.begin));
	}

	// Just derefence the begin pointer.
	constexpr uint8_t as_byte(View sv) {
		return *sv.begin;
	}

	// This function returns a view of a character (2 pointers)
	// rather than decoding to an integer. This can be useful
	// if you don't care what the character is and don't want to
	// pay the cost of decoding.
	constexpr View as_view(View sv) {
		return { sv.begin, sv.begin + utf_char_length(sv.begin) };
	}


	// Indexing.
	constexpr uint8_t byte_at(View sv, size_t i) {
		sv = next_byte(sv, i);
		return as_byte(sv);
	}

	constexpr uint32_t char_at(View sv, size_t i) {
		sv = next_char(sv, i);
		return as_char(sv);
	}

	constexpr View view_at(View sv, size_t i) {
		sv = next_char(sv, i);
		return as_view(sv);
	}


	// Front char.
	constexpr uint8_t byte_front(View sv) {
		return as_byte(sv);
	}

	constexpr uint32_t char_front(View sv) {
		return as_char(sv);
	}

	constexpr View view_front(View sv) {
		return as_view(sv);
	}


	// Back char.
	constexpr uint8_t byte_back(View sv) {
		return as_byte(view_back(sv));
	}

	constexpr uint32_t char_back(View sv) {
		return as_char(view_back(sv));
	}

	constexpr View view_back(View sv) {
		return { utf_char_prev(sv.end), sv.end };
	}


	// Consume a range of characters while predicate holds.
	template <typename F> constexpr View consume_view(View& src, View& chr, const F& fn) {
		View sv = src;   // Lookahead.
		View out = src;  // Matching character range.

		chr = as_view(src);

		do {
			src = sv;
			sv = iter_next_view(sv, chr);
		} while (not src.is_eof() and fn(chr));

		out.end = src.begin;

		return out;
	}

	template <typename F> constexpr View consume_char(View& src, uint32_t& chr, const F& fn) {
		View sv = src;   // Lookahead.
		View out = src;  // Matching character range.

		chr = as_char(src);

		do {
			src = sv;
			sv = iter_next_char(sv, chr);
		} while (not src.is_eof() and fn(chr));

		out.end = src.begin;

		return out;
	}

	template <typename F> constexpr View consume_byte(View& src, uint8_t& chr, const F& fn) {
		View sv = src;   // Lookahead.
		View out = src;  // Matching character range.

		chr = as_byte(src);

		do {
			src = sv;
			sv = iter_next_byte(sv, chr);
		} while (not src.is_eof() and fn(chr));

		out.end = src.begin;

		return out;
	}


	// Strip leading and trailing whitespace.
	constexpr View lstrip(View sv) {
		if (sv.is_eof()) return sv;

		// Skip forward while the first character
		// is whitespace.
		while (is_whitespace(as_char(sv)))
			sv = next_char(sv);

		return sv;
	}

	constexpr View rstrip(View sv) {
		if (sv.is_eof()) return sv;

		// Shrink the view (end--) while the last
		// character is whitespace.
		while (is_whitespace(char_back(sv)))
			sv = shrink_char(sv);

		return sv;
	}

	constexpr View strip(View sv) {
		return lstrip(rstrip(sv));
	}


	// Return a new view of the overlapping region of 2 other views.
	constexpr View overlap(View a, View b) {
		if (not overlapping_intervals(a.begin, a.end, b.begin, b.end))
			return a;

		return { a.begin, b.end };
	}


	// Return the outer view minus the inner view.
	constexpr View before(const View outer, View inner) {
		return { outer.begin, inner.begin };
	}

	constexpr View after(const View outer, View inner) {
		return { inner.end, outer.end };
	}


	// Count occurences of character as per supplied predicate in view.
	template <typename F> constexpr size_t count(View sv, const F& fn) {
		if (sv.is_eof()) return 0;

		uint32_t chr = as_char(sv);
		size_t count = 0;

		while (not sv.is_eof()) {
			if (fn(chr))
				count++;

			sv = iter_next_char(sv, chr);
		}

		return count;
	}

	template <typename F> constexpr size_t count_consecutive(View sv, const F& fn) {
		if (sv.is_eof()) return 0;

		uint32_t chr = as_char(sv);
		size_t count = 0;

		if (not fn(chr))
			return 0;

		while (not sv.is_eof() and fn(chr)) {
			count++;
			sv = iter_next_char(sv, chr);
		}

		return count - 1;
	}


	// Count newline characters in view.
	constexpr size_t count_lines(View sv) {
		return count(sv, [] (uint32_t c) { return c == '\n'; });
	}


	// Get specific line.
	template <typename F> constexpr View split(View sv, const F& fn, size_t i) {
		if (sv.is_eof()) return sv;

		View out = sv;

		const auto& [sbegin, send] = sv;
		auto& [begin, end] = out;

		uint32_t chr = as_char(sv);
		size_t count = 0;

		const char* prev_end = sbegin;

		// Loop over every character while taking note of item
		// beginnings and endings. If we reach the specific
		// count we want or hit EOF, we return the last item we found.
		while (true) {
			if (sv.is_eof() or fn(chr)) {
				consume_char(sv, chr, fn);

				begin = prev_end;   // Set begin to start of line.
				prev_end = sbegin;  // Set the previous line ending to the current line.

				// If we reach EOF, set the end position otherwise we
				// will ignore a single character before EOF.
				if (sv.is_eof())
					end = sbegin;

				if (count == i)
					break;

				count++;
			}

			end = sbegin;  // Set the current line ending to the current char.
			sv = iter_next_char(sv, chr);
		}

		return out;
	}


	// Get specific line.
	constexpr View line(View sv, size_t i) {
		return split(sv, [] (uint32_t chr) {
			return chr == '\n';
		}, i);
	}


	// Get nth word.
	constexpr View word(View sv, size_t i) {
		return split(sv, is_whitespace, i);
	}


	// Extend `inner` to be a view of the same line it's on.
	constexpr View extend_to_line(const View outer, View inner) {
		if (inner.is_eof()) return inner;

		const auto& [sbegin, send] = outer;
		auto& [begin, end] = inner;

		// Walk backwards through the source until we hit the beginning of the string
		// or we hit the count of lines.
		while (begin >= sbegin and *begin != '\n')
			begin--;

		// Walk forwards through the source until we hit EOF or we hit the count
		// of lines.
		while (end <= send and *end != '\n')
			end++;

		if (*begin == '\n')
			begin++;

		return inner;
	}

}


// Convert a string literal to a View.
// We can determine the size if it's a literal
// because it hasn't decayed and so the length
// is available to us.
// #define cstr(s)
// 	(cane::View { s, ((const char*)s) + (sizeof(s) - 1) })

constexpr cane::View operator""_sv(const char* const str, size_t n) {
	return { str, n };
}


// Hashing function for `View` so that we can insert
// it into unordered_map.
namespace std {
	template <> struct hash<cane::View> {
		constexpr size_t operator()(cane::View v) const {
			return cane::hash_bytes(v.begin, v.end);
		}
	};
}

#endif

