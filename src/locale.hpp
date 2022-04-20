#ifndef CANE_LOCALE_HPP
#define CANE_LOCALE_HPP

#include <view.hpp>

namespace cane {

	constexpr View STR_ENCODING     = "malformed source encoding"_sv;
	constexpr View STR_UNKNOWN_CHAR = "unknown character '{}'"_sv;
	constexpr View STR_NOT_NUMBER   = "invalid digit '{}'"_sv;

	constexpr View STR_SEQUENCE = "expecting a sequence"_sv;
	constexpr View STR_LITERAL  = "expecting a literal"_sv;
	constexpr View STR_STEP     = "expecting a step"_sv;
	constexpr View STR_IDENT    = "expecting an identifier"_sv;
	constexpr View STR_OPERATOR = "expecting an operator"_sv;
	constexpr View STR_EXPR     = "expecting an expression"_sv;

	constexpr View STR_EXPECT = "expecting '{}'"_sv;

}

#endif
