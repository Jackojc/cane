#ifndef CANE_LOCALE_HPP
#define CANE_LOCALE_HPP

#include <view.hpp>

namespace cane {

	constexpr View STR_ENCODING     = "malformed source encoding"_sv;
	constexpr View STR_UNKNOWN_CHAR = "unknown character `{}`"_sv;
	constexpr View STR_NOT_NUMBER   = "invalid digit `{}`"_sv;

	constexpr View STR_GREATER      = "value must be greater than `{}`"_sv;
	constexpr View STR_LESSER       = "value must be lesser than `{}`"_sv;
	constexpr View STR_GREATER_EQ   = "value must be greater than or equal to `{}`"_sv;
	constexpr View STR_LESSER_EQ    = "value must be lesser than or equal to `{}`"_sv;
	constexpr View STR_BETWEEN      = "value must be between `{}` and `{}` inclusively"_sv;

	constexpr View STR_UNDEFINED = "undefined variable `{}`"_sv;
	constexpr View STR_REDEFINED = "variable `{}` re-defined"_sv;

	constexpr View STR_SEQUENCE  = "expecting a sequence"_sv;
	constexpr View STR_LITERAL   = "expecting a literal"_sv;
	constexpr View STR_STEP      = "expecting a step"_sv;
	constexpr View STR_IDENT     = "expecting an identifier"_sv;
	constexpr View STR_EXPR      = "expecting an expression"_sv;
	constexpr View STR_NUD       = "expecting a sequence or identifier"_sv;
	constexpr View STR_CHAIN     = "expecting an identifier"_sv;
	constexpr View STR_OPERATOR  = "expecting an operator"_sv;
	constexpr View STR_STATEMENT = "expecting a statement"_sv;

	constexpr View STR_EXPECT = "expecting `{}`"_sv;

	constexpr View STR_DEBUG = "sequence is `{}` at this point"_sv;

}

#endif
