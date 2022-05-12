#ifndef CANE_LOCALE_HPP
#define CANE_LOCALE_HPP

#include <view.hpp>

namespace cane {

	constexpr const char* CSTR_EXE   = "cane";
	constexpr const char* CSTR_PORT  = "midi out";

	constexpr View STR_EXE           = "cane"_sv;
	constexpr View STR_PORT          = "midi out"_sv;

	constexpr View STR_ENCODING      = "malformed source encoding"_sv;
	constexpr View STR_UNREACHABLE   = "unreachable code"_sv;

	constexpr View STR_GREATER       = "value must be > `{}`"_sv;
	constexpr View STR_LESSER        = "value must be < `{}`"_sv;
	constexpr View STR_GREATER_EQ    = "value must be >= `{}`"_sv;
	constexpr View STR_LESSER_EQ     = "value must be <= `{}`"_sv;
	constexpr View STR_BETWEEN       = "value must be >= `{}` and <= `{}`"_sv;

	constexpr View STR_SEQUENCE      = "expecting a sequence"_sv;
	constexpr View STR_LITERAL       = "expecting a literal"_sv;
	constexpr View STR_STEP          = "expecting a step"_sv;
	constexpr View STR_IDENT         = "expecting an identifier"_sv;
	constexpr View STR_IDENT_LITERAL = "expecting an identifier or literal"_sv;

	constexpr View STR_LIT_PREFIX         = "expecting a literal prefix operator"_sv;
	constexpr View STR_LIT_INFIX          = "expecting a literal infix operator"_sv;
	constexpr View STR_LIT_PREFIX_LITERAL = "expecting a literal or prefix literal operator"_sv;

	constexpr View STR_SEQ_PREFIX        = "expecting a sequence prefix operator"_sv;
	constexpr View STR_SEQ_POSTFIX       = "expecting a sequence postfix operator"_sv;
	constexpr View STR_SEQ_INFIX         = "expecting a sequence infix operator"_sv;
	constexpr View STR_SEQ_INFIX_EXPR    = "expecting a sequence infix expression operator"_sv;
	constexpr View STR_SEQ_INFIX_LITERAL = "expecting a sequence infix literal operator"_sv;

	constexpr View STR_SEQ_EXPR          = "expecting a sequence expression"_sv;
	constexpr View STR_LIT_EXPR          = "expecting a literal expression"_sv;

	constexpr View STR_STATEMENT     = "expecting a statement"_sv;

	constexpr View STR_EXPECT        = "expecting `{}`"_sv;
	constexpr View STR_UNKNOWN_CHAR  = "unknown character `{}`"_sv;
	constexpr View STR_NOT_NUMBER    = "invalid digit `{}`"_sv;
	constexpr View STR_EMPTY         = "empty sequence"_sv;
	constexpr View STR_NO_BPM        = "sequence has no tempo"_sv;

	constexpr View STR_UNDEFINED     = "`{}` is undefined"_sv;
	constexpr View STR_REDEFINED     = "`{}` has been re-defined"_sv;

	constexpr View STR_DEBUG         = "`{}`(x{}) @{}bpm/{}s"_sv;

	constexpr View STR_OPT_NO_FILE        = "no file specified"_sv;
	constexpr View STR_OPT_INVALID_OPTION = "invalid option `{}`"_sv;
	constexpr View STR_OPT_INVALID_ARG    = "invalid argument `{}` for `{}`"_sv;
	constexpr View STR_OPT_MISSING_ARG    = "missing argument for `{}`"_sv;

	constexpr View STR_MIDI_LOST_EVENT      = "`{}` MIDI event(s) lost"_sv;
	constexpr View STR_MIDI_NO_DEVICE       = "no MIDI device specified"_sv;
	constexpr View STR_MIDI_DEVICE          = "device `{}`"_sv;
	constexpr View STR_MIDI_FOUND           = "found port `{}`"_sv;
	constexpr View STR_MIDI_NOT_FOUND       = "port `{}` not found"_sv;
	constexpr View STR_MIDI_CONNECT_ERROR   = "could not connect to the JACK server"_sv;
	constexpr View STR_MIDI_PORT_ERROR      = "could not register port"_sv;
	constexpr View STR_MIDI_WRITE_ERROR     = "could not send MIDI event"_sv;
	constexpr View STR_MIDI_ACTIVATE_ERROR  = "could not activate JACK client"_sv;
	constexpr View STR_MIDI_GET_PORTS_ERROR = "could not get MIDI input ports from JACK"_sv;
	constexpr View STR_MIDI_PATCH_ERROR     = "could not connect to port `{}`"_sv;

	constexpr View STR_SYMLINK_ERROR        = "symlink `{}` resolves to itself"_sv;
	constexpr View STR_NOT_FILE_ERROR       = "`{}` is not a file"_sv;
	constexpr View STR_FILE_NOT_FOUND_ERROR = "file `{}` not found"_sv;
	constexpr View STR_FILE_READ_ERROR      = "cannot read `{}`"_sv;

}

#endif
