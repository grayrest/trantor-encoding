import TomlNumber
import TomlProblem

## TOML's four strings over bytes, each read from its opening quote.
##
## Basic strings take escapes (`\b \t \n \f \r \e \" \\`, `\xHH`, `\uHHHH`,
## `\UHHHHHHHH` naming Unicode scalar values); literal strings take none.
## Multi-line forms drop a line break right after the opening delimiter, read
## every line break as `\n`, and hold up to two quotes before the closing
## delimiter. A carriage return outside CRLF and control characters other than
## tab are refused.
TomlString :: [].{

	Read : { text : Str, next : U64 }

	## Whichever string starts at `index` (a `"` or `'`).
	read : List(U8), U64 -> Try(Read, TomlProblem.Problem)
	read = |bytes, index|
		if bytes.get(index) == Ok('"') {
			if is_triple(bytes, index, '"') {
				multi_line_basic(bytes, index)
			} else {
				basic(bytes, index)
			}
		} else if is_triple(bytes, index, '\'') {
			multi_line_literal(bytes, index)
		} else {
			literal(bytes, index)
		}

	## A single-line string starting at `index`, as a quoted key is.
	single_line : List(U8), U64 -> Try(Read, TomlProblem.Problem)
	single_line = |bytes, index|
		if bytes.get(index) == Ok('"') {
			basic(bytes, index)
		} else {
			literal(bytes, index)
		}
}

delimiter_length : U64
delimiter_length = 3

## The most quotes a run may hold where a multi-line string closes: two of
## the content and the three of the delimiter.
max_closing_quotes : U64
max_closing_quotes = 5

delete : U8
delete = 0x7F

expected_same_line : Str
expected_same_line = "the closing quote on the same line"

expected_no_control : Str
expected_no_control = "no control characters other than tab (escape them)"

is_triple : List(U8), U64, U8 -> Bool
is_triple = |bytes, index, quote| bytes.sublist({ start: index, len: delimiter_length }) == List.repeat(quote, delimiter_length)

## Control characters a string refuses, line breaks aside.
is_forbidden_control : U8 -> Bool
is_forbidden_control = |byte| (byte < ' ' and byte != '\t' and byte != '\n' and byte != '\r') or byte == delete

finish : List(U8), U64 -> TomlString.Read
finish = |out, next| { text: Str.from_utf8_lossy(out), next }

basic : List(U8), U64 -> Try(TomlString.Read, TomlProblem.Problem)
basic = |bytes, open| basic_from(bytes, open + 1, [])

basic_from : List(U8), U64, List(U8) -> Try(TomlString.Read, TomlProblem.Problem)
basic_from = |bytes, index, out|
	match bytes.get(index) {
		Ok('"') => Ok(finish(out, index + 1))
		Ok('\\') => {
			escaped = escape(bytes, index)?
			basic_from(bytes, escaped.next, out.concat(escaped.bytes))
		}
		Ok('\n') | Ok('\r') | Err(_) => Err(TomlProblem.syntax(index, expected_same_line))
		Ok(byte) if is_forbidden_control(byte) => Err(TomlProblem.syntax(index, expected_no_control))
		Ok(byte) => basic_from(bytes, index + 1, out.append(byte))
	}

literal : List(U8), U64 -> Try(TomlString.Read, TomlProblem.Problem)
literal = |bytes, open| literal_from(bytes, open + 1, open + 1)

## A literal string is one slice: nothing in it is rewritten.
literal_from : List(U8), U64, U64 -> Try(TomlString.Read, TomlProblem.Problem)
literal_from = |bytes, start, index|
	match bytes.get(index) {
		Ok('\'') => Ok(finish(bytes.sublist({ start, len: index - start }), index + 1))
		Ok('\n') | Ok('\r') | Err(_) => Err(TomlProblem.syntax(index, expected_same_line))
		Ok(byte) if is_forbidden_control(byte) => Err(TomlProblem.syntax(index, expected_no_control))
		Ok(_) => literal_from(bytes, start, index + 1)
	}

## A line break right after the opening delimiter is dropped.
body_start : List(U8), U64 -> U64
body_start = |bytes, open| {
	after = open + delimiter_length
	match line_break_length(bytes, after) {
		Ok(length) => after + length
		Err(_) => after
	}
}

## LF is one byte, CRLF two; anything else is no line break.
line_break_length : List(U8), U64 -> Try(U64, [NotALineBreak])
line_break_length = |bytes, index|
	match bytes.get(index) {
		Ok('\n') => Ok(1)
		Ok('\r') if bytes.get(index + 1) == Ok('\n') => Ok(2)
		_ => Err(NotALineBreak)
	}

multi_line_basic : List(U8), U64 -> Try(TomlString.Read, TomlProblem.Problem)
multi_line_basic = |bytes, open| multi_line(bytes, open, '"', body_start(bytes, open), [])

multi_line_literal : List(U8), U64 -> Try(TomlString.Read, TomlProblem.Problem)
multi_line_literal = |bytes, open| multi_line(bytes, open, '\'', body_start(bytes, open), [])

## A multi-line body up to its closing delimiter; escapes only when `quote` is
## `"`.
multi_line : List(U8), U64, U8, U64, List(U8) -> Try(TomlString.Read, TomlProblem.Problem)
multi_line = |bytes, open, quote, index, out|
	match bytes.get(index) {
		Err(_) => Err(TomlProblem.syntax(open, "a closing ${Str.from_utf8_lossy(List.repeat(quote, delimiter_length))} for the string opened here"))
		Ok(byte) if byte == quote => {
			run = quote_run(bytes, index, quote) - index
			if run < delimiter_length {
				multi_line(bytes, open, quote, index + run, out.concat(List.repeat(quote, run)))
			} else if run <= max_closing_quotes {
				Ok(finish(out.concat(List.repeat(quote, run - delimiter_length)), index + run))
			} else {
				Err(TomlProblem.syntax(index + max_closing_quotes, "at most two quotes before the closing delimiter"))
			}
		}
		Ok('\\') if quote == '"' => {
			escaped = multi_line_escape(bytes, index)?
			multi_line(bytes, open, quote, escaped.next, out.concat(escaped.bytes))
		}
		Ok('\n') => multi_line(bytes, open, quote, index + 1, out.append('\n'))
		Ok('\r') =>
			if bytes.get(index + 1) == Ok('\n') {
				multi_line(bytes, open, quote, index + 2, out.append('\n'))
			} else {
				Err(TomlProblem.syntax(index, "a line feed after the carriage return"))
			}
		Ok(byte) if is_forbidden_control(byte) => Err(TomlProblem.syntax(index, expected_no_control))
		Ok(byte) => multi_line(bytes, open, quote, index + 1, out.append(byte))
	}

## The index just past the run of `quote` starting at `index`.
quote_run : List(U8), U64, U8 -> U64
quote_run = |bytes, index, quote|
	if bytes.get(index) == Ok(quote) {
		quote_run(bytes, index + 1, quote)
	} else {
		index
	}

## A line-ending backslash drops itself, the rest of the line's whitespace
## and every blank that follows; any other backslash is an escape.
multi_line_escape : List(U8), U64 -> Try({ bytes : List(U8), next : U64 }, TomlProblem.Problem)
multi_line_escape = |bytes, index| {
	after_space = skip_space(bytes, index + 1)
	match line_break_length(bytes, after_space) {
		Ok(length) => Ok({ bytes: [], next: skip_blanks(bytes, after_space + length)? })
		Err(_) => escape(bytes, index)
	}
}

skip_space : List(U8), U64 -> U64
skip_space = |bytes, index|
	match bytes.get(index) {
		Ok(' ') | Ok('\t') => skip_space(bytes, index + 1)
		_ => index
	}

skip_blanks : List(U8), U64 -> Try(U64, TomlProblem.Problem)
skip_blanks = |bytes, index|
	match bytes.get(index) {
		Ok(' ') | Ok('\t') | Ok('\n') => skip_blanks(bytes, index + 1)
		Ok('\r') =>
			if bytes.get(index + 1) == Ok('\n') {
				skip_blanks(bytes, index + 2)
			} else {
				Err(TomlProblem.syntax(index, "a line feed after the carriage return"))
			}
		_ => Ok(index)
	}

## The escape at the backslash at `index`, as UTF-8.
escape : List(U8), U64 -> Try({ bytes : List(U8), next : U64 }, TomlProblem.Problem)
escape = |bytes, index| {
	simple = |byte| Ok({ bytes: [byte], next: index + 2 })
	match bytes.get(index + 1) {
		Ok('b') => simple(0x08)
		Ok('t') => simple('\t')
		Ok('n') => simple('\n')
		Ok('f') => simple(0x0C)
		Ok('r') => simple('\r')
		Ok('e') => simple(0x1B)
		Ok('"') => simple('"')
		Ok('\\') => simple('\\')
		Ok('x') => code_point_escape(bytes, index, byte_escape_digits)
		Ok('u') => code_point_escape(bytes, index, short_escape_digits)
		Ok('U') => code_point_escape(bytes, index, long_escape_digits)
		_ => Err(TomlProblem.syntax(index, "a valid escape (\\b \\t \\n \\f \\r \\e \\\" \\\\ \\xHH \\uHHHH \\UHHHHHHHH)"))
	}
}

code_point_escape : List(U8), U64, U64 -> Try({ bytes : List(U8), next : U64 }, TomlProblem.Problem)
code_point_escape = |bytes, index, width| {
	digits = bytes.sublist({ start: index + 2, len: width })
	value = if digits.len() == width { hex_value(digits) } else { Err(NotADigit) }
	match value {
		Ok(code_point) if is_scalar_value(code_point) => Ok({ bytes: utf8(code_point), next: index + 2 + width })
		Ok(_) => Err(TomlProblem.syntax(index, "an escape naming a Unicode scalar value"))
		Err(_) => Err(TomlProblem.syntax(index, "${width.to_str()} hexadecimal digits after the escape"))
	}
}

## `\xHH`, `\uHHHH` and `\UHHHHHHHH`.
byte_escape_digits : U64
byte_escape_digits = 2

short_escape_digits : U64
short_escape_digits = 4

long_escape_digits : U64
long_escape_digits = 8

hex_radix : U32
hex_radix = 16

hex_value : List(U8) -> Try(U32, [NotADigit])
hex_value = |digits|
	digits.fold_try(0, |value, byte| {
		digit = TomlNumber.hex_digit(byte)?
		Ok(value * hex_radix + U8.to_u32(digit))
	})

surrogate_first : U32
surrogate_first = 0xD800

surrogate_last : U32
surrogate_last = 0xDFFF

code_point_last : U32
code_point_last = 0x10FFFF

is_scalar_value : U32 -> Bool
is_scalar_value = |code_point| code_point <= code_point_last and (code_point < surrogate_first or code_point > surrogate_last)

one_byte_limit : U32
one_byte_limit = 0x80

two_byte_limit : U32
two_byte_limit = 0x800

three_byte_limit : U32
three_byte_limit = 0x10000

## The six payload bits of a continuation byte, and its `10` marker.
continuation_bits : U32
continuation_bits = 0x3F

continuation_marker : U32
continuation_marker = 0x80

## Lead-byte markers for two-, three- and four-byte sequences.
lead_two : U32
lead_two = 0xC0

lead_three : U32
lead_three = 0xE0

lead_four : U32
lead_four = 0xF0

bits_per_continuation : U8
bits_per_continuation = 6

## A scalar value's UTF-8 bytes.
utf8 : U32 -> List(U8)
utf8 = |code_point| {
	lead = |continuations, marker| U32.to_u8_wrap(code_point.shr_wrap(continuations * bits_per_continuation).bitwise_or(marker))
	tail = |position| U32.to_u8_wrap(code_point.shr_wrap(position * bits_per_continuation).bitwise_and(continuation_bits).bitwise_or(continuation_marker))
	if code_point < one_byte_limit {
		[U32.to_u8_wrap(code_point)]
	} else if code_point < two_byte_limit {
		[lead(1, lead_two), tail(0)]
	} else if code_point < three_byte_limit {
		[lead(2, lead_three), tail(1), tail(0)]
	} else {
		[lead(3, lead_four), tail(2), tail(1), tail(0)]
	}
}

reads : Str -> Try(Str, TomlProblem.Problem)
reads = |text| {
	bytes = text.to_utf8()
	read = TomlString.read(bytes, 0)?
	if read.next == bytes.len() { Ok(read.text) } else { Err(TomlProblem.syntax(read.next, "the end")) }
}

refused_at : Str -> Try(U64, [NotRefused])
refused_at = |text|
	match reads(text) {
		Err(Syntax(problem)) => Ok(problem.at)
		_ => Err(NotRefused)
	}

# Basic strings and every escape; `\x`, `\u` and `\U` come out as UTF-8.
expect reads("\"a\\tb\"") == Ok("a\tb")
expect reads("\"\\b\\t\\n\\f\\r\\e\\\"\\\\\"") == Ok("\u(8)\t\n\u(C)\r\u(1B)\"\\")
expect reads("\"\\xE9\\u20ac\\U0001F600\\x7f\"") == Ok("é€😀\u(7F)")
expect refused_at("\"\\u00\"") == Ok(1)
expect refused_at("\"\\uD800\"") == Ok(1)
expect refused_at("\"\\U00110000\"") == Ok(1)
expect refused_at("\"\\q\"") == Ok(1)
expect refused_at("\"a\nb\"") == Ok(2)
expect refused_at("\"a\u(7F)\"") == Ok(2)
expect refused_at("\"open") == Ok(5)
expect reads("\"a\tb\"") == Ok("a\tb")

# Literal strings: no escapes.
expect reads("'C:\\Users\\n'") == Ok("C:\\Users\\n")
expect refused_at("'a\u(0)'") == Ok(2)

# Multi-line: the leading break dropped, CRLF and LF both read as LF, an
# escaped CR kept, quotes inside and just before the closing delimiter.
expect reads("\"\"\"\nRoses\r\nViolets\"\"\"") == Ok("Roses\nViolets")
expect reads("\"\"\"\r\nRoses\nViolets\"\"\"") == Ok("Roses\nViolets")
expect reads("\"\"\"a\\rb\"\"\"") == Ok("a\rb")
expect reads("\"\"\"two \"\" here\"\"\"") == Ok("two \"\" here")
expect reads("\"\"\"\"quoted\"\"\"\"") == Ok("\"quoted\"")
expect reads("\"\"\"\"\"\"\"\"") == Ok("\"\"")
expect refused_at("\"\"\"a\"\"\"\"\"\"") == Ok(9)
expect refused_at("\"\"\"a\rb\"\"\"") == Ok(4)
expect reads("\"\"\"The quick \\  \n\n   brown\"\"\"") == Ok("The quick brown")
expect refused_at("\"\"\"a \\ b\"\"\"") == Ok(5)
expect reads("'''\nit's '' fine'''") == Ok("it's '' fine")
expect reads("''''a'''''") == Ok("'a''")
expect reads("'''a\\n'''") == Ok("a\\n")
expect refused_at("'''open") == Ok(0)
