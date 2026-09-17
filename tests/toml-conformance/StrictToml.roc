import pf.Toml

## A strict TOML 1.0 checker, for tests: a document `Toml.parse` accepts and
## that uses none of 1.1's additions. Those are, outside strings and comments,
## a line break or comment inside an inline table (outside a nested array), a
## trailing comma before `}`, a time without seconds, and inside basic strings
## the `\e` and `\xHH` escapes. It is checked against toml-test's 1.0.0 lists:
## every valid file accepted, every invalid file refused.
StrictToml :: [].{

	## `Ok` for a TOML 1.0 document, else what disqualifies it.
	check : Str -> Try({}, Str)
	check = |text|
		match Toml.parse(text) {
			Err(problem) => Err("not TOML: ${Toml.err_to_str(problem)}")
			Ok(_) => scan(text.to_utf8(), 0, { open: [], last: ' ' })
		}
}

## Brackets and braces open around the cursor, and the last byte outside
## strings, comments and blanks.
Context : { open : List(U8), last : U8 }

scan : List(U8), U64, Context -> Try({}, Str)
scan = |bytes, index, context|
	match bytes.get(index) {
		Err(_) => Ok({})
		Ok(byte) =>
			if byte == '"' or byte == '\'' {
				scan(bytes, string_end(bytes, index)?, { ..context, last: byte })
			} else if byte == '#' {
				if is_in_inline_table(context) { Err("a comment inside an inline table") } else { scan(bytes, line_end(bytes, index), context) }
			} else {
				structure(bytes, index, byte, context)
			}
	}

structure : List(U8), U64, U8, Context -> Try({}, Str)
structure = |bytes, index, byte, context|
	match byte {
		'\n' if is_in_inline_table(context) => Err("a line break inside an inline table")
		'[' | '{' => scan(bytes, index + 1, { open: context.open.append(byte), last: byte })
		']' => scan(bytes, index + 1, { open: context.open.drop_last(1), last: byte })
		'}' if context.last == ',' => Err("a trailing comma in an inline table")
		'}' => scan(bytes, index + 1, { open: context.open.drop_last(1), last: byte })
		' ' | '\t' | '\r' | '\n' => scan(bytes, index + 1, context)
		_ if is_short_time(bytes, index) => Err("a time without seconds")
		_ => scan(bytes, index + 1, { ..context, last: byte })
	}

is_in_inline_table : Context -> Bool
is_in_inline_table = |context| context.open.last() == Ok('{')

line_end : List(U8), U64 -> U64
line_end = |bytes, index| bytes.drop_first(index).find_first_index(|byte| byte == '\n').map_ok(|offset| index + offset) ?? bytes.len()

## `hh:mm` starting at `index` and not followed by `:`, where an hour can
## start (not after a digit, `:` or an offset's sign).
is_short_time : List(U8), U64 -> Bool
is_short_time = |bytes, index| {
	shape = [is_digit, is_digit, |byte| byte == ':', is_digit, is_digit]
	matches = shape.map_with_index(|test, offset| bytes.get(index + offset).map_ok(test) == Ok(True)).all(|ok| ok)
	before = if index == 0 { ' ' } else { bytes.get(index - 1) ?? ' ' }
	is_hour_start = !is_digit(before) and before != ':' and before != '+' and before != '-'
	matches and is_hour_start and bytes.get(index + 5) != Ok(':')
}

is_digit : U8 -> Bool
is_digit = |byte| byte >= '0' and byte <= '9'

## The index past the string opening at `index`; 1.1-only escapes refused.
string_end : List(U8), U64 -> Try(U64, Str)
string_end = |bytes, index| {
	quote = bytes.get(index) ?? '"'
	delimiter = if bytes.sublist({ start: index, len: 3 }) == List.repeat(quote, 3) { 3 } else { 1 }
	body_end(bytes, index + delimiter, quote, delimiter)
}

body_end : List(U8), U64, U8, U64 -> Try(U64, Str)
body_end = |bytes, index, quote, delimiter|
	match bytes.get(index) {
		Err(_) => Ok(index)
		Ok('\\') if quote == '"' =>
			match bytes.get(index + 1) {
				Ok('e') | Ok('x') => Err("a TOML 1.1 escape")
				_ => body_end(bytes, index + 2, quote, delimiter)
			}
		Ok(byte) if byte == quote => {
			run = quote_run(bytes, index, quote)
			if run >= delimiter {
				Ok(index + (if delimiter == 1 { 1 } else { run }))
			} else {
				body_end(bytes, index + run, quote, delimiter)
			}
		}
		Ok(_) => body_end(bytes, index + 1, quote, delimiter)
	}

## How many `quote`s in a row start at `index`; a multi-line string may end
## with up to two of its quotes before the delimiter.
quote_run : List(U8), U64, U8 -> U64
quote_run = |bytes, index, quote| bytes.drop_first(index).find_first_index(|byte| byte != quote) ?? (bytes.len() - index)
