import TomlDate
import TomlNumber
import TomlProblem
import TomlString
import TomlValue

## The tokens between TOML's structure: whitespace, comments and line breaks,
## keys, and scalar values (strings, booleans, numbers, dates and times).
## Arrays and inline tables are `TomlParse`'s.
TomlLex :: [].{

	## One segment of a key, the byte where it starts and the byte after it.
	KeyPart : { name : Str, at : U64, next : U64 }

	## Past spaces and tabs.
	skip_space : List(U8), U64 -> U64
	skip_space = |bytes, index|
		match bytes.get(index) {
			Ok(' ') | Ok('\t') => TomlLex.skip_space(bytes, index + 1)
			_ => index
		}

	## Past a comment starting at `index`, if there is one, up to its line
	## break.
	skip_comment : List(U8), U64 -> Try(U64, TomlProblem.Problem)
	skip_comment = |bytes, index|
		if bytes.get(index) == Ok('#') {
			comment_from(bytes, index + 1)
		} else {
			Ok(index)
		}

	## Spaces, an optional comment, then a line break or the end; the index
	## after the break.
	line_end : List(U8), U64 -> Try(U64, TomlProblem.Problem)
	line_end = |bytes, index| {
		at = TomlLex.skip_comment(bytes, TomlLex.skip_space(bytes, index))?
		if at >= bytes.len() {
			Ok(at)
		} else {
			past_line_break(bytes, at).map_err(|_| TomlProblem.syntax(at, "a line break or a comment after this"))
		}
	}

	## Past any run of spaces, comments and line breaks.
	skip_blank : List(U8), U64 -> Try(U64, TomlProblem.Problem)
	skip_blank = |bytes, index| {
		at = TomlLex.skip_comment(bytes, TomlLex.skip_space(bytes, index))?
		match past_line_break(bytes, at) {
			Ok(next) => TomlLex.skip_blank(bytes, next)
			Err(NotALineBreak) => Ok(at)
			Err(LoneCarriageReturn) => Err(TomlProblem.syntax(at, expected_line_feed))
		}
	}

	## A bare, quoted or dotted key; spaces around dots are allowed.
	key : List(U8), U64 -> Try({ parts : List(KeyPart), next : U64 }, TomlProblem.Problem)
	key = |bytes, index| key_from(bytes, index, [])

	## The scalar value starting at `index`.
	scalar : List(U8), U64 -> Try({ value : TomlValue.Value, next : U64 }, TomlProblem.Problem)
	scalar = |bytes, index|
		match bytes.get(index) {
			Ok('"') | Ok('\'') => {
				read = TomlString.read(bytes, index)?
				Ok({ value: String(read.text), next: read.next })
			}
			Ok('t') => word(bytes, index, "true", Boolean(True))
			Ok('f') => word(bytes, index, "false", Boolean(False))
			Ok(byte) if is_digit(byte) and TomlDate.starts_at(bytes, index) => moment(bytes, index)
			Ok(byte) if is_digit(byte) or byte == '+' or byte == '-' or byte == 'i' or byte == 'n' => number(bytes, index)
			_ => Err(TomlProblem.syntax(index, "a value"))
		}
}

expected_line_feed : Str
expected_line_feed = "a line feed after the carriage return"

delete : U8
delete = 0x7F

comment_from : List(U8), U64 -> Try(U64, TomlProblem.Problem)
comment_from = |bytes, index|
	match bytes.get(index) {
		Err(_) | Ok('\n') | Ok('\r') => Ok(index)
		Ok(byte) if byte == '\t' or (byte >= ' ' and byte != delete) => comment_from(bytes, index + 1)
		Ok(_) => Err(TomlProblem.syntax(index, "no control characters other than tab in a comment"))
	}

past_line_break : List(U8), U64 -> Try(U64, [NotALineBreak, LoneCarriageReturn])
past_line_break = |bytes, index|
	match bytes.get(index) {
		Ok('\n') => Ok(index + 1)
		Ok('\r') => if bytes.get(index + 1) == Ok('\n') { Ok(index + 2) } else { Err(LoneCarriageReturn) }
		_ => Err(NotALineBreak)
	}

key_from : List(U8), U64, List(TomlLex.KeyPart) -> Try({ parts : List(TomlLex.KeyPart), next : U64 }, TomlProblem.Problem)
key_from = |bytes, index, parts| {
	part = simple_key(bytes, index)?
	more = parts.append({ name: part.text, at: index, next: part.next })
	after_space = TomlLex.skip_space(bytes, part.next)
	if bytes.get(after_space) == Ok('.') {
		key_from(bytes, TomlLex.skip_space(bytes, after_space + 1), more)
	} else {
		Ok({ parts: more, next: part.next })
	}
}

simple_key : List(U8), U64 -> Try(TomlString.Read, TomlProblem.Problem)
simple_key = |bytes, index|
	match bytes.get(index) {
		Ok('"') | Ok('\'') => TomlString.single_line(bytes, index)
		_ => {
			end = bare_end(bytes, index)
			if end == index {
				Err(TomlProblem.syntax(index, "a key"))
			} else {
				Ok({ text: Str.from_utf8_lossy(bytes.sublist({ start: index, len: end - index })), next: end })
			}
		}
	}

bare_end : List(U8), U64 -> U64
bare_end = |bytes, index|
	match bytes.get(index) {
		Ok(byte) if is_bare_key_byte(byte) => bare_end(bytes, index + 1)
		_ => index
	}

is_bare_key_byte : U8 -> Bool
is_bare_key_byte = |byte|
	is_digit(byte) or (byte >= 'A' and byte <= 'Z') or (byte >= 'a' and byte <= 'z') or byte == '_' or byte == '-'

is_digit : U8 -> Bool
is_digit = |byte| byte >= '0' and byte <= '9'

word : List(U8), U64, Str, TomlValue.Value -> Try({ value : TomlValue.Value, next : U64 }, TomlProblem.Problem)
word = |bytes, index, spelling, value| {
	letters = spelling.to_utf8()
	if bytes.sublist({ start: index, len: letters.len() }) == letters {
		Ok({ value, next: index + letters.len() })
	} else {
		Err(TomlProblem.syntax(index, "a value"))
	}
}

number : List(U8), U64 -> Try({ value : TomlValue.Value, next : U64 }, TomlProblem.Problem)
number = |bytes, index|
	match TomlNumber.read(bytes, index) {
		Ok({ number: Integer(value), next }) => Ok({ value: Integer(value), next })
		Ok({ number: Float(reading), next }) => {
			spelling = Str.from_utf8_lossy(bytes.sublist({ start: index, len: next - index }))
			Ok({ value: Float(TomlValue.float_read(spelling, reading)), next })
		}
		Err(Invalid(problem)) => Err(Syntax(problem))
		Err(OutOfRange(next)) => Err(OutOfRange({ at: index, next }))
	}

moment : List(U8), U64 -> Try({ value : TomlValue.Value, next : U64 }, TomlProblem.Problem)
moment = |bytes, index|
	match TomlDate.read(bytes, index) {
		Ok(read) => Ok({ value: moment_value(read.moment), next: read.next })
		Err(Invalid(problem)) => Err(Syntax(problem))
		Err(OutOfRange(next)) => Err(OutOfRange({ at: index, next }))
	}

moment_value : TomlDate.Moment -> TomlValue.Value
moment_value = |moment_read|
	match moment_read {
		OffsetDatetime(parts) => OffsetDatetime(parts)
		LocalDatetime(parts) => LocalDatetime(parts)
		LocalDate(date) => LocalDate(date)
		LocalTime(time) => LocalTime(time)
	}

keys_of : Str -> Try(List(Str), TomlProblem.Problem)
keys_of = |text| TomlLex.key(text.to_utf8(), 0).map_ok(|read| read.parts.map(|part| part.name))

scalar_of : Str -> Try(TomlValue.Value, TomlProblem.Problem)
scalar_of = |text| TomlLex.scalar(text.to_utf8(), 0).map_ok(|read| read.value)

# Keys: bare, quoted, dotted with spaces, and where each part starts.
expect keys_of("bare-key_1") == Ok(["bare-key_1"])
expect keys_of("\"a.b\" . 'c' .d = 1") == Ok(["a.b", "c", "d"])
expect TomlLex.key("a . b = 1".to_utf8(), 0).map_ok(|read| (read.parts.map(|part| part.at), read.next)) == Ok(([0, 4], 5))
expect keys_of("\"\"") == Ok([""])
expect keys_of("= 1") == Err(Syntax({ at: 0, expected: "a key" }))
expect keys_of("a. = 1") == Err(Syntax({ at: 3, expected: "a key" }))
expect keys_of("é = 1") == Err(Syntax({ at: 0, expected: "a key" }))

# Scalars of each kind.
expect scalar_of("true") == Ok(Boolean(True))
expect scalar_of("'lit'") == Ok(String("lit"))
expect scalar_of("-17") == Ok(Integer(-17))
expect scalar_of("1979-05-27") == Ok(LocalDate({ year: 1979, month: 5, day: 27 }))
expect scalar_of("+1_0.5e1").map_ok(|value| value == Float(TomlValue.float_from_f64(105.0))) == Ok(True)
expect scalar_of("9223372036854775808") == Err(OutOfRange({ at: 0, next: 19 }))
expect scalar_of("tru") == Err(Syntax({ at: 0, expected: "a value" }))
expect scalar_of("#") == Err(Syntax({ at: 0, expected: "a value" }))

# Trivia: comments refuse control characters; line ends take LF or CRLF.
expect TomlLex.line_end("  # note\nx".to_utf8(), 0) == Ok(9)
expect TomlLex.line_end(" \r\nx".to_utf8(), 0) == Ok(3)
expect TomlLex.line_end("".to_utf8(), 0) == Ok(0)
expect TomlLex.line_end(" x".to_utf8(), 0) == Err(Syntax({ at: 1, expected: "a line break or a comment after this" }))
expect TomlLex.line_end("\rx".to_utf8(), 0) == Err(Syntax({ at: 0, expected: "a line break or a comment after this" }))
expect TomlLex.line_end("# a\u(7F)".to_utf8(), 0) == Err(Syntax({ at: 3, expected: "no control characters other than tab in a comment" }))
expect TomlLex.skip_blank(" # a\n\r\n\t# b\n  x".to_utf8(), 0) == Ok(14)
expect TomlLex.skip_blank("\r x".to_utf8(), 0) == Err(Syntax({ at: 0, expected: expected_line_feed }))
