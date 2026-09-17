import pf.Toml

## A JSON reader for toml-test's expected output: objects, arrays and strings
## only, a `{ "type", "value" }` object being a TOML value. It reads escapes
## and RFC 3339 text itself, so the expected side does not lean on the parser
## under test.
ExpectedJson :: [].{

	## The TOML value a whole expected-output file describes.
	document : Str -> Try(Toml.Value, Str)
	document = |text| {
		bytes = text.to_utf8()
		read = json_value(bytes, skip_json_space(bytes, 0))?
		toml_of(read.json)
	}
}

JsonTree := [Object(List((Str, JsonTree))), Array(List(JsonTree)), Text(Str)]

json_value : List(U8), U64 -> Try({ json : JsonTree, next : U64 }, Str)
json_value = |bytes, index|
	match bytes.get(index) {
		Ok('{') => json_members(bytes, skip_json_space(bytes, index + 1), [])
		Ok('[') => json_items(bytes, skip_json_space(bytes, index + 1), [])
		Ok('"') => {
			read = json_string(bytes, index + 1, [])?
			Ok({ json: Text(read.text), next: read.next })
		}
		_ => Err("unexpected byte at ${index.to_str()}")
	}

json_members : List(U8), U64, List((Str, JsonTree)) -> Try({ json : JsonTree, next : U64 }, Str)
json_members = |bytes, index, members|
	if bytes.get(index) == Ok('}') {
		Ok({ json: Object(members), next: index + 1 })
	} else {
		name = json_string(bytes, index + 1, [])?
		colon = skip_json_space(bytes, name.next)
		member = json_value(bytes, skip_json_space(bytes, colon + 1))?
		after = skip_json_space(bytes, member.next)
		more = members.append((name.text, member.json))
		if bytes.get(after) == Ok(',') {
			json_members(bytes, skip_json_space(bytes, after + 1), more)
		} else {
			Ok({ json: Object(more), next: after + 1 })
		}
	}

json_items : List(U8), U64, List(JsonTree) -> Try({ json : JsonTree, next : U64 }, Str)
json_items = |bytes, index, items|
	if bytes.get(index) == Ok(']') {
		Ok({ json: Array(items), next: index + 1 })
	} else {
		item = json_value(bytes, index)?
		after = skip_json_space(bytes, item.next)
		more = items.append(item.json)
		if bytes.get(after) == Ok(',') {
			json_items(bytes, skip_json_space(bytes, after + 1), more)
		} else {
			Ok({ json: Array(more), next: after + 1 })
		}
	}

json_string : List(U8), U64, List(U8) -> Try({ text : Str, next : U64 }, Str)
json_string = |bytes, index, out|
	match bytes.get(index) {
		Ok('"') => Ok({ text: Str.from_utf8_lossy(out), next: index + 1 })
		Ok('\\') => {
			escaped = json_escape(bytes, index)?
			json_string(bytes, escaped.next, out.concat(escaped.bytes))
		}
		Ok(byte) => json_string(bytes, index + 1, out.append(byte))
		Err(_) => Err("unterminated string")
	}

json_escape : List(U8), U64 -> Try({ bytes : List(U8), next : U64 }, Str)
json_escape = |bytes, index| {
	simple = |byte| Ok({ bytes: [byte], next: index + 2 })
	match bytes.get(index + 1) {
		Ok('n') => simple('\n')
		Ok('t') => simple('\t')
		Ok('r') => simple('\r')
		Ok('b') => simple(0x08)
		Ok('f') => simple(0x0C)
		Ok('u') => {
			unit = hex4(bytes, index + 2)?
			if unit >= 0xD800 and unit < 0xDC00 {
				low = hex4(bytes, index + 8)?
				code_point = 0x10000 + (unit - 0xD800) * 0x400 + (low - 0xDC00)
				Ok({ bytes: utf8(code_point), next: index + 12 })
			} else {
				Ok({ bytes: utf8(unit), next: index + 6 })
			}
		}
		Ok(byte) => simple(byte)
		Err(_) => Err("dangling escape")
	}
}

hex4 : List(U8), U64 -> Try(U32, Str)
hex4 = |bytes, index|
	bytes.sublist({ start: index, len: 4 }).fold_try(0, |value, byte| {
		digit =
			if byte >= '0' and byte <= '9' {
				Ok(byte - '0')
			} else if byte >= 'a' and byte <= 'f' {
				Ok(byte - 'a' + 10)
			} else if byte >= 'A' and byte <= 'F' {
				Ok(byte - 'A' + 10)
			} else {
				Err("bad \\u escape")
			}
		Ok(value * 16 + U8.to_u32(digit?))
	})

## A code point's UTF-8 bytes.
utf8 : U32 -> List(U8)
utf8 = |code_point| {
	low = |shift| U32.to_u8_wrap(code_point.shr_wrap(shift).bitwise_and(0x3F).bitwise_or(0x80))
	if code_point < 0x80 {
		[U32.to_u8_wrap(code_point)]
	} else if code_point < 0x800 {
		[U32.to_u8_wrap(code_point.shr_wrap(6).bitwise_or(0xC0)), low(0)]
	} else if code_point < 0x10000 {
		[U32.to_u8_wrap(code_point.shr_wrap(12).bitwise_or(0xE0)), low(6), low(0)]
	} else {
		[U32.to_u8_wrap(code_point.shr_wrap(18).bitwise_or(0xF0)), low(12), low(6), low(0)]
	}
}

skip_json_space : List(U8), U64 -> U64
skip_json_space = |bytes, index|
	match bytes.get(index) {
		Ok(' ') | Ok('\n') | Ok('\r') | Ok('\t') => skip_json_space(bytes, index + 1)
		_ => index
	}

toml_of : JsonTree -> Try(Toml.Value, Str)
toml_of = |json|
	match json {
		Object([("type", Text(kind)), ("value", Text(text))]) => leaf(kind, text)
		Object([("value", Text(text)), ("type", Text(kind))]) => leaf(kind, text)
		Object(members) => {
			entries = members.map_try(|(name, member)| toml_of(member).map_ok(|value| (name, value)))?
			Ok(Table(entries))
		}
		Array(items) => {
			values = items.map_try(toml_of)?
			Ok(Array(values))
		}
		Text(text) => Err("a bare string ${text}")
	}

leaf : Str, Str -> Try(Toml.Value, Str)
leaf = |kind, text|
	match kind {
		"string" => Ok(String(text))
		"integer" => I64.from_str(text).map_ok(|value| Integer(value)).map_err(|_| "integer ${text}")
		"float" => Ok(Float(float_of(text)))
		"bool" => Ok(Boolean(text == "true"))
		"datetime" | "datetime-local" | "date-local" | "time-local" => moment(kind, text)
		_ => Err("unknown type ${kind}")
	}

float_of : Str -> Toml.Float
float_of = |text| {
	lower = text.with_ascii_lowercased()
	if lower.ends_with("nan") {
		Toml.float_from_f64(F64.nan)
	} else if lower.ends_with("inf") {
		Toml.float_from_f64(if lower.starts_with("-") { -F64.infinity } else { F64.infinity })
	} else {
		Toml.float_from_f64(F64.from_str(text) ?? F64.nan)
	}
}

## RFC 3339 as toml-test writes it: `T`, fractions of any length, `Z` or a
## numeric offset.
moment : Str, Str -> Try(Toml.Value, Str)
moment = |kind, text|
	match kind {
		"date-local" => date_of(text).map_ok(|date| LocalDate(date))
		"time-local" => time_of(text).map_ok(|time| LocalTime(time))
		"datetime-local" => {
			halves = text.split_first("T") ? |_| "datetime ${text}"
			Ok(LocalDatetime({ date: date_of(halves.before)?, time: time_of(halves.after)? }))
		}
		_ => {
			halves = text.split_first("T") ? |_| "datetime ${text}"
			zoned = offset_split(halves.after)
			Ok(OffsetDatetime({ date: date_of(halves.before)?, time: time_of(zoned.clock)?, offset: zoned.offset }))
		}
	}

offset_split : Str -> { clock : Str, offset : Toml.Offset }
offset_split = |text| {
	bytes = text.to_utf8()
	if text.ends_with("Z") {
		{ clock: text.drop_suffix("Z"), offset: { minutes: 0 } }
	} else {
		cut = bytes.len() - 6
		sign = if bytes.get(cut) == Ok('-') { -1 } else { 1 }
		hours = number(bytes.sublist({ start: cut + 1, len: 2 }))
		minutes = number(bytes.sublist({ start: cut + 4, len: 2 }))
		{ clock: Str.from_utf8_lossy(bytes.take_first(cut)), offset: { minutes: sign * U64.to_i16_wrap(hours * 60 + minutes) } }
	}
}

date_of : Str -> Try(Toml.Date, Str)
date_of = |text|
	match text.split_on("-") {
		[year, month, day] => Ok({ year: U64.to_i32_wrap(number(year.to_utf8())), month: U64.to_u8_wrap(number(month.to_utf8())), day: U64.to_u8_wrap(number(day.to_utf8())) })
		_ => Err("date ${text}")
	}

time_of : Str -> Try(Toml.Time, Str)
time_of = |text| {
	halves = text.split_first(".") ?? { before: text, after: "" }
	nine = halves.after.to_utf8().take_first(9).concat(List.repeat('0', 9 - halves.after.count_utf8_bytes().min(9)))
	match halves.before.split_on(":") {
		[hour, minute, second] =>
			Ok({
				hour: U64.to_u8_wrap(number(hour.to_utf8())),
				minute: U64.to_u8_wrap(number(minute.to_utf8())),
				second: U64.to_u8_wrap(number(second.to_utf8())),
				millisecond: U64.to_u16_wrap(number(nine.take_first(3))),
				microsecond: U64.to_u16_wrap(number(nine.sublist({ start: 3, len: 3 }))),
				nanosecond: U64.to_u16_wrap(number(nine.drop_first(6))),
			})
		_ => Err("time ${text}")
	}
}

number : List(U8) -> U64
number = |digits| digits.fold(0, |value, byte| value * 10 + U8.to_u64(byte - '0'))
