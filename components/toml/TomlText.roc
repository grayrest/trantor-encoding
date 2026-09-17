import EncodingDate
import TomlValue

## How values are spelled when written: strings and keys with their escapes,
## numbers, dates and times, and inline arrays and tables.
##
## Escaped in strings and quoted keys: `"`, `\`, U+0000–U+001F and U+007F, as
## `\b \t \n \f \r` where named, else `\u00XX` (1.0) or `\e`/`\xHH` (1.1),
## hex digits uppercase; everything else is literal UTF-8. A string holding a
## line feed is written multi-line with its line feeds literal (`\r` stays
## escaped). Floats keep their spelling; dates use `T` and `Z` for offset 0;
## `V1_1` leaves out seconds that are zero with no fraction.
TomlText :: [].{

	Version : [V1_0, V1_1]

	## A basic string, multi-line when it holds a line feed.
	string : Str, Version -> Str
	string = |text, version|
		if text.contains("\n") {
			"\"\"\"\n${escaped(text, version, MultiLine)}\"\"\""
		} else {
			"\"${escaped(text, version, SingleLine)}\""
		}

	## Bare when non-empty and only `A-Za-z0-9_-`, else a quoted basic string.
	key : Str, Version -> Str
	key = |name, version|
		if !name.is_empty() and name.to_utf8().all(is_bare_key_byte) {
			name
		} else {
			"\"${escaped(name, version, SingleLine)}\""
		}

	## Keys joined by dots, for a header.
	dotted : List(Str), Version -> Str
	dotted = |names, version| Str.join_with(names.map(|name| TomlText.key(name, version)), ".")

	## Any value on one line; tables as inline tables.
	inline : TomlValue.Value, Version -> Str
	inline = |value, version|
		match value {
			String(text) => TomlText.string(text, version)
			Integer(number) => number.to_str()
			Float(float) => TomlValue.float_spelling(float)
			Boolean(flag) => if flag { "true" } else { "false" }
			LocalDate(date) => EncodingDate.date_text(date)
			LocalTime(time) => time_text(time, version)
			LocalDatetime(moment) => "${EncodingDate.date_text(moment.date)}T${time_text(moment.time, version)}"
			OffsetDatetime(moment) => "${EncodingDate.date_text(moment.date)}T${time_text(moment.time, version)}${EncodingDate.offset_text(moment.offset)}"
			Array(items) => "[${Str.join_with(items.map(|item| TomlText.inline(item, version)), ", ")}]"
			Table([]) => "{}"
			Table(entries) => "{ ${Str.join_with(entries.map(|entry| TomlText.key_value(entry, version)), ", ")} }"
		}

	## `key = value`.
	key_value : (Str, TomlValue.Value), Version -> Str
	key_value = |(name, value), version| "${TomlText.key(name, version)} = ${TomlText.inline(value, version)}"
}

hex_digits : List(U8)
hex_digits = "0123456789ABCDEF".to_utf8()

delete : U8
delete = 0x7F

escape_byte : U8
escape_byte = 0x1B

nibble_bits : U8
nibble_bits = 4

nibble_mask : U8
nibble_mask = 0x0F

is_bare_key_byte : U8 -> Bool
is_bare_key_byte = |byte|
	(byte >= '0' and byte <= '9') or (byte >= 'A' and byte <= 'Z') or (byte >= 'a' and byte <= 'z') or byte == '_' or byte == '-'

escaped : Str, TomlText.Version, [SingleLine, MultiLine] -> Str
escaped = |text, version, form| {
	bytes = text.to_utf8()
	if bytes.any(|byte| byte_text(byte, version, form) != [byte]) {
		Str.from_utf8_lossy(bytes.fold([], |out, byte| out.concat(byte_text(byte, version, form))))
	} else {
		text
	}
}

byte_text : U8, TomlText.Version, [SingleLine, MultiLine] -> List(U8)
byte_text = |byte, version, form|
	match byte {
		'"' => "\\\"".to_utf8()
		'\\' => "\\\\".to_utf8()
		'\n' => if form == MultiLine { ['\n'] } else { "\\n".to_utf8() }
		'\t' => "\\t".to_utf8()
		'\r' => "\\r".to_utf8()
		0x08 => "\\b".to_utf8()
		0x0C => "\\f".to_utf8()
		_ if byte < ' ' or byte == delete => control(byte, version)
		_ => [byte]
	}

## A control character without a short name.
control : U8, TomlText.Version -> List(U8)
control = |byte, version|
	match version {
		V1_0 => "\\u00".to_utf8().concat(hex(byte))
		V1_1 if byte == escape_byte => "\\e".to_utf8()
		V1_1 => "\\x".to_utf8().concat(hex(byte))
	}

hex : U8 -> List(U8)
hex = |byte| [nibble(byte.shr_wrap(nibble_bits)), nibble(byte.bitwise_and(nibble_mask))]

nibble : U8 -> U8
nibble = |value| hex_digits.get(U8.to_u64(value)) ?? '0'

## `hh:mm:ss` and any fraction; `V1_1` writes `hh:mm` when there are neither
## seconds nor a fraction.
time_text : EncodingDate.Time, TomlText.Version -> Str
time_text = |time, version| {
	is_whole_minute = time.second == 0 and EncodingDate.fraction_text(time) == ""
	if version == V1_1 and is_whole_minute {
		"${EncodingDate.pad(U8.to_u64(time.hour), clock_digits)}:${EncodingDate.pad(U8.to_u64(time.minute), clock_digits)}"
	} else {
		EncodingDate.time_text(time)
	}
}

clock_digits : U64
clock_digits = 2
