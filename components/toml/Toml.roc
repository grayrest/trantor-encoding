import EncodingDate
import EncodingPath
import EncodingText
import TomlCheck
import TomlCursor
import TomlDocument
import TomlEncode
import TomlFormat
import TomlParse
import TomlText
import TomlValue
import TomlWrite

## TOML 1.1.0, which reads every 1.0 document too, and writes 1.0 (or 1.1).
##
## ```roc
## config = Toml.parse("[server]\nport = 8080")
## config == Ok(Table([("server", Table([("port", Integer(8080))]))]))
##
## server : Try({ server : { port : U16 } }, _)
## server = Toml.decode("[server]\nport = 8080")
## Toml.encode({ server: { port: 8080.U16 } }) == Ok("[server]\nport = 8080\n")
## ```
##
## Lines are 1-based and columns count code points; a leading byte-order mark
## is skipped and CRLF is accepted. Reading stops at the first error, and so
## does writing. Written text is byte-stable within a major version; error
## message text is not promised.
Toml :: [].{

	## One step into a document: a table key, or an index into an array or an
	## array of tables.
	Segment : EncodingPath.Segment

	Date : EncodingDate.Date

	## Fraction digits past nanoseconds are cut off when read.
	Time : EncodingDate.Time

	Offset : EncodingDate.Offset

	## A parsed document or any value in it. Tables keep document order; `==`
	## compares tables by their keys in any order and floats by `F64`, NaN
	## equal to NaN. As a typed field it holds whatever is there; only TOML's
	## format reads and writes it.
	Value : TomlValue.Value

	## A float with its spelling kept: `to_f64`, `to_dec`, `==` by `F64`.
	Float : TomlValue.Float

	## `Syntax`, `DuplicateKey` (at the second definition, with the key's full
	## path), `OutOfRange` (an integer past `I64`, or a date, time or offset
	## past its fields' ranges; `text` is the literal) and `TooDeep` (a table
	## or array nested past 128 levels, the root table being the first).
	Err : TomlParse.Err

	## Writing failures: `InvalidDate`, `InvalidTime` (second 60 or a
	## sub-second field over 999 included) and `InvalidOffset` (past ±23:59)
	## where TOML cannot hold the value; `IntegerOutOfRange` past `I64`;
	## `RootNotATable`; and in a `Value`, `DuplicateKey` and `TooDeep` (nesting
	## past 128 levels, which a typed value can reach too).
	EncodeErr : TomlCheck.EncodeErr

	## Which TOML a writer may produce: `V1_0` (what `to_str` and `encode`
	## write) or `V1_1`, which adds `\e` and `\xHH` escapes and leaves out
	## seconds that are zero with no fraction.
	Write : { version : [V1_0, V1_1] }

	## The document as a table.
	parse : Str -> Try(Value, Err)
	parse = |text| TomlParse.parse(text)

	## A document that keeps its text: comments, blank lines, line endings, a
	## byte-order mark, spellings and table styles. `doc.to_str()` gives the
	## text back byte for byte until it is edited, `doc.to_value()` the table
	## `parse` reads, and `doc.get(path)` one value (`[]` for the root).
	## `doc.set(path, value)`, `doc.set_with(path, value, edit)`,
	## `doc.remove(path)` and `doc.append(path, value)` edit it: what is there
	## keeps its layout, what an edit creates is written as `Toml.to_str_with`
	## writes values (TOML 1.0 unless `edit` says 1.1). A table made only of
	## dotted keys, or only named by deeper headers, goes away with its last
	## key.
	Document : TomlDocument.Document

	## `NotFound`, `NotATable` and `NotAnArray` with the path where the lookup
	## stopped; `StyleNotPossible` for a table style its place cannot have;
	## `Encode` for a value that cannot be written.
	EditErr : TomlDocument.EditErr

	## How an edit writes what it creates: `version` (`V1_0` unless given) and
	## `table`, the style of a new table (`Auto` unless given). A value of it
	## kept in a variable needs the annotation `e : Toml.Edit`.
	Edit : TomlDocument.Edit

	## A document, refused as `parse` refuses its text.
	parse_document : Str -> Try(Document, Err)
	parse_document = |text| TomlDocument.parse(text)

	## A one-line message: `line L, column C: …`.
	err_to_str : Err -> Str
	err_to_str = |problem| {
		prefix = |line, column| EncodingText.position_prefix(line, column)
		match problem {
			Syntax(syntax) => "${prefix(syntax.line, syntax.column)}expected ${syntax.expected}"
			DuplicateKey(duplicate) => "${prefix(duplicate.line, duplicate.column)}the key ${path_text(duplicate.path)} is already defined"
			OutOfRange(range) => "${prefix(range.line, range.column)}${range.text} is out of range"
			TooDeep(deep) => "${prefix(deep.line, deep.column)}nesting deeper than 128 levels"
		}
	}

	## Keys as a path.
	path : List(Str) -> List(Segment)
	path = |keys| keys.map(|key| Key(key))

	a.Parseable(errs) :
		where [
			a.parser_for : TomlFormat.Format -> (TomlCursor.DecodeState -> Try({ value : a, rest : TomlCursor.DecodeState }, errs)),
		]

	## `parse`, then `decode_value`.
	decode : Str -> Try(a, [Parse(Err), Mismatch({ path : List(Segment), expected : Str }), MissingRequiredField(Str), ..errs])
		where [a.Parseable([Parse(Err), Mismatch({ path : List(Segment), expected : Str }), MissingRequiredField(Str), ..errs])]
	decode = |text| {
		value = Toml.parse(text) ? |problem| Parse(problem)
		Toml.decode_value(value)
	}

	## A value as an `a`: tables into records and `Dict(Str, _)`, arrays into
	## lists and tuples.
	decode_value : Value -> Try(a, [Mismatch({ path : List(Segment), expected : Str }), MissingRequiredField(Str), ..errs])
		where [a.Parseable([Mismatch({ path : List(Segment), expected : Str }), MissingRequiredField(Str), ..errs])]
	decode_value = |value| EncodingPath.run(TomlFormat.Format.Default, TomlCursor.start(value))

	a.Encodable(err) :
		where [
			a.encoder_for : TomlEncode.Encoder -> (a, TomlEncode.EncodeState -> Try(TomlEncode.EncodeState, err)),
		]

	## An `a` as TOML 1.0 text: `encode_value`, then `to_str`.
	encode : a -> Try(Str, EncodeErr) where [a.Encodable(EncodeErr)]
	encode = |value| Toml.encode_with(value, { version: V1_0 })

	encode_with : a, Write -> Try(Str, EncodeErr) where [a.Encodable(EncodeErr)]
	encode_with = |value, write| {
		tree = Toml.encode_value(value)?
		Toml.to_str_with(tree, write)
	}

	## An `a` as a value: records and `Dict(Str, _)` as tables, lists and tuples
	## as arrays. Optional (`?:`) fields are left out when absent.
	encode_value : a -> Try(Value, EncodeErr) where [a.Encodable(EncodeErr)]
	encode_value = |value| EncodingPath.encode_run(TomlEncode.Encoder.Default, TomlEncode.start, value).map_ok(TomlEncode.result)

	## A document as TOML 1.0 text (`to_str_with` for 1.1).
	to_str : Value -> Try(Str, EncodeErr)
	to_str = |value| Toml.to_str_with(value, { version: V1_0 })

	to_str_with : Value, Write -> Try(Str, EncodeErr)
	to_str_with = |value, write|
		match value {
			Table(entries) => {
				TomlCheck.value_at(value, [])?
				Ok(TomlWrite.document(entries, write.version))
			}
			_ => Err(RootNotATable)
		}

	## A local date, decoded and encoded through any format with the date
	## methods (TOML's and CSV's).
	LocalDate := { year : I32, month : U8, day : U8 }.{
		parser_for : format -> (state -> Try({ value : LocalDate, rest : state }, [Mismatch({ path : List(Segment), expected : Str })]))
			where [format.parse_local_date : format, state -> Try({ value : Date, rest : state }, [Mismatch({ path : List([Key(Str), Index(U64)]), expected : Str })])]
		parser_for = |format| |state| {
			parsed = format.parse_local_date(state) ? |Mismatch(problem)| Mismatch(problem)
			Ok({ value: parsed.value, rest: parsed.rest })
		}

		encoder_for : encoder -> (LocalDate, state -> Try(state, err))
			where [encoder.encode_local_date : encoder, Date, state -> Try(state, err)]
		encoder_for = |encoder| |date, state| encoder.encode_local_date({ year: date.year, month: date.month, day: date.day }, state)

		is_eq : LocalDate, LocalDate -> Bool
		is_eq = |left, right| left.year == right.year and left.month == right.month and left.day == right.day
	}

	## A local time, through any format with the date methods.
	LocalTime := { hour : U8, minute : U8, second : U8, millisecond : U16, microsecond : U16, nanosecond : U16 }.{
		parser_for : format -> (state -> Try({ value : LocalTime, rest : state }, [Mismatch({ path : List(Segment), expected : Str })]))
			where [format.parse_local_time : format, state -> Try({ value : Time, rest : state }, [Mismatch({ path : List([Key(Str), Index(U64)]), expected : Str })])]
		parser_for = |format| |state| {
			parsed = format.parse_local_time(state) ? |Mismatch(problem)| Mismatch(problem)
			Ok({ value: parsed.value, rest: parsed.rest })
		}

		encoder_for : encoder -> (LocalTime, state -> Try(state, err))
			where [encoder.encode_local_time : encoder, Time, state -> Try(state, err)]
		encoder_for = |encoder| |time, state| encoder.encode_local_time(time_fields(time), state)

		is_eq : LocalTime, LocalTime -> Bool
		is_eq = |left, right| time_fields(left) == time_fields(right)
	}

	## A local date and time, through any format with the date methods.
	LocalDatetime := { date : Date, time : Time }.{
		parser_for : format -> (state -> Try({ value : LocalDatetime, rest : state }, [Mismatch({ path : List(Segment), expected : Str })]))
			where [format.parse_local_datetime : format, state -> Try({ value : { date : Date, time : Time }, rest : state }, [Mismatch({ path : List([Key(Str), Index(U64)]), expected : Str })])]
		parser_for = |format| |state| {
			parsed = format.parse_local_datetime(state) ? |Mismatch(problem)| Mismatch(problem)
			Ok({ value: parsed.value, rest: parsed.rest })
		}

		encoder_for : encoder -> (LocalDatetime, state -> Try(state, err))
			where [encoder.encode_local_datetime : encoder, { date : Date, time : Time }, state -> Try(state, err)]
		encoder_for = |encoder| |moment, state| encoder.encode_local_datetime({ date: moment.date, time: moment.time }, state)

		is_eq : LocalDatetime, LocalDatetime -> Bool
		is_eq = |left, right| left.date == right.date and left.time == right.time
	}

	## A date and time at an offset from UTC, through any format with the date
	## methods.
	OffsetDatetime := { date : Date, time : Time, offset : Offset }.{
		parser_for : format -> (state -> Try({ value : OffsetDatetime, rest : state }, [Mismatch({ path : List(Segment), expected : Str })]))
			where [format.parse_offset_datetime : format, state -> Try({ value : { date : Date, time : Time, offset : Offset }, rest : state }, [Mismatch({ path : List([Key(Str), Index(U64)]), expected : Str })])]
		parser_for = |format| |state| {
			parsed = format.parse_offset_datetime(state) ? |Mismatch(problem)| Mismatch(problem)
			Ok({ value: parsed.value, rest: parsed.rest })
		}

		encoder_for : encoder -> (OffsetDatetime, state -> Try(state, err))
			where [encoder.encode_offset_datetime : encoder, { date : Date, time : Time, offset : Offset }, state -> Try(state, err)]
		encoder_for = |encoder| |moment, state| encoder.encode_offset_datetime({ date: moment.date, time: moment.time, offset: moment.offset }, state)

		is_eq : OffsetDatetime, OffsetDatetime -> Bool
		is_eq = |left, right| left.date == right.date and left.time == right.time and left.offset == right.offset
	}

	## The shortest spelling that reads back as `value` (`1.0`, `1e300`,
	## `inf`, `nan`).
	float_from_f64 : F64 -> Float
	float_from_f64 = |value| TomlValue.float_from_f64(value)

	## `value`'s exact decimal text (`5.0`, `-0.25`), so `to_dec` gives it back.
	float_from_dec : Dec -> Float
	float_from_dec = |value| TomlValue.float_from_dec(value)
}

time_fields : Toml.LocalTime -> EncodingDate.Time
time_fields = |time| {
	hour: time.hour,
	minute: time.minute,
	second: time.second,
	millisecond: time.millisecond,
	microsecond: time.microsecond,
	nanosecond: time.nanosecond,
}

## `a.b[2]."c d"`: keys quoted unless bare, indices in brackets.
path_text : List(EncodingPath.Segment) -> Str
path_text = |segments|
	segments.fold_with_index("", |text, segment, position|
		match segment {
			Index(index) => "${text}[${index.to_str()}]"
			Key(key) if position == 0 => key_text(key)
			Key(key) => "${text}.${key_text(key)}"
		})

## Spelled as a key is written, control characters escaped, so a message
## stays on one line.
key_text : Str -> Str
key_text = |key| TomlText.key(key, V1_0)
