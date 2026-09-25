import EncodingDate
import EncodingNumber
import EncodingPath
import EncodingText
import TomlNumber

## `Toml.Value` and `Toml.Float`, defined below the parser so it can build
## them; `Toml` aliases both.
TomlValue :: [].{

	## A float as written (`1_000.5`, `+inf`) or as generated, with its `F64`.
	Float :: { spelling : Str, value : F64 }.{

		## The nearest `F64`; ±infinity past its range.
		to_f64 : Float -> F64
		to_f64 = |float| float.value

		## The exact decimal with digits past the 18th fractional place cut
		## toward zero; `NotADec` for `inf`, `nan` and values past `Dec`'s
		## range.
		to_dec : Float -> Try(Dec, [NotADec])
		to_dec = |float|
			match TomlNumber.float_of(float.spelling) {
				Ok(Finite(parts)) => EncodingNumber.to_dec(parts).map_err(|_| NotADec)
				_ => Err(NotADec)
			}

		## Equal `F64`s, NaN equal to NaN: `1.0` equals `1.00`.
		is_eq : Float, Float -> Bool
		is_eq = |left, right| (left.value.is_nan() and right.value.is_nan()) or left.value == right.value
	}

	## A TOML value. Tables keep document order; keys are unique when parsed.
	Value := [
		String(Str),
		Integer(I64),
		Float(Float),
		Boolean(Bool),
		OffsetDatetime({ date : EncodingDate.Date, time : EncodingDate.Time, offset : EncodingDate.Offset }),
		LocalDatetime({ date : EncodingDate.Date, time : EncodingDate.Time }),
		LocalDate(EncodingDate.Date),
		LocalTime(EncodingDate.Time),
		Array(List(Value)),
		Table(List((Str, Value))),
	].{

		## Tables equal when they hold the same keys with equal values in any
		## order (entries compared sorted by key bytes, a key held more than
		## once matching its values in any order); arrays element by element;
		## floats by `Float.is_eq`.
		is_eq : Value, Value -> Bool
		is_eq = |left, right|
			match (left, right) {
				(String(a), String(b)) => a == b
				(Integer(a), Integer(b)) => a == b
				(Float(a), Float(b)) => a == b
				(Boolean(a), Boolean(b)) => a == b
				(OffsetDatetime(a), OffsetDatetime(b)) => a == b
				(LocalDatetime(a), LocalDatetime(b)) => a == b
				(LocalDate(a), LocalDate(b)) => a == b
				(LocalTime(a), LocalTime(b)) => a == b
				(Array(a), Array(b)) => a == b
				(Table(a), Table(b)) => a.len() == b.len() and same_entries(by_key(a), by_key(b), 0)
				_ => False
			}

		## Any `Value` in a typed field, read whole. Only a format with
		## `parse_toml_value` (TOML's) has one to give.
		parser_for : fmt -> (state -> Try({ value : Value, rest : state }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str })]))
			where [fmt.parse_toml_value : fmt, state -> Try({ value : Value, rest : state }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str })])]
		parser_for = |format| |state| format.parse_toml_value(state).map_err(|Mismatch(problem)| Mismatch(problem))

		## Any `Value` in a typed field, written whole by a format with
		## `encode_toml_value` (TOML's).
		encoder_for : encoder -> (Value, state -> Try(state, err))
			where [encoder.encode_toml_value : encoder, Value, state -> Try(state, err)]
		encoder_for = |encoder| |value, state| encoder.encode_toml_value(value, state)
	}

	## A float read from a literal, keeping its spelling.
	float_read : Str, TomlNumber.FloatReading -> Float
	float_read = |spelling, reading| { spelling, value: reading_f64(reading) }

	## The shortest spelling that reads back as `value` (`1.0`, `inf`, `nan`).
	float_from_f64 : F64 -> Float
	float_from_f64 = |value| { spelling: EncodingNumber.f64_spelling(value, specials), value }

	## `value` by `F32`'s own shortest spelling (`0.1`, not the widened
	## `0.10000000149011612`), its `F64` read from that spelling as a parsed
	## literal's is, so encoding the `F32` and writing this float agree.
	float_from_f32 : F32 -> Float
	float_from_f32 = |value| float_spelled(EncodingNumber.f32_spelling(value, specials))

	## The spelling a float is written with.
	float_spelling : Float -> Str
	float_spelling = |float| float.spelling

	## `value`'s exact decimal text (`5.0`, `-0.25`).
	float_from_dec : Dec -> Float
	float_from_dec = |value| float_spelled(value.to_str())
}

## A generated spelling (always a float literal) with its `F64`.
float_spelled : Str -> TomlValue.Float
float_spelled = |spelling| { spelling, value: reading_f64(TomlNumber.float_of(spelling) ?? NotANumber) }

specials : EncodingNumber.Specials
specials = { infinity: "inf", negative_infinity: "-inf", nan: "nan" }

reading_f64 : TomlNumber.FloatReading -> F64
reading_f64 = |reading|
	match reading {
		Finite(parts) => EncodingNumber.to_f64(parts)
		Infinite(is_negative) => if is_negative { -F64.infinity } else { F64.infinity }
		NotANumber => F64.nan
	}

by_key : List((Str, TomlValue.Value)) -> List((Str, TomlValue.Value))
by_key = |entries| entries.sort_with(|(a, _), (b, _)| EncodingText.compare(a, b))

## Entries sorted by key, equal from `index` on. A key's run of entries must
## end at the same place in both; a run of more than one matches its values
## as a multiset, since sorting leaves duplicates in the order given.
same_entries : List((Str, TomlValue.Value)), List((Str, TomlValue.Value)), U64 -> Bool
same_entries = |left, right, index|
	match (left.get(index), right.get(index)) {
		(Ok((key, value)), Ok((other_key, other_value))) => {
			end = run_end(left, key, index + 1)
			if key != other_key or run_end(right, key, index + 1) != end {
				False
			} else if end == index + 1 {
				if value == other_value { same_entries(left, right, end) } else { False }
			} else if same_values(values_between(left, index, end), values_between(right, index, end)) {
				same_entries(left, right, end)
			} else {
				False
			}
		}
		_ => True
	}

## Where the run of entries with `key` that continues at `index` ends.
run_end : List((Str, TomlValue.Value)), Str, U64 -> U64
run_end = |entries, key, index|
	match entries.get(index) {
		Ok((name, _)) if name == key => run_end(entries, key, index + 1)
		_ => index
	}

values_between : List((Str, TomlValue.Value)), U64, U64 -> List(TomlValue.Value)
values_between = |entries, start, end| entries.sublist({ start, len: end - start }).map(|(_, value)| value)

## The same values with the same counts, in any order.
same_values : List(TomlValue.Value), List(TomlValue.Value) -> Bool
same_values = |left, right|
	match left {
		[] => right.is_empty()
		[first, .. as rest] =>
			match right.find_first_index(|candidate| candidate == first) {
				Ok(position) => same_values(rest, right.take_first(position).concat(right.drop_first(position + 1)))
				Err(_) => False
			}
	}
