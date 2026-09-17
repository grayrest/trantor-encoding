import EncodingDate
import EncodingNumber
import EncodingPath
import TomlCursor
import TomlNumber
import TomlValue

## Typed reading over a `Toml.Value`: the format the derived parsers call.
##
## Walking tables and arrays is `TomlCursor`'s. Integers narrow into any width
## that holds them and into `F32`/`F64`/`Dec` when exact; floats go to
## `F32`/`F64` (±infinity past the range) and to `Dec` from their spelling;
## a float into an integer, or any other kind, is `Mismatch` at the path.
TomlFormat :: [].{

	DecodeState : TomlCursor.DecodeState

	Format := [Default].{
		rename_field : Format, Str -> Str
		rename_field = |_, name| name

		parse_str : Format, DecodeState -> Try({ value : Str, rest : DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_str = |_, state|
			match state.current {
				String(text) => Ok({ value: text, rest: state })
				_ => Err(EncodingPath.mismatch_at(state, EncodingPath.expected_str))
			}

		parse_bool : Format, DecodeState -> Try({ value : Bool, rest : DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_bool = |_, state|
			match state.current {
				Boolean(flag) => Ok({ value: flag, rest: state })
				_ => Err(EncodingPath.mismatch_at(state, EncodingPath.expected_bool))
			}

		parse_u8 : Format, DecodeState -> Try({ value : U8, rest : DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_u8 = |_, state| unsigned(state, U128.to_u8_try, EncodingPath.expected_u8)

		parse_i8 : Format, DecodeState -> Try({ value : I8, rest : DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_i8 = |_, state| signed(state, I128.to_i8_try, EncodingPath.expected_i8)

		parse_u16 : Format, DecodeState -> Try({ value : U16, rest : DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_u16 = |_, state| unsigned(state, U128.to_u16_try, EncodingPath.expected_u16)

		parse_i16 : Format, DecodeState -> Try({ value : I16, rest : DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_i16 = |_, state| signed(state, I128.to_i16_try, EncodingPath.expected_i16)

		parse_u32 : Format, DecodeState -> Try({ value : U32, rest : DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_u32 = |_, state| unsigned(state, U128.to_u32_try, EncodingPath.expected_u32)

		parse_i32 : Format, DecodeState -> Try({ value : I32, rest : DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_i32 = |_, state| signed(state, I128.to_i32_try, EncodingPath.expected_i32)

		parse_u64 : Format, DecodeState -> Try({ value : U64, rest : DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_u64 = |_, state| unsigned(state, U128.to_u64_try, EncodingPath.expected_u64)

		parse_i64 : Format, DecodeState -> Try({ value : I64, rest : DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_i64 = |_, state| signed(state, I128.to_i64_try, EncodingPath.expected_i64)

		parse_u128 : Format, DecodeState -> Try({ value : U128, rest : DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_u128 = |_, state| unsigned(state, |size| Ok(size), EncodingPath.expected_u128)

		parse_i128 : Format, DecodeState -> Try({ value : I128, rest : DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_i128 = |_, state| signed(state, |value| Ok(value), EncodingPath.expected_i128)

		parse_f32 : Format, DecodeState -> Try({ value : F32, rest : DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_f32 = |_, state|
			match state.current {
				Float(float) => Ok({ value: f32_of(float), rest: state })
				Integer(integer) => exact(state, integer, I64.to_f32(integer), F32.to_i64_try, EncodingPath.expected_f32)
				_ => Err(EncodingPath.mismatch_at(state, EncodingPath.expected_f32))
			}

		parse_f64 : Format, DecodeState -> Try({ value : F64, rest : DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_f64 = |_, state|
			match state.current {
				Float(float) => Ok({ value: float.to_f64(), rest: state })
				Integer(integer) => exact(state, integer, I64.to_f64(integer), F64.to_i64_try, EncodingPath.expected_f64)
				_ => Err(EncodingPath.mismatch_at(state, EncodingPath.expected_f64))
			}

		## From the float's spelling, digits past the 18th fractional place
		## cut toward zero.
		parse_dec : Format, DecodeState -> Try({ value : Dec, rest : DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_dec = |_, state|
			match state.current {
				Float(float) =>
					match float.to_dec() {
						Ok(value) => Ok({ value, rest: state })
						Err(_) => Err(EncodingPath.mismatch_at(state, EncodingPath.expected_dec))
					}
				Integer(integer) => Ok({ value: I64.to_dec(integer), rest: state })
				_ => Err(EncodingPath.mismatch_at(state, EncodingPath.expected_dec))
			}

		parse_local_date : Format, DecodeState -> Try({ value : EncodingDate.Date, rest : DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_local_date = |_, state|
			match state.current {
				LocalDate(date) => Ok({ value: date, rest: state })
				_ => Err(EncodingPath.mismatch_at(state, EncodingPath.expected_local_date))
			}

		parse_local_time : Format, DecodeState -> Try({ value : EncodingDate.Time, rest : DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_local_time = |_, state|
			match state.current {
				LocalTime(time) => Ok({ value: time, rest: state })
				_ => Err(EncodingPath.mismatch_at(state, EncodingPath.expected_local_time))
			}

		parse_local_datetime : Format, DecodeState -> Try({ value : { date : EncodingDate.Date, time : EncodingDate.Time }, rest : DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_local_datetime = |_, state|
			match state.current {
				LocalDatetime(moment) => Ok({ value: moment, rest: state })
				_ => Err(EncodingPath.mismatch_at(state, EncodingPath.expected_local_datetime))
			}

		parse_offset_datetime : Format, DecodeState -> Try({ value : { date : EncodingDate.Date, time : EncodingDate.Time, offset : EncodingDate.Offset }, rest : DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_offset_datetime = |_, state|
			match state.current {
				OffsetDatetime(moment) => Ok({ value: moment, rest: state })
				_ => Err(EncodingPath.mismatch_at(state, EncodingPath.expected_offset_datetime))
			}

		## The value under the cursor, whatever it is (`Toml.Value` fields).
		parse_toml_value : Format, DecodeState -> Try({ value : TomlValue.Value, rest : DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_toml_value = |_, state| Ok({ value: state.current, rest: state })

		parse_record_start : Format, DecodeState -> Try([Counted({ len : U64, rest : DecodeState }), Uncounted(DecodeState)], [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_record_start = |_, state| TomlCursor.open_table(state)

		## Each entry in document order; a name the record lacks is skipped.
		parse_record_field : Format,
		Encoding.FieldName.FieldNames(_shape),
		DecodeState -> Try(
			[
				Field({ field : Encoding.FieldName(_shape), rest : DecodeState }),
				TryField({ name : Str, rest : DecodeState }),
				TryFieldCaseless({ name : Str, rest : DecodeState }),
				Continue(DecodeState),
				Done(DecodeState),
			],
			[Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..],
		)
		parse_record_field = |_, _, state|
			match TomlCursor.next_entry(state) {
				Entry({ name, rest }) => Ok(TryField({ name, rest }))
				Finished(rest) => Ok(Done(rest))
			}

		parse_record_after_field : Format, DecodeState -> Try([Continue(DecodeState), Done(DecodeState)], [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_record_after_field = |_, state| Ok(Continue(TomlCursor.leave(state)))

		skip_record_field : Format, DecodeState -> Try(DecodeState, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		skip_record_field = |_, state| Ok(state)

		parse_list_start : Format, DecodeState -> Try([Counted({ len : U64, rest : DecodeState }), Uncounted(DecodeState)], [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_list_start = |_, state|
			match state.current {
				Array(items) => Ok(Uncounted(TomlCursor.open_items(state, items)))
				_ => Err(EncodingPath.mismatch_at(state, TomlCursor.expected_array))
			}

		parse_list_next : Format, DecodeState -> Try([Item(DecodeState), Done(DecodeState)], [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_list_next = |_, state|
			match TomlCursor.next_item(state) {
				Ok(rest) => Ok(Item(rest))
				Err(Finished(rest)) => Ok(Done(rest))
			}

		parse_list_after_item : Format, DecodeState -> Try([Continue(DecodeState), Done(DecodeState)], [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_list_after_item = |_, state| Ok(Continue(TomlCursor.leave(state)))

		## What the derived tuple parser reports when it cannot go on.
		invalid_value : Format, DecodeState -> [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..]
		invalid_value = |_, state| EncodingPath.mismatch_at(state, expected_value)

		## An array of exactly `len` values.
		parse_tuple_start : Format, DecodeState, U64 -> Try(DecodeState, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_tuple_start = |_, state, len|
			match state.current {
				Array(items) if items.len() == len => Ok(TomlCursor.tuple_item(TomlCursor.open_items(state, items), 0))
				_ => Err(EncodingPath.mismatch_at(state, "an array of ${len.to_str()} values"))
			}

		parse_tuple_next : Format, DecodeState, U64, U64 -> Try(DecodeState, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_tuple_next = |_, state, index, _| Ok(TomlCursor.tuple_item(TomlCursor.leave(state), index))

		parse_tuple_end : Format, DecodeState, U64 -> Try(DecodeState, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_tuple_end = |_, state, _| Ok(TomlCursor.close(TomlCursor.leave(state)))

		parse_dict_start : Format, DecodeState -> Try([Counted({ len : U64, rest : DecodeState }), Uncounted(DecodeState)], [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_dict_start = |_, state| TomlCursor.open_table(state)

		## Each entry's key comes first as the value under the cursor.
		parse_dict_next : Format, DecodeState -> Try([Entry(DecodeState), Done(DecodeState)], [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_dict_next = |_, state|
			match TomlCursor.next_entry(state) {
				Entry({ name, rest }) => Ok(Entry({ ..rest, current: String(name), path: state.path }))
				Finished(rest) => Ok(Done(rest))
			}

		parse_key_str : Format, DecodeState -> Try({ value : Str, rest : DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_key_str = |format, state| Format.parse_str(format, state)

		parse_dict_after_key : Format, DecodeState -> Try(DecodeState, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_dict_after_key = |_, state| Ok(TomlCursor.entry_value(state))

		parse_dict_after_entry : Format, DecodeState -> Try([Continue(DecodeState), Done(DecodeState)], [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
		parse_dict_after_entry = |_, state| Ok(Continue(TomlCursor.leave(state)))
	}
}

expected_value : Str
expected_value = "a value of the field's type"

integer_of : I64 -> EncodingNumber.Integer
integer_of = |value| {
	wide = I64.to_i128(value)
	{ is_negative: wide < 0, magnitude: I128.to_u128_wrap(wide.abs()) }
}

unsigned : TomlCursor.DecodeState, (U128 -> Try(n, [OutOfRange, ..])), Str -> Try({ value : n, rest : TomlCursor.DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
unsigned = |state, convert, expected|
	match state.current {
		Integer(integer) =>
			match EncodingNumber.to_unsigned(integer_of(integer), convert) {
				Ok(value) => Ok({ value, rest: state })
				Err(_) => Err(EncodingPath.mismatch_at(state, expected))
			}
		_ => Err(EncodingPath.mismatch_at(state, expected))
	}

signed : TomlCursor.DecodeState, (I128 -> Try(n, [OutOfRange, ..])), Str -> Try({ value : n, rest : TomlCursor.DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
signed = |state, convert, expected|
	match state.current {
		Integer(integer) =>
			match EncodingNumber.to_signed(integer_of(integer), convert) {
				Ok(value) => Ok({ value, rest: state })
				Err(_) => Err(EncodingPath.mismatch_at(state, expected))
			}
		_ => Err(EncodingPath.mismatch_at(state, expected))
	}

## An integer as a float only when converting back gives the same integer.
exact : TomlCursor.DecodeState, I64, f, (f -> Try(I64, [OutOfRange, ..])), Str -> Try({ value : f, rest : TomlCursor.DecodeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), ..])
exact = |state, integer, float, back, expected|
	if back(float) == Ok(integer) {
		Ok({ value: float, rest: state })
	} else {
		Err(EncodingPath.mismatch_at(state, expected))
	}

## The nearest `F32` to the float's spelling.
f32_of : TomlValue.Float -> F32
f32_of = |float|
	match TomlNumber.float_of(TomlValue.float_spelling(float)) {
		Ok(Finite(parts)) => EncodingNumber.to_f32(parts)
		Ok(Infinite(is_negative)) => if is_negative { -F32.infinity } else { F32.infinity }
		_ => F32.nan
	}
