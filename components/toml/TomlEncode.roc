import EncodingDate
import EncodingPath
import EncodingText
import TomlCheck
import TomlValue

## Typed writing into a `Toml.Value`: the format the derived encoders call.
##
## Records and `Dict(Str, _)` become tables (a dict's keys in byte order),
## lists and tuples arrays, `F32` keeps its own shortest spelling and `Dec` its
## exact decimal text. Integers past `I64`, invalid dates, times and offsets,
## nesting past 128 levels and a `Toml.Value` field that cannot be written stop
## encoding with the path where they were found.
TomlEncode :: [].{

	## The value a field's writer produced, and where that field is.
	EncodeState :: { value : TomlValue.Value, path : List(EncodingPath.Segment) }

	## A table's entries or an array's items while they are written.
	ContainerState :: { entries : List((Str, TomlValue.Value)), items : List(TomlValue.Value), path : List(EncodingPath.Segment) }

	## Nothing written yet, at the root.
	start : EncodeState
	start = { value: Table([]), path: [] }

	## What was written.
	result : EncodeState -> TomlValue.Value
	result = |state| state.value

	Encoder := [Default].{
		rename_field : Encoder, Str -> Str
		rename_field = |_, name| name

		encode_str : Str, EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_str = |text, state| Ok(written(state, String(text)))

		encode_bool : Bool, EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_bool = |flag, state| Ok(written(state, Boolean(flag)))

		encode_u8 : U8, EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_u8 = |number, state| Ok(written(state, Integer(U8.to_i64(number))))

		encode_i8 : I8, EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_i8 = |number, state| Ok(written(state, Integer(I8.to_i64(number))))

		encode_u16 : U16, EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_u16 = |number, state| Ok(written(state, Integer(U16.to_i64(number))))

		encode_i16 : I16, EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_i16 = |number, state| Ok(written(state, Integer(I16.to_i64(number))))

		encode_u32 : U32, EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_u32 = |number, state| Ok(written(state, Integer(U32.to_i64(number))))

		encode_i32 : I32, EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_i32 = |number, state| Ok(written(state, Integer(I32.to_i64(number))))

		encode_u64 : U64, EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_u64 = |number, state| integer(state, U64.to_i64_try(number), number.to_str())

		encode_i64 : I64, EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_i64 = |number, state| Ok(written(state, Integer(number)))

		encode_u128 : U128, EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_u128 = |number, state| integer(state, U128.to_i64_try(number), number.to_str())

		encode_i128 : I128, EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_i128 = |number, state| integer(state, I128.to_i64_try(number), number.to_str())

		encode_f32 : F32, EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_f32 = |number, state| Ok(written(state, Float(TomlValue.float_from_f32(number))))

		encode_f64 : F64, EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_f64 = |number, state| Ok(written(state, Float(TomlValue.float_from_f64(number))))

		encode_dec : Dec, EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_dec = |number, state| Ok(written(state, Float(TomlValue.float_from_dec(number))))

		encode_local_date : Encoder, EncodingDate.Date, EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_local_date = |_, date, state| {
			TomlCheck.date_at(date, state.path)?
			Ok(written(state, LocalDate(date)))
		}

		encode_local_time : Encoder, EncodingDate.Time, EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_local_time = |_, time, state| {
			TomlCheck.time_at(time, state.path)?
			Ok(written(state, LocalTime(time)))
		}

		encode_local_datetime : Encoder, { date : EncodingDate.Date, time : EncodingDate.Time }, EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_local_datetime = |_, moment, state| {
			value = LocalDatetime(moment)
			TomlCheck.value_at(value, state.path)?
			Ok(written(state, value))
		}

		encode_offset_datetime : Encoder, { date : EncodingDate.Date, time : EncodingDate.Time, offset : EncodingDate.Offset }, EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_offset_datetime = |_, moment, state| {
			value = OffsetDatetime(moment)
			TomlCheck.value_at(value, state.path)?
			Ok(written(state, value))
		}

		## A `Toml.Value` field, checked as `to_str` checks a document.
		encode_toml_value : Encoder, TomlValue.Value, EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_toml_value = |_, value, state| {
			TomlCheck.value_at(value, state.path)?
			Ok(written(state, value))
		}

		encode_key_str : Encoder, Str, EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_key_str = |_, key, state| Ok(written(state, String(key)))

		encode_record : EncodeState, U64, (ContainerState, (ContainerState, Str, (EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)) -> Try(ContainerState, TomlCheck.EncodeErr)) -> Try(ContainerState, TomlCheck.EncodeErr)) -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_record = |state, _, write_fields| {
			TomlCheck.container_at(state.path)?
			fields = write_fields(opened(state), |container, name, write_value| {
				field = write_value(inside(container, Key(name)))?
				Ok({ ..container, entries: container.entries.append((name, field.value)) })
			})?
			Ok(written(state, Table(fields.entries)))
		}

		encode_list : EncodeState, U64, (ContainerState, (ContainerState, (EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)) -> Try(ContainerState, TomlCheck.EncodeErr)) -> Try(ContainerState, TomlCheck.EncodeErr)) -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_list = |state, _, write_items| array(state, write_items)

		encode_tuple : EncodeState, U64, (ContainerState, (ContainerState, (EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)) -> Try(ContainerState, TomlCheck.EncodeErr)) -> Try(ContainerState, TomlCheck.EncodeErr)) -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_tuple = |state, _, write_items| array(state, write_items)

		## Entries in byte order of their keys, so equal dicts write the same.
		encode_dict : EncodeState, U64, (ContainerState, (ContainerState, (EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)), (EncodeState -> Try(EncodeState, TomlCheck.EncodeErr)) -> Try(ContainerState, TomlCheck.EncodeErr)) -> Try(ContainerState, TomlCheck.EncodeErr)) -> Try(EncodeState, TomlCheck.EncodeErr)
		encode_dict = |state, _, write_entries| {
			TomlCheck.container_at(state.path)?
			entries = write_entries(opened(state), |container, write_key, write_value| {
				key = write_key({ value: Table([]), path: container.path })?
				name = key_name(key.value)
				entry = write_value(inside(container, Key(name)))?
				Ok({ ..container, entries: container.entries.append((name, entry.value)) })
			})?
			sorted = entries.entries.sort_with(|(left, _), (right, _)| EncodingText.compare(left, right))
			Ok(written(state, Table(sorted)))
		}
	}
}

written : TomlEncode.EncodeState, TomlValue.Value -> TomlEncode.EncodeState
written = |state, value| { ..state, value }

opened : TomlEncode.EncodeState -> TomlEncode.ContainerState
opened = |state| { entries: [], items: [], path: state.path }

inside : TomlEncode.ContainerState, EncodingPath.Segment -> TomlEncode.EncodeState
inside = |container, segment| { value: Table([]), path: container.path.append(segment) }

key_name : TomlValue.Value -> Str
key_name = |value|
	match value {
		String(name) => name
		_ => ""
	}

integer : TomlEncode.EncodeState, Try(I64, [OutOfRange, ..]), Str -> Try(TomlEncode.EncodeState, TomlCheck.EncodeErr)
integer = |state, converted, text|
	match converted {
		Ok(number) => Ok(written(state, Integer(number)))
		Err(_) => Err(IntegerOutOfRange({ path: state.path, value: text }))
	}

array : TomlEncode.EncodeState, (TomlEncode.ContainerState, (TomlEncode.ContainerState, (TomlEncode.EncodeState -> Try(TomlEncode.EncodeState, TomlCheck.EncodeErr)) -> Try(TomlEncode.ContainerState, TomlCheck.EncodeErr)) -> Try(TomlEncode.ContainerState, TomlCheck.EncodeErr)) -> Try(TomlEncode.EncodeState, TomlCheck.EncodeErr)
array = |state, write_items| {
	TomlCheck.container_at(state.path)?
	items = write_items(opened(state), |container, write_value| {
		item = write_value(inside(container, Index(container.items.len())))?
		Ok({ ..container, items: container.items.append(item.value) })
	})?
	Ok(written(state, Array(items.items)))
}
