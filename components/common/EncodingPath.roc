## Paths into a document, `Mismatch` errors, and the generic entry points the
## formats decode and encode through.
##
## `run` and `encode_run` name neither the format nor its state, so one copy
## serves every format; the format module builds its initial state and calls
## them. `expected` phrases are message text, not promised output.
EncodingPath :: [].{

	## One step into a document: a table key or record field, or an array or
	## record index.
	Segment : [Key(Str), Index(U64)]

	## A value at `path` that is not what the type needs.
	mismatch : List(Segment), Str -> [Mismatch({ path : List(Segment), expected : Str })]
	mismatch = |path, expected| Mismatch({ path, expected })

	## `mismatch` at the path a format state reports.
	mismatch_at : state, Str -> [Mismatch({ path : List(Segment), expected : Str })]
		where [state.key_path : state -> List(Segment)]
	mismatch_at = |state, expected| Mismatch({ path: state.key_path(), expected })

	## Decode an `a` from `state` with `format`.
	run : fmt, state -> Try(a, [Mismatch({ path : List(Segment), expected : Str }), MissingRequiredField(Str), ..errs])
		where [a.parser_for : fmt -> (state -> Try({ value : a, rest : state }, [Mismatch({ path : List(Segment), expected : Str }), MissingRequiredField(Str), ..errs]))]
	run = |format, state| {
		Shape : a
		parse = Shape.parser_for(format)
		parsed = parse(state)?
		Ok(parsed.value)
	}

	## Encode `value` into `state` with `format`.
	encode_run : fmt, state, a -> Try(state, err)
		where [a.encoder_for : fmt -> (a, state -> Try(state, err))]
	encode_run = |format, state, value| {
		Shape : a
		encode = Shape.encoder_for(format)
		encode(value, state)
	}

	expected_u8 : Str
	expected_u8 = "an integer from 0 to 255"

	expected_i8 : Str
	expected_i8 = "an integer from -128 to 127"

	expected_u16 : Str
	expected_u16 = "an integer from 0 to 65535"

	expected_i16 : Str
	expected_i16 = "an integer from -32768 to 32767"

	expected_u32 : Str
	expected_u32 = "an integer from 0 to 4294967295"

	expected_i32 : Str
	expected_i32 = "an integer from -2147483648 to 2147483647"

	expected_u64 : Str
	expected_u64 = "an integer from 0 to 18446744073709551615"

	expected_i64 : Str
	expected_i64 = "an integer from -9223372036854775808 to 9223372036854775807"

	expected_u128 : Str
	expected_u128 = "an integer from 0 to 340282366920938463463374607431768211455"

	expected_i128 : Str
	expected_i128 = "an integer from -170141183460469231731687303715884105728 to 170141183460469231731687303715884105727"

	expected_f32 : Str
	expected_f32 = "an F32 number"

	expected_f64 : Str
	expected_f64 = "an F64 number"

	expected_dec : Str
	expected_dec = "a Dec number within its range"

	expected_bool : Str
	expected_bool = "a boolean"

	expected_str : Str
	expected_str = "a string"

	expected_local_date : Str
	expected_local_date = "a local date"

	expected_local_time : Str
	expected_local_time = "a local time"

	expected_local_datetime : Str
	expected_local_datetime = "a local date-time"

	expected_offset_datetime : Str
	expected_offset_datetime = "an offset date-time"
}

# A test-only format: a record's fields as named text cells, each field's path
# `[Index(row), Key(name)]`. Encoding refuses a zero `U8` to carry a path out.

ProbeState := { row : U64, names : List(Str), cells : List(Str), at : U64 }.{
	key_path : ProbeState -> List(EncodingPath.Segment)
	key_path = |state| [Index(state.row), Key(state.names.get(state.at) ?? "")]
}

cell : ProbeState -> Str
cell = |state| state.cells.get(state.at) ?? ""

next_cell : ProbeState -> ProbeState
next_cell = |state| { ..state, at: state.at + 1 }

ProbeFormat := [Default].{
	rename_field : ProbeFormat, Str -> Str
	rename_field = |_, name| name

	parse_str : ProbeFormat, ProbeState -> Try({ value : Str, rest : ProbeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str })])
	parse_str = |_, state| Ok({ value: cell(state), rest: next_cell(state) })

	parse_u8 : ProbeFormat, ProbeState -> Try({ value : U8, rest : ProbeState }, [Mismatch({ path : List(EncodingPath.Segment), expected : Str })])
	parse_u8 = |_, state|
		match U8.from_str(cell(state)) {
			Ok(value) => Ok({ value, rest: next_cell(state) })
			Err(_) => Err(EncodingPath.mismatch_at(state, EncodingPath.expected_u8))
		}

	parse_record_start : ProbeFormat, ProbeState -> Try([Counted({ len : U64, rest : ProbeState }), Uncounted(ProbeState)], [Mismatch({ path : List(EncodingPath.Segment), expected : Str })])
	parse_record_start = |_, state| Ok(Uncounted(state))

	parse_record_field : ProbeFormat,
	Encoding.FieldName.FieldNames(_shape),
	ProbeState -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : ProbeState }),
			TryField({ name : Str, rest : ProbeState }),
			TryFieldCaseless({ name : Str, rest : ProbeState }),
			Continue(ProbeState),
			Done(ProbeState),
		],
		[Mismatch({ path : List(EncodingPath.Segment), expected : Str })],
	)
	parse_record_field = |_, _, state|
		match state.names.get(state.at) {
			Ok(name) => Ok(TryField({ name, rest: state }))
			Err(_) => Ok(Done(state))
		}

	parse_record_after_field : ProbeFormat, ProbeState -> Try([Continue(ProbeState), Done(ProbeState)], [Mismatch({ path : List(EncodingPath.Segment), expected : Str })])
	parse_record_after_field = |_, state| Ok(Continue(state))

	skip_record_field : ProbeFormat, ProbeState -> Try(ProbeState, [Mismatch({ path : List(EncodingPath.Segment), expected : Str })])
	skip_record_field = |_, state| Ok(next_cell(state))
}

ProbeOut := { text : Str, path : List(EncodingPath.Segment) }

ProbeFields := { text : Str, path : List(EncodingPath.Segment) }

ProbeEncoder := [Default].{
	rename_field : ProbeEncoder, Str -> Str
	rename_field = |_, name| name

	encode_str : Str, ProbeOut -> Try(ProbeOut, err)
	encode_str = |value, out| Ok({ ..out, text: "${out.text}${value};" })

	encode_u8 : U8, ProbeOut -> Try(ProbeOut, [Zero(List(EncodingPath.Segment))])
	encode_u8 = |value, out|
		if value == 0 {
			Err(Zero(out.path))
		} else {
			Ok({ ..out, text: "${out.text}${value.to_str()};" })
		}

	encode_record : ProbeOut, U64, (ProbeFields, (ProbeFields, Str, (ProbeOut -> Try(ProbeOut, err)) -> Try(ProbeFields, err)) -> Try(ProbeFields, err)) -> Try(ProbeOut, err)
	encode_record = |out, _, write_fields| {
		written = write_fields({ text: out.text, path: out.path }, |fields, name, write_value| {
			value_out = write_value({ text: "${fields.text}${name}=", path: fields.path.append(Key(name)) })?
			Ok({ text: value_out.text, path: fields.path })
		})?
		Ok({ text: written.text, path: written.path })
	}
}

probe_state : List(Str), List(Str) -> ProbeState
probe_state = |names, cells| { row: 4, names, cells, at: 0 }

expect EncodingPath.mismatch([Key("a"), Index(2)], "a string") == Mismatch({ path: [Key("a"), Index(2)], expected: "a string" })
expect {
	problem : [Mismatch({ path : List(EncodingPath.Segment), expected : Str }), MissingRequiredField(Str)]
	problem = EncodingPath.mismatch_at(probe_state(["age"], ["x"]), EncodingPath.expected_u8)
	problem == Mismatch({ path: [Index(4), Key("age")], expected: EncodingPath.expected_u8 })
}

# `run` decodes a record through a format it does not name.
expect {
	decoded : Try({ age : U8, name : Str }, _)
	decoded = EncodingPath.run(ProbeFormat.Default, probe_state(["age", "name"], ["42", "ann"]))
	decoded == Ok({ age: 42, name: "ann" })
}
expect {
	decoded : Try({ age : U8, name : Str }, _)
	decoded = EncodingPath.run(ProbeFormat.Default, probe_state(["name", "age"], ["ann", "420"]))
	match decoded {
		Err(Mismatch(problem)) => problem.path == [Index(4), Key("age")]
		_ => False
	}
}
expect {
	decoded : Try({ age : U8, name : Str }, _)
	decoded = EncodingPath.run(ProbeFormat.Default, probe_state(["name"], ["ann"]))
	decoded == Err(MissingRequiredField("age"))
}

# `encode_run` encodes a nested record and carries an error's path out.
expect {
	encoded = EncodingPath.encode_run(ProbeEncoder.Default, { text: "", path: [] }, { a: "x", b: { c: 7.U8 } })
	encoded.map_ok(|out| out.text) == Ok("a=x;b=c=7;")
}
expect {
	encoded = EncodingPath.encode_run(ProbeEncoder.Default, { text: "", path: [] }, { a: "x", b: { c: 0.U8 } })
	match encoded {
		Err(Zero(path)) => path == [Key("b"), Key("c")]
		_ => False
	}
}
