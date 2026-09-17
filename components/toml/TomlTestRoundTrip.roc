import Toml

## `decode(encode(v)) == v` in both write modes over every type TOML writes
## (`Try` fields only decode), and `Dec` exact through `encode_value` and
## `decode_value`.
TomlTestRoundTrip :: [].{}

writes : List(Toml.Write)
writes = [{ version: V1_0 }, { version: V1_1 }]

Scalars : {
	s : Str,
	b : Bool,
	u8 : U8,
	i8 : I8,
	u16 : U16,
	i16 : I16,
	u32 : U32,
	i32 : I32,
	u64 : U64,
	i64 : I64,
	u128 : U128,
	i128 : I128,
	f32 : F32,
	f64 : F64,
	dec : Dec,
}

scalar_cases : List(Scalars)
scalar_cases = [
	{ s: "", b: Bool.True, u8: 0, i8: -128, u16: 0, i16: -32768, u32: 0, i32: -2147483648, u64: 0, i64: I64.lowest, u128: 0, i128: -9223372036854775808, f32: -F32.infinity, f64: F64.lowest, dec: Dec.lowest },
	{ s: "a\"b\\c\n\r\t\u(1B)\u(7F)é😀", b: Bool.False, u8: 255, i8: 127, u16: 65535, i16: 32767, u32: 4294967295, i32: 2147483647, u64: 9223372036854775807, i64: I64.highest, u128: 9223372036854775807, i128: 9223372036854775807, f32: F32.highest, f64: 5e-324, dec: Dec.highest },
	{ s: "\n\n\"\"\"'''", b: Bool.True, u8: 7, i8: 0, u16: 1, i16: -1, u32: 2, i32: -2, u64: 3, i64: -3, u128: 4, i128: -4, f32: 0.1, f64: 0.1, dec: 12345678.123456789012345678 },
	{ s: "x", b: Bool.False, u8: 1, i8: 1, u16: 1, i16: 1, u32: 1, i32: 1, u64: 1, i64: 1, u128: 1, i128: 1, f32: 16777216.0, f64: -F64.infinity, dec: -0.000000000000000001 },
]

Moments : { d : Toml.LocalDate, t : Toml.LocalTime, l : Toml.LocalDatetime, o : Toml.OffsetDatetime }

moment_cases : List(Moments)
moment_cases = [
	{
		d: { year: 0, month: 1, day: 1 },
		t: { hour: 0, minute: 0, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 },
		l: { date: { year: 9999, month: 12, day: 31 }, time: { hour: 23, minute: 59, second: 59, millisecond: 999, microsecond: 999, nanosecond: 999 } },
		o: { date: { year: 2024, month: 2, day: 29 }, time: { hour: 12, minute: 30, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 }, offset: { minutes: -1439 } },
	},
	{
		d: { year: 2000, month: 2, day: 29 },
		t: { hour: 7, minute: 32, second: 0, millisecond: 0, microsecond: 1, nanosecond: 0 },
		l: { date: { year: 1979, month: 5, day: 27 }, time: { hour: 7, minute: 32, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 } },
		o: { date: { year: 1979, month: 5, day: 27 }, time: { hour: 0, minute: 0, second: 1, millisecond: 0, microsecond: 0, nanosecond: 0 }, offset: { minutes: 0 } },
	},
]

Shapes : {
	record : { inner : { deeper : { x : U8 } }, empty : {} },
	lists : List(List(Str)),
	none : List(U8),
	tables : List({ name : Str, tags : List(Str) }),
	tuple : (U8, Str, List(Bool)),
	dict : Dict(Str, { v : I64 }),
	raw : Toml.Value,
	values : List(Toml.Value),
}

shapes : Shapes
shapes = {
	record: { inner: { deeper: { x: 1 } }, empty: {} },
	lists: [["a"], [], ["b", "c"]],
	none: [],
	tables: [{ name: "a", tags: [] }, { name: "", tags: ["x y"] }],
	tuple: (1, "two", [Bool.True]),
	dict: Dict.from_list([("", { v: 1 }), ("b c", { v: -2 }), ("é", { v: 3 })]),
	raw: Table([("f", Float(Toml.float_from_dec(0.25))), ("a", Array([Table([("t", LocalTime({ hour: 1, minute: 2, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 }))])]))]),
	values: [Integer(1), Array([]), Table([])],
}

Optional : { a : U8, b ?: Str }

## `value` through `encode_with` and `decode` under `write`.
through_text : a, Toml.Write -> Try(a, [Encode, Decode]) where [a.Toml.Encodable(Toml.EncodeErr), a.Toml.Parseable([Parse(Toml.Err), Mismatch({ path : List(Toml.Segment), expected : Str }), MissingRequiredField(Str)])]
through_text = |value, write| {
	text = Toml.encode_with(value, write) ? |_| Encode
	Toml.decode(text).map_err(|_| Decode)
}

expect writes.all(|write| scalar_cases.all(|case| through_text(case, write) == Ok(case)))
expect writes.all(|write| moment_cases.all(|case| through_text(case, write) == Ok(case)))
expect writes.all(|write| through_text(shapes, write) == Ok(shapes))

# Negative zero keeps its sign, which `==` cannot see.
expect writes.all(|write| {
	back : Try({ x : F64, y : F32 }, _)
	back = through_text({ x: -0.0, y: -0.0 }, write)
	match back {
		Ok(record) => record.x.to_str() == "-0" and record.y.to_str() == "-0"
		Err(_) => False
	}
})
expect writes.all(|write| {
	back : Try({ x : F64, y : F32 }, _)
	back = through_text({ x: F64.nan, y: F32.nan }, write)
	match back {
		Ok(record) => record.x.is_nan() and record.y.is_nan()
		Err(_) => False
	}
})

# Optional fields: written when present, left out when absent.
expect writes.all(|write| {
	cases : List(Optional)
	cases = [{ a: 1, b: "x" }, { a: 2 }]
	back : Try(List(Optional), _)
	back = cases.map_try(|case| through_text(case, write))
	match back {
		Ok([first, second]) => first.?b ?? "" == "x" and second.?b ?? "absent" == "absent" and second.a == 2
		_ => False
	}
})

# `Dec` past 17 digits exact through a value too.
expect {
	numbers = { a: 12345678.123456789012345678.Dec, b: 170141183460469231731.687303715884105727.Dec }
	match Toml.encode_value(numbers) {
		Ok(value) => {
			back : Try({ a : Dec, b : Dec }, _)
			back = Toml.decode_value(value)
			back == Ok(numbers)
		}
		Err(_) => False
	}
}
