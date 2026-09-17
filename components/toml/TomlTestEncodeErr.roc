import Toml

## Every `Toml.EncodeErr`, its tag and path, through typed encoding and
## through `to_str` of a built `Value`.
TomlTestEncodeErr :: [].{}

midnight : Toml.Time
midnight = { hour: 0, minute: 0, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 }

day : Toml.Date
day = { year: 2024, month: 2, day: 29 }

## `levels` arrays nested around an empty one, under the key `a`.
nested_arrays : U64 -> Toml.Value
nested_arrays = |levels| Table([("a", List.repeat({}, levels).fold(Array([]), |inner, _| Array([inner])))])

# Dates outside TOML's years or calendar.
expect {
	date : Toml.LocalDate
	date = { year: 10000, month: 1, day: 1 }
	Toml.encode({ when: date }) == Err(InvalidDate({ path: [Key("when")], date: { year: 10000, month: 1, day: 1 } }))
}
expect {
	date : Toml.LocalDate
	date = { year: 2023, month: 2, day: 29 }
	Toml.encode_value({ days: [date] }) == Err(InvalidDate({ path: [Key("days"), Index(0)], date: { year: 2023, month: 2, day: 29 } }))
}
expect {
	moment : Toml.LocalDatetime
	moment = { date: { year: -1, month: 1, day: 1 }, time: midnight }
	Toml.encode({ at: moment }) == Err(InvalidDate({ path: [Key("at")], date: { year: -1, month: 1, day: 1 } }))
}

# Times: second 60, hour 24, and each sub-second field over 999.
expect {
	time : Toml.LocalTime
	time = { hour: 23, minute: 59, second: 60, millisecond: 0, microsecond: 0, nanosecond: 0 }
	Toml.encode({ t: time }) == Err(InvalidTime({ path: [Key("t")], time: { hour: 23, minute: 59, second: 60, millisecond: 0, microsecond: 0, nanosecond: 0 } }))
}
expect [{ ..midnight, hour: 24 }, { ..midnight, millisecond: 1000 }, { ..midnight, microsecond: 1000 }, { ..midnight, nanosecond: 1000 }].all(|fields| {
	moment : Toml.OffsetDatetime
	moment = { date: day, time: fields, offset: { minutes: 0 } }
	Toml.encode({ inner: { at: moment } }) == Err(InvalidTime({ path: [Key("inner"), Key("at")], time: fields }))
})

# Offsets past ±23:59; the date is checked before the time, the time before the offset.
expect {
	moment : Toml.OffsetDatetime
	moment = { date: day, time: midnight, offset: { minutes: -1440 } }
	Toml.encode({ at: Dict.from_list([("k", moment)]) }) == Err(InvalidOffset({ path: [Key("at"), Key("k")], offset: { minutes: -1440 } }))
}
expect {
	moment : Toml.OffsetDatetime
	moment = { date: { ..day, month: 13 }, time: { ..midnight, second: 60 }, offset: { minutes: 1440 } }
	match Toml.encode({ at: moment }) {
		Err(InvalidDate(_)) => True
		_ => False
	}
}

# Integers past `I64`, with their text.
expect Toml.encode({ n: 9223372036854775808.U64 }) == Err(IntegerOutOfRange({ path: [Key("n")], value: "9223372036854775808" }))
expect Toml.encode({ n: [U128.highest] }) == Err(IntegerOutOfRange({ path: [Key("n"), Index(0)], value: "340282366920938463463374607431768211455" }))
expect Toml.encode_value({ n: -9223372036854775809.I128 }) == Err(IntegerOutOfRange({ path: [Key("n")], value: "-9223372036854775809" }))
expect Toml.encode({ a: 9223372036854775807.U64, b: -9223372036854775808.I128 }) == Ok("a = 9223372036854775807\nb = -9223372036854775808\n")

# A document must be a table.
expect Toml.to_str(Integer(1)) == Err(RootNotATable)
expect Toml.encode([1.U8]) == Err(RootNotATable)
expect Toml.encode_value([1.U8]) == Ok(Array([Integer(1)]))

# Duplicate keys in a built value, at the second occurrence.
expect Toml.to_str(Table([("a", Integer(1)), ("b", Table([("x", Integer(1)), ("x", Integer(2))])), ("a", Integer(3))])) == Err(DuplicateKey([Key("b"), Key("x")]))
expect Toml.to_str_with(Table([("t", Array([Table([]), Table([("k", Integer(1)), ("k", Integer(1))])]))]), { version: V1_1 }) == Err(DuplicateKey([Key("t"), Index(1), Key("k")]))
expect {
	raw : Toml.Value
	raw = Table([("a", Integer(1)), ("a", Integer(2))])
	Toml.encode_value({ raw: raw }) == Err(DuplicateKey([Key("raw"), Key("a")]))
}

# Invalid dates inside a built value.
expect Toml.to_str(Table([("x", Array([LocalTime({ ..midnight, minute: 60 })]))])) == Err(InvalidTime({ path: [Key("x"), Index(0)], time: { ..midnight, minute: 60 } }))

# Nesting: the root is level 1, so 127 arrays under a key reach 128 and one more is too deep.
expect Toml.to_str(nested_arrays(126)).is_ok()
expect {
	deep = nested_arrays(127)
	path = [Key("a")].concat(List.repeat(Index(0), 127))
	Toml.to_str(deep) == Err(TooDeep(path))
}
expect {
	deep = nested_arrays(127)
	path = [Key("raw"), Key("a")].concat(List.repeat(Index(0), 126))
	Toml.encode_value({ raw: deep }) == Err(TooDeep(path))
}

# A typed value nests only as its type does: 127 lists under a key fit, 128 do not.
# (Spelled out: aliases doubling the nesting take the compiler exponential time.)
Fits : { a : List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(U8))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

Deeper : { a : List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(List(U8)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

expect {
	fits : Fits
	fits = { a: [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[1]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]] }
	Toml.encode(fits).is_ok()
}
expect {
	deep : Deeper
	deep = { a: [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[1]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]] }
	Toml.encode_value(deep) == Err(TooDeep([Key("a")].concat(List.repeat(Index(0), 127))))
}
