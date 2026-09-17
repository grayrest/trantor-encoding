import Toml

## The hand-written codecs (`Toml.Value` and the four date types) inside a
## record, a list, a nested record and a `Dict` value, both ways, never only
## at the top level. The date types through CSV records are in
## `tests/date-codecs`.
TomlTestCodecs :: [].{}

date : Toml.LocalDate
date = { year: 1979, month: 5, day: 27 }

time : Toml.LocalTime
time = { hour: 7, minute: 32, second: 5, millisecond: 250, microsecond: 0, nanosecond: 0 }

local : Toml.LocalDatetime
local = { date: { year: 1979, month: 5, day: 27 }, time: { hour: 7, minute: 32, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 } }

offset : Toml.OffsetDatetime
offset = { date: { year: 1979, month: 5, day: 27 }, time: { hour: 7, minute: 32, second: 0, millisecond: 0, microsecond: 0, nanosecond: 1 }, offset: { minutes: -30 } }

raw : Toml.Value
raw = Table([("a", Array([Integer(1), String("x")])), ("t", LocalTime({ hour: 1, minute: 2, second: 3, millisecond: 0, microsecond: 0, nanosecond: 0 }))])

Record : { d : Toml.LocalDate, t : Toml.LocalTime, l : Toml.LocalDatetime, o : Toml.OffsetDatetime, v : Toml.Value }

Lists : { d : List(Toml.LocalDate), t : List(Toml.LocalTime), l : List(Toml.LocalDatetime), o : List(Toml.OffsetDatetime), v : List(Toml.Value) }

Nested : { outer : { inner : Record } }

Dicts : { d : Dict(Str, Toml.LocalDate), t : Dict(Str, Toml.LocalTime), l : Dict(Str, Toml.LocalDatetime), o : Dict(Str, Toml.OffsetDatetime), v : Dict(Str, Toml.Value) }

record : Record
record = { d: date, t: time, l: local, o: offset, v: raw }

lists : Lists
lists = { d: [date, date], t: [time], l: [local], o: [offset], v: [raw, Integer(2)] }

nested : Nested
nested = { outer: { inner: record } }

dicts : Dicts
dicts = { d: Dict.from_list([("a", date)]), t: Dict.from_list([("b", time)]), l: Dict.from_list([("c", local)]), o: Dict.from_list([("d", offset)]), v: Dict.from_list([("e", raw)]) }

writes : List(Toml.Write)
writes = [{ version: V1_0 }, { version: V1_1 }]

## Whether `value` encodes to what `text` parses to.
encodes_as : a, Str -> Bool where [a.Toml.Encodable(Toml.EncodeErr)]
encodes_as = |value, text|
	match (Toml.encode_value(value), Toml.parse(text)) {
		(Ok(encoded), Ok(parsed)) => encoded == parsed
		_ => False
	}

## Each value's literal, as the documents below spell them.
record_text : Str
record_text = "d = 1979-05-27\nt = 07:32:05.25\nl = 1979-05-27T07:32:00\no = 1979-05-27T07:32:00.000000001-00:30\nv = { a = [1, 'x'], t = 01:02:03 }"

# Decoding from hand-written documents.
expect {
	decoded : Try(Record, _)
	decoded = Toml.decode(record_text)
	decoded == Ok(record)
}
expect {
	decoded : Try(Lists, _)
	decoded = Toml.decode("d = [1979-05-27, 1979-05-27]\nt = [07:32:05.25]\nl = [1979-05-27T07:32:00]\no = [1979-05-27T07:32:00.000000001-00:30]\nv = [{ a = [1, 'x'], t = 01:02:03 }, 2]")
	decoded == Ok(lists)
}
expect {
	decoded : Try(Nested, _)
	decoded = Toml.decode("[outer.inner]\n${record_text}")
	decoded == Ok(nested)
}
expect {
	decoded : Try(Dicts, _)
	decoded = Toml.decode("d.a = 1979-05-27\nt.b = 07:32:05.25\nl.c = 1979-05-27T07:32:00\no.d = 1979-05-27T07:32:00.000000001-00:30\nv.e = { a = [1, 'x'], t = 01:02:03 }")
	decoded == Ok(dicts)
}

# Encoding into values.
expect encodes_as(record, record_text)
expect encodes_as(nested, "[outer.inner]\n${record_text}")
expect encodes_as(lists, "d = [1979-05-27, 1979-05-27]\nt = [07:32:05.25]\nl = [1979-05-27T07:32:00]\no = [1979-05-27T07:32:00.000000001-00:30]\nv = [{ a = [1, 'x'], t = 01:02:03 }, 2]")
expect encodes_as(dicts, "d.a = 1979-05-27\nt.b = 07:32:05.25\nl.c = 1979-05-27T07:32:00\no.d = 1979-05-27T07:32:00.000000001-00:30\nv.e = { a = [1, 'x'], t = 01:02:03 }")

# Through text and back, in both write modes.
expect writes.all(|write| {
	back : Try(Record, _)
	back = Toml.decode(Toml.encode_with(record, write) ?? "")
	back == Ok(record)
})
expect writes.all(|write| {
	back : Try(Lists, _)
	back = Toml.decode(Toml.encode_with(lists, write) ?? "")
	back == Ok(lists)
})
expect writes.all(|write| {
	back : Try(Nested, _)
	back = Toml.decode(Toml.encode_with(nested, write) ?? "")
	back == Ok(nested)
})
expect writes.all(|write| {
	back : Try(Dicts, _)
	back = Toml.decode(Toml.encode_with(dicts, write) ?? "")
	back == Ok(dicts)
})

# A mismatch inside each container carries its path out through the codec.
expect {
	decoded : Try(Lists, _)
	decoded = Toml.decode("d = [1979-05-27, 07:32:00]\nt = []\nl = []\no = []\nv = []")
	match decoded {
		Err(Mismatch(problem)) => problem.path == [Key("d"), Index(1)]
		_ => False
	}
}
expect {
	decoded : Try(Dicts, _)
	decoded = Toml.decode("d = {}\nt = {}\nl.c = 1979-05-27\no = {}\nv = {}")
	match decoded {
		Err(Mismatch(problem)) => problem.path == [Key("l"), Key("c")]
		_ => False
	}
}
expect {
	decoded : Try(Nested, _)
	decoded = Toml.decode("[outer.inner]\nd = 1979-05-27\nt = 07:32:05\nl = 1979-05-27T07:32:00\no = 1979-05-27T07:32:00\nv = 1")
	match decoded {
		Err(Mismatch(problem)) => problem.path == [Key("outer"), Key("inner"), Key("o")]
		_ => False
	}
}
