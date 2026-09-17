import Csv
import EncodingDate

## Typed dates through CSV: each XML Schema form into each field kind, the
## refused variants with their paths, extended years both ways, golden output,
## every `Csv.EncodeErr`, and `encode_columns` with a record that cannot be
## written. The field types here are test codecs over the date contract; the
## `Toml` date types go through CSV in `tests/date-codecs`.
CsvTestDate :: [].{}

Mismatch : [Mismatch({ path : List([Key(Str), Index(U64)]), expected : Str })]

Day := { year : I32, month : U8, day : U8 }.{
	parser_for : format -> (state -> Try({ value : Day, rest : state }, [Mismatch({ path : List(Csv.Segment), expected : Str }), ..]))
		where [format.parse_local_date : format, state -> Try({ value : EncodingDate.Date, rest : state }, Mismatch)]
	parser_for = |format| |state| {
		parsed = format.parse_local_date(state) ? |Mismatch(problem)| Mismatch(problem)
		Ok({ value: { year: parsed.value.year, month: parsed.value.month, day: parsed.value.day }, rest: parsed.rest })
	}

	encoder_for : encoder -> (Day, state -> Try(state, err))
		where [encoder.encode_local_date : encoder, EncodingDate.Date, state -> Try(state, err)]
	encoder_for = |encoder| |day, state| encoder.encode_local_date({ year: day.year, month: day.month, day: day.day }, state)

	is_eq : Day, Day -> Bool
	is_eq = |left, right| left.year == right.year and left.month == right.month and left.day == right.day
}

Clock := { time : EncodingDate.Time }.{
	parser_for : format -> (state -> Try({ value : Clock, rest : state }, [Mismatch({ path : List(Csv.Segment), expected : Str }), ..]))
		where [format.parse_local_time : format, state -> Try({ value : EncodingDate.Time, rest : state }, Mismatch)]
	parser_for = |format| |state| {
		parsed = format.parse_local_time(state) ? |Mismatch(problem)| Mismatch(problem)
		Ok({ value: { time: parsed.value }, rest: parsed.rest })
	}

	encoder_for : encoder -> (Clock, state -> Try(state, err))
		where [encoder.encode_local_time : encoder, EncodingDate.Time, state -> Try(state, err)]
	encoder_for = |encoder| |clock, state| encoder.encode_local_time(clock.time, state)

	is_eq : Clock, Clock -> Bool
	is_eq = |left, right| left.time == right.time
}

Local := { date : EncodingDate.Date, time : EncodingDate.Time }.{
	parser_for : format -> (state -> Try({ value : Local, rest : state }, [Mismatch({ path : List(Csv.Segment), expected : Str }), ..]))
		where [format.parse_local_datetime : format, state -> Try({ value : { date : EncodingDate.Date, time : EncodingDate.Time }, rest : state }, Mismatch)]
	parser_for = |format| |state| {
		parsed = format.parse_local_datetime(state) ? |Mismatch(problem)| Mismatch(problem)
		Ok({ value: { date: parsed.value.date, time: parsed.value.time }, rest: parsed.rest })
	}

	encoder_for : encoder -> (Local, state -> Try(state, err))
		where [encoder.encode_local_datetime : encoder, { date : EncodingDate.Date, time : EncodingDate.Time }, state -> Try(state, err)]
	encoder_for = |encoder| |moment, state| encoder.encode_local_datetime({ date: moment.date, time: moment.time }, state)

	is_eq : Local, Local -> Bool
	is_eq = |left, right| left.date == right.date and left.time == right.time
}

Moment := { date : EncodingDate.Date, time : EncodingDate.Time, offset : EncodingDate.Offset }.{
	parser_for : format -> (state -> Try({ value : Moment, rest : state }, [Mismatch({ path : List(Csv.Segment), expected : Str }), ..]))
		where [format.parse_offset_datetime : format, state -> Try({ value : { date : EncodingDate.Date, time : EncodingDate.Time, offset : EncodingDate.Offset }, rest : state }, Mismatch)]
	parser_for = |format| |state| {
		parsed = format.parse_offset_datetime(state) ? |Mismatch(problem)| Mismatch(problem)
		Ok({ value: { date: parsed.value.date, time: parsed.value.time, offset: parsed.value.offset }, rest: parsed.rest })
	}

	encoder_for : encoder -> (Moment, state -> Try(state, err))
		where [encoder.encode_offset_datetime : encoder, { date : EncodingDate.Date, time : EncodingDate.Time, offset : EncodingDate.Offset }, state -> Try(state, err)]
	encoder_for = |encoder| |moment, state| encoder.encode_offset_datetime({ date: moment.date, time: moment.time, offset: moment.offset }, state)

	is_eq : Moment, Moment -> Bool
	is_eq = |left, right| left.date == right.date and left.time == right.time and left.offset == right.offset
}

Row : { d : Day, t : Clock, l : Local, o : Moment }

## The path of a `Mismatch`, or the error's tag.
mismatch_path : Try(List(a), [Parse(Csv.Err), Mismatch({ path : List(Csv.Segment), expected : Str }), MissingRequiredField(Str)]) -> Try(List(Csv.Segment), [Parse, Missing(Str), Decoded])
mismatch_path = |result|
	match result {
		Err(Mismatch(problem)) => Ok(problem.path)
		Err(Parse(_)) => Err(Parse)
		Err(MissingRequiredField(name)) => Err(Missing(name))
		Ok(_) => Err(Decoded)
	}

noon : EncodingDate.Time
noon = { hour: 12, minute: 0, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 }

march8 : EncodingDate.Date
march8 = { year: 2026, month: 3, day: 8 }

row : Row
row = {
	d: { year: 2026, month: 3, day: 8 },
	t: { time: { hour: 7, minute: 32, second: 5, millisecond: 250, microsecond: 0, nanosecond: 0 } },
	l: { date: { year: 2026, month: 3, day: 8 }, time: { hour: 12, minute: 0, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 } },
	o: { date: { year: 2026, month: 3, day: 8 }, time: { hour: 12, minute: 0, second: 0, millisecond: 0, microsecond: 0, nanosecond: 1 }, offset: { minutes: -480 } },
}

row_text : Str
row_text = "d,l,o,t\n2026-03-08,2026-03-08T12:00:00,2026-03-08T12:00:00.000000001-08:00,07:32:05.25"

# Each form into each field kind, and golden output for each kind.
expect {
	decoded : Try(List(Row), _)
	decoded = Csv.decode(row_text)
	decoded == Ok([row])
}
expect Csv.encode([row]) == Ok(row_text)
expect {
	back : Try(List(Row), _)
	back = Csv.decode(Csv.encode([row, row]) ?? "")
	back == Ok([row, row])
}

# Refused variants arrive as `Mismatch` at the cell.
expect ["2026-03-08Z", "2026-3-8", "08/03/2026", "02026-03-08"].all(|cell| {
	rows : Try(List({ d : Day }), _)
	rows = Csv.decode("d\n2026-03-08\n${cell}")
	mismatch_path(rows) == Ok([Index(1), Key("d")])
})
expect ["07:32", "24:00:00", "23:59:60", "07:32:00+01:00"].all(|cell| {
	rows : Try(List({ n : U8, t : Clock }), _)
	rows = Csv.decode("n,t\n1,${cell}")
	mismatch_path(rows) == Ok([Index(0), Key("t")])
})
expect ["2026-03-08 12:00:00", "2026-03-08t12:00:00", "2026-03-08T12:00:00Z"].all(|cell| {
	rows : Try(List({ l : Local }), _)
	rows = Csv.decode("l\n${cell}")
	mismatch_path(rows) == Ok([Index(0), Key("l")])
})
expect ["2026-03-08T12:00:00", "2026-03-08T12:00:00z", "2026-03-08T12:00:00+14:01"].all(|cell| {
	rows : Try(List({ o : Moment }), _)
	rows = Csv.decode("o\n${cell}")
	mismatch_path(rows) == Ok([Index(0), Key("o")])
})

# An empty date cell: absent for an optional field, a mismatch for a required one.
expect {
	rows : Try(List({ n : U8, d ?: Day }), _)
	rows = Csv.decode("n,d\n1,\n2,2026-03-08")
	match rows {
		Ok([first, second]) => first.?d ?? { year: 0, month: 1, day: 1 } == { year: 0, month: 1, day: 1 } and second.?d ?? { year: 0, month: 1, day: 1 } == { year: 2026, month: 3, day: 8 }
		_ => False
	}
}
expect {
	rows : Try(List({ d : Day }), _)
	rows = Csv.decode("d\n")
	rows == Ok([])
}
expect {
	rows : Try(List({ n : U8, d : Day }), _)
	rows = Csv.decode("n,d\n1,")
	mismatch_path(rows) == Ok([Index(0), Key("d")])
}

# Extended years both ways; a fraction longer than nanoseconds; `-00:00`.
expect {
	rows : Try(List({ d : Day }), _)
	rows = Csv.decode("d\n10000-01-01\n-0044-03-15\n0000-01-01")
	rows == Ok([{ d: { year: 10000, month: 1, day: 1 } }, { d: { year: -44, month: 3, day: 15 } }, { d: { year: 0, month: 1, day: 1 } }])
}
expect {
	days : List({ d : Day })
	days = [{ d: { year: 10000, month: 1, day: 1 } }, { d: { year: -44, month: 3, day: 15 } }, { d: { year: I32.lowest, month: 1, day: 1 } }]
	Csv.encode(days) == Ok("d\n10000-01-01\n-0044-03-15\n-2147483648-01-01")
}
expect {
	rows : Try(List({ t : Clock }), _)
	rows = Csv.decode("t\n23:59:59.9999999999")
	rows == Ok([{ t: { time: { hour: 23, minute: 59, second: 59, millisecond: 999, microsecond: 999, nanosecond: 999 } } }])
}
expect {
	rows : Try(List({ o : Moment }), _)
	rows = Csv.decode("o\n2026-03-08T12:00:00-00:00")
	match rows {
		Ok(decoded) => decoded == [{ o: { date: march8, time: noon, offset: { minutes: 0 } } }] and Csv.encode(decoded) == Ok("o\n2026-03-08T12:00:00Z")
		_ => False
	}
}

# Every `EncodeErr`, at `[Index(record), Key(column)]`.
expect {
	days : List({ d : Day })
	days = [{ d: { year: 2024, month: 2, day: 29 } }, { d: { year: 2023, month: 2, day: 29 } }]
	Csv.encode(days) == Err(InvalidDate({ path: [Index(1), Key("d")], date: { year: 2023, month: 2, day: 29 } }))
}
expect {
	clocks : List({ a : U8, t : Clock })
	clocks = [{ a: 1, t: { time: { hour: 24, minute: 0, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 } } }]
	Csv.encode(clocks) == Err(InvalidTime({ path: [Index(0), Key("t")], time: { hour: 24, minute: 0, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 } }))
}
expect {
	locals : List({ l : Local })
	locals = [{ l: { date: march8, time: { hour: 0, minute: 0, second: 0, millisecond: 1000, microsecond: 0, nanosecond: 0 } } }]
	Csv.encode(locals) == Err(InvalidTime({ path: [Index(0), Key("l")], time: { hour: 0, minute: 0, second: 0, millisecond: 1000, microsecond: 0, nanosecond: 0 } }))
}
expect {
	moments : List({ o : Moment })
	moments = [{ o: { date: march8, time: noon, offset: { minutes: 840 } } }, { o: { date: march8, time: noon, offset: { minutes: -841 } } }]
	Csv.encode_with(moments, Csv.Dialect.tsv) == Err(InvalidOffset({ path: [Index(1), Key("o")], offset: { minutes: -841 } }))
}

# The first problem wins: earlier records, then earlier fields in derive order.
expect {
	rows : List({ a : Day, b : Day })
	rows = [{ a: { year: 2026, month: 1, day: 1 }, b: { year: 2026, month: 1, day: 32 } }, { a: { year: 2026, month: 13, day: 1 }, b: march8_day }]
	Csv.encode(rows) == Err(InvalidDate({ path: [Index(0), Key("b")], date: { year: 2026, month: 1, day: 32 } }))
}
expect {
	rows : List({ a : Day, b : Day })
	rows = [{ a: { year: 2026, month: 0, day: 1 }, b: { year: 2026, month: 1, day: 32 } }]
	Csv.encode(rows) == Err(InvalidDate({ path: [Index(0), Key("a")], date: { year: 2026, month: 0, day: 1 } }))
}

# `encode_columns`: a record that cannot be written still gives its names, so
# the column checks come first and `Encode` after them.
expect {
	rows : List({ a : U8, b ?: Day })
	rows = [{ a: 1 }, { a: 2, b: { year: 2026, month: 2, day: 30 } }]
	Csv.encode_columns(rows, ["a", "b"]) == Err(Encode(InvalidDate({ path: [Index(1), Key("b")], date: { year: 2026, month: 2, day: 30 } })))
}
expect {
	rows : List({ a : U8, b ?: Day })
	rows = [{ a: 1 }, { a: 2, b: { year: 2026, month: 2, day: 30 } }]
	Csv.encode_columns(rows, ["a"]) == Err(MissingColumn("b"))
}
expect {
	rows : List({ a : U8, b ?: Day })
	rows = [{ a: 1 }, { a: 2, b: { year: 2026, month: 2, day: 30 } }]
	Csv.encode_columns(rows, ["b", "a", "c"]) == Err(UnknownColumn("c"))
}
expect {
	rows : List({ a : U8, b ?: Day })
	rows = [{ a: 1 }, { a: 2, b: march8_day }]
	Csv.encode_columns(rows, ["b", "a"]) == Ok("b,a\n,1\n2026-03-08,2")
}

march8_day : Day
march8_day = { year: 2026, month: 3, day: 8 }
