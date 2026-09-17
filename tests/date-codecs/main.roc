app [main!] { pf: platform "../target/trantor/app/platform/main.roc" }

import pf.OsStr
import pf.Stdout
import pf.Csv
import pf.Toml

## `Toml`'s four date types through CSV records (D-S3-55.7): decoded from XML
## Schema cells, written back, and refused with CSV's paths. `csv` and `toml`
## never import each other, so the check lives in an app over both.

Row : { d : Toml.LocalDate, t : Toml.LocalTime, l : Toml.LocalDatetime, o : Toml.OffsetDatetime }

text : Str
text = "d,l,o,t\n2026-03-08,2026-03-08T12:00:00,-0044-03-15T12:00:00.5-00:30,07:32:05.25\n10000-01-01,0000-01-01T00:00:00,2026-03-08T12:00:00-00:00,23:59:59.1234567899"

row : Row
row = {
	d: { year: 10000, month: 1, day: 1 },
	t: { hour: 7, minute: 32, second: 5, millisecond: 250, microsecond: 0, nanosecond: 0 },
	l: { date: { year: 2026, month: 3, day: 8 }, time: { hour: 12, minute: 0, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 } },
	o: { date: { year: -44, month: 3, day: 15 }, time: { hour: 12, minute: 0, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 }, offset: { minutes: 840 } },
}

## CSV text on one line, records separated by ` | `.
shown : Str -> Str
shown = |csv| csv.replace_each("\n", " | ")

## The decoded records' count and what they write back.
written_back : Try(List(Row), [Parse(Csv.Err), Mismatch({ path : List(Csv.Segment), expected : Str }), MissingRequiredField(Str)]) -> Str
written_back = |rows|
	match rows {
		Ok(values) => "decoded ${values.len().to_str()} records, written back: ${shown(Csv.encode(values) ?? "failed")}"
		Err(_) => "decode failed"
	}

## The path of a refused cell.
refused_at : Try(List({ o : Toml.OffsetDatetime }), [Parse(Csv.Err), Mismatch({ path : List(Csv.Segment), expected : Str }), MissingRequiredField(Str)]) -> Str
refused_at = |rows|
	match rows {
		Err(Mismatch(problem)) => "Mismatch at ${Str.inspect(problem.path)}"
		_ => "not refused"
	}

## The path of an offset that cannot be written.
unwritable_at : Try(Str, Csv.EncodeErr) -> Str
unwritable_at = |written|
	match written {
		Err(InvalidOffset(problem)) => "InvalidOffset at ${Str.inspect(problem.path)}"
		_ => "written"
	}

decoded : {} -> Str
decoded = |{}| {
	rows : Try(List(Row), _)
	rows = Csv.decode(text)
	written_back(rows)
}

round_trip : {} -> Str
round_trip = |{}| {
	back : Try(List(Row), _)
	back = Csv.decode(Csv.encode([row]) ?? "")
	"encoded: ${shown(Csv.encode([row]) ?? "failed")}; decoded back equal: ${Str.inspect(back == Ok([row]))}"
}

refused : {} -> Str
refused = |{}| {
	rows : Try(List({ o : Toml.OffsetDatetime }), _)
	rows = Csv.decode("o\n2026-03-08T12:00:00Z\n2026-03-08T12:00:00")
	"a local cell into an offset field: ${refused_at(rows)}"
}

unwritable : {} -> Str
unwritable = |{}| {
	moments : List({ o : Toml.OffsetDatetime })
	moments = [{ o: { date: { year: 2026, month: 3, day: 8 }, time: { hour: 12, minute: 0, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 }, offset: { minutes: 900 } } }]
	"an offset of +15:00: ${unwritable_at(Csv.encode(moments))}"
}

main! : List(OsStr) => Try({}, _)
main! = |_args| Stdout.line!(Str.join_with([decoded({}), round_trip({}), refused({}), unwritable({})], "\n"))
