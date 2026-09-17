import Csv

## Depth, not behaviour: the scanner recurses per record and per byte, and
## typed reading per record, so a change that takes a recursive call out of
## tail position overflows here on sizes no other suite reaches.
CsvStress :: [].{}

record_count : U64
record_count = 20000

field_bytes : U64
field_bytes = 200000

escaped_repeats : U64
escaped_repeats = 20000

expect {
	match Csv.parse(Str.repeat("aaa,bbb,ccc\n", record_count)) {
		Ok(rows) => rows.len() == record_count
		Err(_) => False
	}
}

# One long unquoted field, one long quoted field, and a quoted field escaping a quote every few bytes.
expect Csv.parse(Str.repeat("x", field_bytes)).map_ok(|rows| rows.len()) == Ok(1)
expect Csv.parse("\"${Str.repeat("y", field_bytes)}\"").map_ok(|rows| rows.len()) == Ok(1)
expect Csv.parse("\"${Str.repeat("a\"\"b", escaped_repeats)}\"").map_ok(|rows| rows.len()) == Ok(1)

# Typed reading and writing over as many records, empty required cells included.
expect {
	rows : Try(List({ a : Str, b : U64, c ?: Str }), _)
	rows = Csv.decode("a,b,c\n${Str.repeat(",7,\n", record_count)}")
	rows.map_ok(|records| records.len()) == Ok(record_count)
}
expect {
	rows : List({ a : Str, b : U64 })
	rows = List.repeat({ a: "x,y", b: 7 }, record_count)
	Csv.encode(rows).map_ok(|text| text.count_utf8_bytes()) == Ok(3 + record_count * 8)
}
