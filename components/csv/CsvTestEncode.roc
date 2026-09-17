import Csv

## Typed writing: golden output (quoting, number and boolean spellings, the
## header union, dialects), `encode_columns` and round trips through `decode`.
CsvTestEncode :: [].{}

Mixed : { a : U64, b ?: Str, c : Str }

# Field names become the header in byte order; values become records in the same order.
expect Csv.encode([{ name: "ada", age: 36.U32 }]) == Ok("age,name\n36,ada")
expect Csv.encode([{ name: "ada", age: 36.U32 }, { name: "grace", age: 45.U32 }]) == Ok("age,name\n36,ada\n45,grace")
expect Csv.encode([{ ab: 1.U8, a_b: 2.U8, a1: 3.U8 }]) == Ok("a1,a_b,ab\n3,2,1")

# No values write nothing: there is no value to take names from.
expect {
	people : List({ name : Str })
	people = []
	Csv.encode(people) == Ok("")
}

# Quoting: the delimiter, a quote and a line break (golden).
expect Csv.encode([{ note: "a,b" }]) == Ok("note\n\"a,b\"")
expect Csv.encode([{ note: "say \"hi\"" }]) == Ok("note\n\"say \"\"hi\"\"\"")
expect Csv.encode([{ note: "two\nlines" }, { note: "cr\rhere" }]) == Ok("note\n\"two\nlines\"\n\"cr\rhere\"")

# Numbers and booleans (golden): the shortest float spelling, XML Schema specials, `Dec.to_str`.
expect Csv.encode([{ n: -42.I64, u: 18446744073709551615.U64 }]) == Ok("n,u\n-42,18446744073709551615")
expect Csv.encode([{ x: 1.0.F64 }, { x: -0.0 }, { x: 1e300 }, { x: 1e-7 }, { x: 0.1 }]) == Ok("x\n1.0\n-0.0\n1e300\n1e-7\n0.1")
expect Csv.encode([{ x: F64.infinity }, { x: -F64.infinity }, { x: F64.nan }]) == Ok("x\nINF\n-INF\nNaN")
expect Csv.encode([{ x: 0.1.F32 }, { x: 16777216.0 }, { x: F32.nan }]) == Ok("x\n0.1\n16777216.0\nNaN")
expect Csv.encode([{ d: 1.5.Dec }, { d: 5.0 }, { d: -0.25 }, { d: 12345678.123456789012345678 }]) == Ok("d\n1.5\n5.0\n-0.25\n12345678.123456789012345678")
expect {
	flags : List({ ok : Bool, no : Bool })
	flags = [{ ok: True, no: False }]
	Csv.encode(flags) == Ok("no,ok\nfalse,true")
}

# The header union: every record's fields in byte order, absent fields as empty cells by name.
expect {
	rows : List(Mixed)
	rows = [{ a: 1, c: "z" }, { a: 2, b: "x", c: "y" }]
	Csv.encode(rows) == Ok("a,b,c\n1,,z\n2,x,y")
}
expect {
	rows : List({ a : U8, b ?: U8, z ?: U8 })
	rows = [{ a: 1, z: 9 }, { a: 2, b: 3 }, { a: 4 }]
	Csv.encode(rows) == Ok("a,b,z\n1,,9\n2,3,\n4,,")
}

# Dialects: the delimiter and a CRLF newline.
expect Csv.encode_with([{ name: "ada", age: 36.U32 }], Csv.Dialect.tsv) == Ok("age\tname\n36\tada")
expect Csv.encode_with([{ a: "1" }, { a: "x,y" }], { ..Csv.Dialect.csv, newline: Crlf }) == Ok("a\r\n1\r\n\"x,y\"")

# Round trips through `decode`, mixed presence included.
expect {
	people : List({ name : Str, age : U32 })
	people = [{ name: "ada", age: 36 }, { name: "grace", age: 45 }]
	back : Try(List({ name : Str, age : U32 }), _)
	back = Csv.decode(Csv.encode(people) ?? "")
	back == Ok(people)
}
expect {
	rows : List({ note : Str })
	rows = [{ note: "a,b" }, { note: "say \"hi\"" }, { note: "two\nlines" }]
	back : Try(List({ note : Str }), _)
	back = Csv.decode(Csv.encode(rows) ?? "")
	back == Ok(rows)
}
# One empty column: the empty cell is written `""` and read back as a record.
expect {
	rows : List({ note : Str })
	rows = [{ note: "x" }, { note: "" }, { note: "y" }]
	back : Try(List({ note : Str }), _)
	back = Csv.decode(Csv.encode(rows) ?? "")
	back == Ok(rows)
}
expect {
	rows : List({ note : Str })
	rows = [{ note: "x" }, { note: "" }]
	back : Try(List({ note : Str }), _)
	back = Csv.decode(Csv.encode(rows) ?? "")
	back == Ok(rows)
}
expect {
	rows : List({ b ?: Str })
	rows = [{ b: "x" }, {}, { b: "y" }]
	back : Try(List({ b ?: Str }), _)
	back = Csv.decode(Csv.encode(rows) ?? "")
	back.map_ok(List.len) == Ok(3)
}
expect {
	rows : List({ a : Str, b : I64 })
	rows = [{ a: "x\ty", b: -1 }]
	back : Try(List({ a : Str, b : I64 }), _)
	back = Csv.decode_with(Csv.encode_with(rows, Csv.Dialect.tsv) ?? "", Csv.Dialect.tsv)
	back == Ok(rows)
}
expect {
	rows : List(Mixed)
	rows = [{ a: 1, c: "z" }, { a: 2, b: "x", c: "y" }]
	back : Try(List(Mixed), _)
	back = Csv.decode(Csv.encode(rows) ?? "")
	match back {
		Ok([first, second]) => first.a == 1 and first.?b ?? "absent" == "absent" and second.?b ?? "absent" == "x" and second.c == "y"
		_ => False
	}
}
expect {
	rows : List({ ok : Bool, x : F64, d : Dec })
	rows = [{ ok: True, x: -0.0, d: 170141183460469231731.687303715884105727 }, { ok: False, x: 1e-300, d: -0.000000000000000001 }]
	back : Try(List({ ok : Bool, x : F64, d : Dec }), _)
	back = Csv.decode(Csv.encode(rows) ?? "")
	back == Ok(rows)
}

# Typed output is ordinary CSV.
expect {
	people : List({ name : Str, age : U32 })
	people = [{ name: "ada", age: 36 }]
	Csv.parse(Csv.encode(people) ?? "") == Ok([["age", "name"], ["36", "ada"]])
}

# `encode_columns` fixes the order and refuses to drop or invent a column.
expect Csv.encode_columns([{ name: "ada", age: 36.U32 }], ["name", "age"]) == Ok("name,age\nada,36")
expect {
	rows : List(Mixed)
	rows = [{ a: 1, c: "z" }, { a: 2, b: "x", c: "y" }]
	Csv.encode_columns_with(rows, ["c", "b", "a"], Csv.Dialect.tsv) == Ok("c\tb\ta\nz\t\t1\ny\tx\t2")
}
expect Csv.encode_columns([{ a: 1.U8, b: 2.U8 }], ["a"]) == Err(MissingColumn("b"))
expect Csv.encode_columns([{ a: 1.U8 }], ["a", "zz"]) == Err(UnknownColumn("zz"))
expect Csv.encode_columns([{ a: 1.U8 }], ["a", "a"]) == Err(DuplicateColumn("a"))
expect {
	rows : List(Mixed)
	rows = [{ a: 1, c: "z" }]
	Csv.encode_columns(rows, ["a", "b", "c"]) == Err(UnknownColumn("b"))
}

# Precedence: duplicates, then unknown names, by list position; then missing fields by byte order.
expect Csv.encode_columns([{ a: 1.U8, b: 2.U8, c: 3.U8 }], ["x", "b", "y", "b", "x"]) == Err(DuplicateColumn("b"))
expect Csv.encode_columns([{ a: 1.U8, b: 2.U8, c: 3.U8 }], ["y", "x"]) == Err(UnknownColumn("y"))
expect Csv.encode_columns([{ a: 1.U8, b: 2.U8, c: 3.U8 }], ["b"]) == Err(MissingColumn("a"))
expect Csv.encode_columns([{ ab: 1.U8, a_b: 2.U8, a1: 3.U8 }], ["ab"]) == Err(MissingColumn("a1"))
expect {
	people : List({ name : Str })
	people = []
	Csv.encode_columns(people, []) == Ok("") and Csv.encode_columns(people, ["name"]) == Err(UnknownColumn("name"))
}
