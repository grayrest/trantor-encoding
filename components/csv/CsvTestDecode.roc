import Csv

## Typed reading: columns matched by name, XML Schema cells, absent and
## optional fields, and the first error with its path.
CsvTestDecode :: [].{}

## The path of a `Mismatch`, or the error's tag.
mismatch_path : Try(List(a), [Parse(Csv.Err), Mismatch({ path : List(Csv.Segment), expected : Str }), MissingRequiredField(Str)]) -> Try(List(Csv.Segment), [Parse, Missing(Str), Decoded])
mismatch_path = |result|
	match result {
		Err(Mismatch(problem)) => Ok(problem.path)
		Err(Parse(_)) => Err(Parse)
		Err(MissingRequiredField(name)) => Err(Missing(name))
		Ok(_) => Err(Decoded)
	}

# Text to typed records, columns matched by name in any order; unnamed columns ignored.
expect {
	people : Try(List({ name : Str, age : U32 }), _)
	people = Csv.decode("name,age\nada,36\ngrace,45")
	people == Ok([{ name: "ada", age: 36 }, { name: "grace", age: 45 }])
}
expect {
	people : Try(List({ name : Str, age : U32 }), _)
	people = Csv.decode("age,name\n36,ada")
	people == Ok([{ name: "ada", age: 36 }])
}
expect {
	people : Try(List({ name : Str }), _)
	people = Csv.decode("name,age,note\nada,36,x")
	people == Ok([{ name: "ada" }])
}

# A column the type needs and the header lacks names the field.
expect {
	people : Try(List({ nope : Str }), _)
	people = Csv.decode("name\nada")
	people == Err(MissingRequiredField("nope"))
}

# `Try` fields: absent column, empty cell and a present value.
expect {
	people : Try(List({ name : Str, nick : Try(Str, _) }), _)
	people = Csv.decode("name\nada")
	match people {
		Ok([person]) => person.name == "ada" and person.nick.is_err()
		_ => False
	}
}
expect {
	people : Try(List({ name : Str, nick : Try(Str, _) }), _)
	people = Csv.decode("name,nick\nada,countess")
	match people {
		Ok([person]) => person.nick.ok_or("") == "countess"
		_ => False
	}
}
expect {
	rows : Try(List({ id : U8, score : Try(U32, _) }), _)
	rows = Csv.decode("id,score\n1,\n2,7")
	match rows {
		Ok([first, second]) => first.score.is_err() and second.score.ok_or(0) == 7
		_ => False
	}
}

# Optional fields: an empty cell is absent; a required field's empty cell is `""` or a mismatch.
expect {
	rows : Try(List({ a : U64, b ?: Str, c : Str }), _)
	rows = Csv.decode("a,b,c\n1,,\n2,x,y")
	match rows {
		Ok([first, second]) => first.?b ?? "absent" == "absent" and first.c == "" and second.?b ?? "absent" == "x" and second.c == "y"
		_ => False
	}
}
expect {
	rows : Try(List({ a : U64, b : Str }), _)
	rows = Csv.decode("a,b\n1,\n,x")
	mismatch_path(rows) == Ok([Index(1), Key("a")])
}

# The first bad cell stops decoding and names `[Index(record), Key(column)]`, records from 0 after the header.
expect {
	people : Try(List({ name : Str, age : U32 }), _)
	people = Csv.decode("name,age\nada,36\ngrace,old")
	mismatch_path(people) == Ok([Index(1), Key("age")])
}
expect {
	rows : Try(List({ a : I64, b : I64 }), _)
	rows = Csv.decode("b,a\nx,y")
	mismatch_path(rows) == Ok([Index(0), Key("b")])
}
expect {
	rows : Try(List({ age : U32 }), _)
	rows = Csv.decode_with("age\n\n# note\n1\n\"2\"\n\n\"x\ny\"", { ..Csv.Dialect.csv, comment: CommentPrefix('#') })
	mismatch_path(rows) == Ok([Index(2), Key("age")])
}

# A quoted cell holding the delimiter is one value; the dialect is honoured.
expect {
	rows : Try(List({ name : Str, note : Str }), _)
	rows = Csv.decode("name,note\nada,\"a,b\"")
	rows == Ok([{ name: "ada", note: "a,b" }])
}
expect {
	people : Try(List({ name : Str, age : U32 }), _)
	people = Csv.decode_with("name\tage\nada\t36", Csv.Dialect.tsv)
	people == Ok([{ name: "ada", age: 36 }])
}

# Scan errors, no header and duplicate names arrive as `Parse`; a header alone is no records.
expect {
	people : Try(List({ name : Str }), _)
	people = Csv.decode("name\n\"unclosed")
	people == Err(Parse(Syntax({ line: 2, column: 1, expected: "a closing quote for the field opened here" })))
}
expect {
	people : Try(List({ name : Str }), _)
	people = Csv.decode("")
	people == Err(Parse(MissingHeader))
}
expect {
	rows : Try(List({ a : Str }), _)
	rows = Csv.decode("a,a\n1,2")
	rows == Err(Parse(DuplicateHeader({ line: 1, column: 3, name: "a" })))
}
expect {
	rows : Try(List({ a : Str }), _)
	rows = Csv.decode("a,a\n1,2\n3")
	rows == Err(Parse(DuplicateHeader({ line: 1, column: 3, name: "a" })))
}
expect {
	people : Try(List({ name : Str }), _)
	people = Csv.decode("\u(FEFF)name\n")
	people == Ok([])
}

# Booleans: `true`, `false`, `1`, `0` and nothing else.
expect {
	flags : Try(List({ ok : Bool }), _)
	flags = Csv.decode("ok\ntrue\nfalse\n1\n0")
	flags == Ok([{ ok: True }, { ok: False }, { ok: True }, { ok: False }])
}
expect {
	flags : Try(List({ ok : Bool }), _)
	flags = Csv.decode("ok\ntrue\nTRUE")
	mismatch_path(flags) == Ok([Index(1), Key("ok")])
}

# Numbers by XML Schema: signs and leading zeros for integers, float forms, specials, rounding and truncation.
expect {
	rows : Try(List({ a : I8, b : U16, c : U8 }), _)
	rows = Csv.decode("a,b,c\n-128,+5,007")
	rows == Ok([{ a: -128, b: 5, c: 7 }])
}
expect ["1_000", "0x10", "1,000", "1.0", " 1"].all(|cell| {
	rows : Try(List({ n : I64 }), _)
	rows = Csv.decode("n\n\"${cell}\"")
	mismatch_path(rows) == Ok([Index(0), Key("n")])
})
expect {
	rows : Try(List({ n : U8 }), _)
	rows = Csv.decode("n\n256")
	mismatch_path(rows) == Ok([Index(0), Key("n")])
}
expect {
	rows : Try(List({ x : F64 }), _)
	rows = Csv.decode("x\n.5\n5.\n-1.5E2\nINF\n-INF\n+INF\n1e400\n9007199254740993")
	rows == Ok([{ x: 0.5 }, { x: 5.0 }, { x: -150.0 }, { x: F64.infinity }, { x: -F64.infinity }, { x: F64.infinity }, { x: F64.infinity }, { x: 9007199254740992.0 }])
}
expect {
	rows : Try(List({ x : F64 }), _)
	rows = Csv.decode("x\nNaN")
	match rows {
		Ok([row]) => row.x.is_nan()
		_ => False
	}
}
expect ["nan", "inf", "Infinity", "1_000.5"].all(|cell| {
	rows : Try(List({ x : F64 }), _)
	rows = Csv.decode("x\n${cell}")
	mismatch_path(rows) == Ok([Index(0), Key("x")])
})
expect {
	rows : Try(List({ x : F32 }), _)
	rows = Csv.decode("x\n0.1\n1e39")
	rows == Ok([{ x: 0.1 }, { x: F32.infinity }])
}
expect {
	rows : Try(List({ d : Dec }), _)
	rows = Csv.decode("d\n12345678.123456789012345678\n0.9999999999999999999\n1e-30\n-2.5e1")
	rows == Ok([{ d: 12345678.123456789012345678 }, { d: 0.999999999999999999 }, { d: 0 }, { d: -25 }])
}
expect {
	rows : Try(List({ d : Dec }), _)
	rows = Csv.decode("d\nINF")
	mismatch_path(rows) == Ok([Index(0), Key("d")])
}

# CSV cells are scalars: a nested record is a mismatch at its column.
expect {
	rows : Try(List({ a : Str, inner : { b : Str } }), _)
	rows = Csv.decode("a,inner,b\nx,y,z")
	mismatch_path(rows) == Ok([Index(0), Key("inner")])
}

# The typed reader and the row reader see the same document.
expect {
	text = "name,age\nada,36"
	typed : Try(List({ name : Str, age : U32 }), _)
	typed = Csv.decode(text)
	match (typed, Csv.parse(text)) {
		(Ok(people), Ok(rows)) => people.len() == rows.len() - 1
		_ => False
	}
}
