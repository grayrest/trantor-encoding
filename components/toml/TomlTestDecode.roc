import Toml

## Typed reading: every scalar width, records, lists, tuples, dicts, `Try`
## and optional fields, and the tag and path of each error.
TomlTestDecode :: [].{}

Problem : [Parse(Toml.Err), Mismatch({ path : List(Toml.Segment), expected : Str }), MissingRequiredField(Str)]

## The path of a `Mismatch`, or which other outcome it was.
mismatch_path : Try(a, Problem) -> Try(List(Toml.Segment), [Parse, Missing(Str), Decoded])
mismatch_path = |result|
	match result {
		Err(Mismatch(problem)) => Ok(problem.path)
		Err(Parse(_)) => Err(Parse)
		Err(MissingRequiredField(name)) => Err(Missing(name))
		Ok(_) => Err(Decoded)
	}

# Integers narrow into every width at its bounds; one past is a mismatch.
expect {
	decoded : Try({ a : U8, b : I8, c : U16, d : I16, e : U32, f : I32 }, _)
	decoded = Toml.decode("a = 255\nb = -128\nc = 65535\nd = -32768\ne = 4294967295\nf = -2147483648")
	decoded == Ok({ a: 255, b: -128, c: 65535, d: -32768, e: 4294967295, f: -2147483648 })
}
expect {
	decoded : Try({ a : U64, b : I64, c : U128, d : I128 }, _)
	decoded = Toml.decode("a = 9223372036854775807\nb = -9223372036854775808\nc = 0x7FFF_FFFF_FFFF_FFFF\nd = -9223372036854775808")
	decoded == Ok({ a: 9223372036854775807, b: I64.lowest, c: 9223372036854775807, d: -9223372036854775808 })
}
expect {
	decoded : Try({ n : U8 }, Problem)
	decoded = Toml.decode("n = 256")
	mismatch_path(decoded) == Ok([Key("n")])
}
expect {
	decoded : Try({ n : I8 }, Problem)
	decoded = Toml.decode("n = -129")
	mismatch_path(decoded) == Ok([Key("n")])
}
expect {
	decoded : Try({ n : U64 }, Problem)
	decoded = Toml.decode("n = -1")
	mismatch_path(decoded) == Ok([Key("n")])
}
expect {
	decoded : Try({ n : U16 }, Problem)
	decoded = Toml.decode("n = -0")
	decoded == Ok({ n: 0 })
}

# A float into an integer is a mismatch, even a whole one.
expect {
	decoded : Try({ n : I64 }, Problem)
	decoded = Toml.decode("n = 1.0")
	mismatch_path(decoded) == Ok([Key("n")])
}

# Integers into floats when exact; floats by their value, ±infinity past the range.
expect {
	decoded : Try({ a : F64, b : F32, c : Dec, d : F64 }, _)
	decoded = Toml.decode("a = 9007199254740992\nb = 16777216\nc = -9223372036854775808\nd = -9223372036854775808")
	decoded == Ok({ a: 9007199254740992.0, b: 16777216.0, c: -9223372036854775808, d: -9223372036854775808.0 })
}
expect {
	decoded : Try({ x : F64 }, Problem)
	decoded = Toml.decode("x = 9007199254740993")
	mismatch_path(decoded) == Ok([Key("x")])
}
expect {
	decoded : Try({ x : F32 }, Problem)
	decoded = Toml.decode("x = 16777217")
	mismatch_path(decoded) == Ok([Key("x")])
}
expect {
	decoded : Try({ x : F64 }, Problem)
	decoded = Toml.decode("x = 9223372036854775807")
	mismatch_path(decoded) == Ok([Key("x")])
}
expect {
	decoded : Try({ a : F64, b : F32, c : F64, d : F32, e : F64 }, _)
	decoded = Toml.decode("a = 1e400\nb = 1e39\nc = -inf\nd = 0.1\ne = 6.626e-34")
	decoded == Ok({ a: F64.infinity, b: F32.infinity, c: -F64.infinity, d: 0.1, e: 6.626e-34 })
}
expect {
	decoded : Try({ x : F32 }, _)
	decoded = Toml.decode("x = nan")
	match decoded {
		Ok(record) => record.x.is_nan()
		Err(_) => False
	}
}

# `Dec` reads the spelling: digits past 17 kept, past 18 cut toward zero; `inf` refused.
expect {
	decoded : Try({ a : Dec, b : Dec, c : Dec, d : Dec }, _)
	decoded = Toml.decode("a = 12345678.123456789012345678\nb = 0.99999999999999999999\nc = 1e-30\nd = 12_345.5e2")
	decoded == Ok({ a: 12345678.123456789012345678, b: 0.999999999999999999, c: 0, d: 1234550 })
}
expect {
	decoded : Try({ d : Dec }, Problem)
	decoded = Toml.decode("d = inf")
	mismatch_path(decoded) == Ok([Key("d")])
}
expect {
	decoded : Try({ d : Dec }, Problem)
	decoded = Toml.decode("d = 1e21")
	mismatch_path(decoded) == Ok([Key("d")])
}

# Strings and booleans take only their own kind; a date is not a string.
expect {
	decoded : Try({ s : Str, b : Bool }, _)
	decoded = Toml.decode("s = \"\"\"\nx\"\"\"\nb = false")
	decoded == Ok({ s: "x", b: Bool.False })
}
expect {
	decoded : Try({ s : Str }, Problem)
	decoded = Toml.decode("s = 1979-05-27")
	mismatch_path(decoded) == Ok([Key("s")])
}
expect {
	decoded : Try({ b : Bool }, Problem)
	decoded = Toml.decode("b = 'true'")
	mismatch_path(decoded) == Ok([Key("b")])
}

# Each date kind into its own type only.
expect {
	decoded : Try({ d : Toml.LocalDate, t : Toml.LocalTime, l : Toml.LocalDatetime, o : Toml.OffsetDatetime }, _)
	decoded = Toml.decode("d = 1979-05-27\nt = 07:32\nl = 1979-05-27T07:32:00.5\no = 1979-05-27T07:32:00-08:00")
	match decoded {
		Ok(record) =>
			record.d == { year: 1979, month: 5, day: 27 }
			and record.t == { hour: 7, minute: 32, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 }
			and record.l == { date: { year: 1979, month: 5, day: 27 }, time: { hour: 7, minute: 32, second: 0, millisecond: 500, microsecond: 0, nanosecond: 0 } }
			and record.o == { date: { year: 1979, month: 5, day: 27 }, time: { hour: 7, minute: 32, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 }, offset: { minutes: -480 } }
		Err(_) => False
	}
}
expect ["d = 07:32:00", "d = 1979-05-27T07:32:00", "d = '1979-05-27'"].all(|text| {
	decoded : Try({ d : Toml.LocalDate }, Problem)
	decoded = Toml.decode(text)
	mismatch_path(decoded) == Ok([Key("d")])
})
expect {
	decoded : Try({ o : Toml.OffsetDatetime }, Problem)
	decoded = Toml.decode("o = 1979-05-27T07:32:00")
	mismatch_path(decoded) == Ok([Key("o")])
}

# Records from tables, however they were written; unknown keys ignored.
expect {
	decoded : Try({ server : { host : Str, port : U16 }, name : Str }, _)
	decoded = Toml.decode("name = 'x'\nextra = [1, 2]\n[server]\nhost = 'h'\nport = 80\nother = { a = 1 }")
	decoded == Ok({ name: "x", server: { host: "h", port: 80 } })
}
expect {
	decoded : Try({ a : { b : { c : U8 } } }, _)
	decoded = Toml.decode("a.b.c = 1")
	decoded == Ok({ a: { b: { c: 1 } } })
}

# Lists from arrays and arrays of tables; tuples from arrays of their length.
expect {
	decoded : Try({ xs : List(U8), bins : List({ name : Str }) }, _)
	decoded = Toml.decode("xs = [1, 2, 3]\n[[bins]]\nname = 'a'\n[[bins]]\nname = 'b'")
	decoded == Ok({ xs: [1, 2, 3], bins: [{ name: "a" }, { name: "b" }] })
}
expect {
	decoded : Try({ pair : (U8, Str), nested : List((Bool, List(I8))) }, _)
	decoded = Toml.decode("pair = [1, 'b']\nnested = [[true, [-1]], [false, []]]")
	decoded == Ok({ pair: (1, "b"), nested: [(Bool.True, [-1]), (Bool.False, [])] })
}
expect {
	decoded : Try({ pair : (U8, Str) }, Problem)
	decoded = Toml.decode("pair = [1, 'b', 3]")
	mismatch_path(decoded) == Ok([Key("pair")])
}
expect {
	decoded : Try({ pair : (U8, Str) }, Problem)
	decoded = Toml.decode("pair = [1, 2]")
	mismatch_path(decoded) == Ok([Key("pair"), Index(1)])
}

# `Dict(Str, _)` from any table, keys as written.
expect {
	decoded : Try({ deps : Dict(Str, { version : Str }) }, _)
	decoded = Toml.decode("[deps]\n\"a b\" = { version = '1' }\nc.version = '2'")
	decoded == Ok({ deps: Dict.from_list([("a b", { version: "1" }), ("c", { version: "2" })]) })
}
expect {
	decoded : Try(Dict(Str, I64), _)
	decoded = Toml.decode("")
	decoded == Ok(Dict.empty())
}

# `Try` and optional fields: absent or present.
expect {
	decoded : Try({ name : Str, nick : Try(Str, [Missing]), age ?: U8 }, _)
	decoded = Toml.decode("name = 'a'")
	match decoded {
		Ok(record) => record.nick.is_err() and record.?age ?? 0 == 0
		Err(_) => False
	}
}
expect {
	decoded : Try({ name : Str, nick : Try(Str, [Missing]), age ?: U8 }, _)
	decoded = Toml.decode("name = 'a'\nnick = 'b'\nage = 7")
	match decoded {
		Ok(record) => record.nick.ok_or("") == "b" and record.?age ?? 0 == 7
		Err(_) => False
	}
}

# `Toml.Value` fields take whatever is there.
expect {
	decoded : Try({ raw : Toml.Value, items : List(Toml.Value) }, _)
	decoded = Toml.decode("raw = { a = 1 }\nitems = [1, 'x']")
	match decoded {
		Ok(record) => record.raw == Table([("a", Integer(1))]) and record.items == [Integer(1), String("x")]
		Err(_) => False
	}
}

# Errors: a missing field names only the field; paths carry keys and indices; parse errors pass through.
expect {
	decoded : Try({ server : { port : U16 } }, Problem)
	decoded = Toml.decode("[server]\nhost = 'h'")
	mismatch_path(decoded) == Err(Missing("port"))
}
expect {
	decoded : Try({ bins : List({ deps : Dict(Str, List(U8)) }) }, Problem)
	decoded = Toml.decode("[[bins]]\ndeps = {}\n[[bins]]\ndeps.a = [1, 2]\ndeps.b = [3, -4]")
	mismatch_path(decoded) == Ok([Key("bins"), Index(1), Key("deps"), Key("b"), Index(1)])
}
expect {
	decoded : Try({ server : { port : U16 } }, Problem)
	decoded = Toml.decode("server = 1")
	mismatch_path(decoded) == Ok([Key("server")])
}
expect {
	decoded : Try({ xs : List(U8) }, Problem)
	decoded = Toml.decode("xs = { a = 1 }")
	mismatch_path(decoded) == Ok([Key("xs")])
}
expect {
	decoded : Try({ a : U8 }, Problem)
	decoded = Toml.decode("a = ")
	mismatch_path(decoded) == Err(Parse)
}

# `decode_value` reads any value, not only a document.
expect {
	decoded : Try(List(U8), _)
	decoded = Toml.decode_value(Array([Integer(1), Integer(2)]))
	decoded == Ok([1, 2])
}
expect {
	decoded : Try(U8, _)
	decoded = Toml.decode_value(String("x"))
	match decoded {
		Err(Mismatch(problem)) => problem.path == []
		_ => False
	}
}
