import Toml

## Every `Toml.Err` kind with its position and payload, depth at the limit
## through each kind of nesting, `I64` bounds and out-of-range dates. Tests
## compare tags, positions, paths and source text, never message text.
TomlTestErrors :: [].{}

## An error as its tag, `line:column` and payload; `Ok` for a document read.
outcome : Str -> Str
outcome = |text|
	match Toml.parse(text) {
		Ok(_) => "Ok"
		Err(Syntax(at)) => "Syntax ${at.line.to_str()}:${at.column.to_str()}"
		Err(DuplicateKey(at)) => "DuplicateKey ${at.line.to_str()}:${at.column.to_str()} ${Str.inspect(at.path)}"
		Err(OutOfRange(at)) => "OutOfRange ${at.line.to_str()}:${at.column.to_str()} ${at.text}"
		Err(TooDeep(at)) => "TooDeep ${at.line.to_str()}:${at.column.to_str()}"
	}

## `count` nested levels below the root table of each kind.
nested_arrays : U64 -> Str
nested_arrays = |count| "a = ${Str.repeat("[", count)}${Str.repeat("]", count)}"

nested_inline_tables : U64 -> Str
nested_inline_tables = |count| "a = ${Str.repeat("{b = ", count - 1)}{}${Str.repeat("}", count - 1)}"

nested_headers : U64 -> Str
nested_headers = |count| "[${Str.repeat("k.", count - 1)}k]"

## A dotted key of `count` tables and a value.
nested_dotted : U64 -> Str
nested_dotted = |count| "${Str.repeat("k.", count)}k = 1"

## Levels below the root the limit allows: the root table is level 1.
below_root : U64
below_root = 127

# Syntax, at the offending character; columns count code points.
expect outcome("a = 1\nb = \n") == "Syntax 2:5"
expect outcome("a = 1\r\nb = ?") == "Syntax 2:5"
expect outcome("s = \"😀\" x") == "Syntax 1:9"
expect outcome("a = 1\rb = 2") == "Syntax 1:6"
expect outcome("a = [1,,2]") == "Syntax 1:8"
expect outcome("[t\nx = 1") == "Syntax 1:3"
expect outcome("x = { a = 1 b = 2 }") == "Syntax 1:13"
expect outcome("x = 01") == "Syntax 1:5"
expect outcome("x = \"\\q\"") == "Syntax 1:6"

# DuplicateKey at the second definition, with the key's full path.
expect outcome("a = 1\na = 2") == "DuplicateKey 2:1 [Key(\"a\")]"
expect outcome("a.b = 1\na.b.c = 2") == "DuplicateKey 2:3 [Key(\"a\"), Key(\"b\")]"
expect outcome("[t]\n[t]") == "DuplicateKey 2:2 [Key(\"t\")]"
expect outcome("t = { a = 1, a = 2 }") == "DuplicateKey 1:14 [Key(\"t\"), Key(\"a\")]"
expect outcome("[p]\nt = { a = 1 }\nt.b = 2") == "DuplicateKey 3:1 [Key(\"p\"), Key(\"t\")]"
expect outcome("[a.b.c]\nz = 9\n[a]\nb.c.t = 1") == "DuplicateKey 4:1 [Key(\"a\"), Key(\"b\")]"
expect outcome("a.b = 1\n[a]") == "DuplicateKey 2:2 [Key(\"a\")]"
expect outcome("[a.b]\n[[a]]") == "DuplicateKey 2:3 [Key(\"a\")]"

# `[[x]]` after a static array `x`, and a duplicate inside the second `[[bin]]`.
expect outcome("x = []\n[[x]]") == "DuplicateKey 2:3 [Key(\"x\")]"
expect outcome("[[bin]]\nname = \"a\"\n[[bin]]\nname = \"b\"\nname = \"c\"") == "DuplicateKey 5:1 [Key(\"bin\"), Index(1), Key(\"name\")]"
expect outcome("[[bin]]\n[bin.sub]\n[[bin]]\n[bin.sub]\nx = 1\n[bin.sub]") == "DuplicateKey 6:6 [Key(\"bin\"), Index(1), Key(\"sub\")]"

# OutOfRange carries the literal: `I64`'s bounds, dates, times, offsets.
expect outcome("n = -9223372036854775808") == "Ok"
expect outcome("n = 9223372036854775807") == "Ok"
expect outcome("n = 9223372036854775808") == "OutOfRange 1:5 9223372036854775808"
expect outcome("n = [1, 0x8000000000000000]") == "OutOfRange 1:9 0x8000000000000000"
expect outcome("n = -9_223_372_036_854_775_809") == "OutOfRange 1:5 -9_223_372_036_854_775_809"
expect outcome("d = 1979-05-27T07:32:00+23:59") == "Ok"
expect outcome("d = 1979-05-27T07:32:00+24:00") == "OutOfRange 1:5 1979-05-27T07:32:00+24:00"
expect outcome("d = 1979-05-27 07:32-23:60") == "OutOfRange 1:5 1979-05-27 07:32-23:60"
expect outcome("d = 2021-02-29") == "OutOfRange 1:5 2021-02-29"
expect outcome("t = 23:59:60") == "OutOfRange 1:5 23:59:60"

# Depth: 128 levels accepted and 129 refused, at the bracket or key that
# crosses, through arrays, inline tables, headers and dotted keys.
expect outcome(nested_arrays(below_root)) == "Ok"
expect outcome(nested_arrays(below_root + 1)) == "TooDeep 1:${(4 + below_root + 1).to_str()}"
expect outcome(nested_inline_tables(below_root)) == "Ok"
expect outcome(nested_inline_tables(below_root + 1)) == "TooDeep 1:${(4 + 5 * below_root + 1).to_str()}"
expect outcome(nested_headers(below_root)) == "Ok"
expect outcome(nested_headers(below_root + 1)) == "TooDeep 1:${(1 + 2 * below_root + 1).to_str()}"
expect outcome(nested_dotted(below_root)) == "Ok"
expect outcome(nested_dotted(below_root + 1)) == "TooDeep 1:${(2 * below_root + 1).to_str()}"

# Kinds of nesting add up: `[a.b]` then `c.d = [1]` puts `1` in level 5.
expect outcome("[${Str.repeat("k.", below_root - 3)}k]\nc = [[]]") == "Ok"
expect outcome("[${Str.repeat("k.", below_root - 2)}k]\nc = [[]]") == "TooDeep 2:6"
expect outcome("[[${Str.repeat("k.", below_root - 2)}k]]") == "Ok"
expect outcome("[[${Str.repeat("k.", below_root - 1)}k]]") == "TooDeep 1:${(2 + 2 * (below_root - 1) + 1).to_str()}"

# Messages: a position prefix (message text is not promised).
expect Toml.parse("a = 1\na = 2").map_err(Toml.err_to_str) == Err("line 2, column 1: the key a is already defined")
expect Toml.parse("[[b]]\n\"x y\" = 1\n\"x y\" = 2").map_err(Toml.err_to_str) == Err("line 3, column 1: the key b[0].\"x y\" is already defined")
# Keys are escaped as they are written, so a message stays on one line.
expect Toml.parse("\"a\\nb\" = 1\n\"a\\nb\" = 2").map_err(Toml.err_to_str) == Err("line 2, column 1: the key \"a\\nb\" is already defined")
expect Toml.parse("\"\\u0001\\t\" = 1\n\"\\u0001\\t\" = 2").map_err(Toml.err_to_str) == Err("line 2, column 1: the key \"\\u0001\\t\" is already defined")
