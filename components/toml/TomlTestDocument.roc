import Toml

## `Toml.Document`: text back byte for byte, the value `Toml.parse` reads, the
## same errors, and `get` on every kind of table.
TomlTestDocument :: [].{}

## Whether a document gives its text back exactly and reads as `Toml.parse`.
is_lossless : Str -> Bool
is_lossless = |text|
	match (Toml.parse_document(text), Toml.parse(text)) {
		(Ok(document), Ok(value)) => document.to_str() == text and document.to_value() == value
		_ => False
	}

## `get` on a document's text; a refused text is `NotFound([])`.
got : Str, List(Toml.Segment) -> Try(Toml.Value, Toml.EditErr)
got = |text, path|
	match Toml.parse_document(text) {
		Ok(document) => document.get(path)
		Err(_) => Err(NotFound([]))
	}

## The error `parse_document` refuses a text with, or `Ok` when it reads.
refusal : Str -> Try({}, Toml.Err)
refusal = |text| Toml.parse_document(text).map_ok(|_| {})

## `table` from an edit, `Auto` when it is left out.
table_style : Toml.Edit -> [Auto, Inline, Header, Dotted]
table_style = |edit| edit.?table ?? Auto

## Arrays nested this deep under the root table are one level too many.
past_limit : U64
past_limit = 128

bins : Str
bins = "[[bin]]\nname = 'a'\n[[bin]]\nname = 'b'\n[bin.sub]\nk = 1\n"

# Trivia: comments, blank lines, indentation, spaces around `=`, and text with
# no final line break or ending in spaces.
expect is_lossless("# head\n\n\na = 1 # tail\n  # indented\n\n[t]   # header\n\tb\t=\t2\n")
expect is_lossless("a = 1")
expect is_lossless("a = 1\n   ")
expect is_lossless("# only a comment")
expect is_lossless("")
expect is_lossless("\n\n")

# Line endings: CRLF throughout, mixed, and inside multi-line values.
expect is_lossless("a = 1\r\n[t]\r\nb = [\r\n  1,\r\n]\r\ns = \"\"\"\r\nx\r\n\"\"\"\r\n")
expect is_lossless("a = 1\r\nb = 2\n# c\r\n")

# A byte-order mark stays, with LF or CRLF after it.
expect is_lossless("\u(FEFF)a = 1\n")
expect is_lossless("\u(FEFF)# c\r\n[t]\r\n")

# Number spellings.
expect is_lossless("a = 1_000\nb = 0xDEAD_beef\nc = 0o17\nd = 0b101\ne = +1.5e+06\nf = -inf\ng = nan\nh = 1E3\ni = +0\n")

# String spellings: literal, escapes, line-ending backslash, multi-line literal.
expect is_lossless("a = 'x'\nb = \"\\u00E9\\t\"\nc = \"\"\"\nline \\\n  more\"\"\"\nd = '''\nraw\\n'''\n")

# Date and time spellings.
expect is_lossless("d = 1979-05-27 07:32:00Z\nt = 07:32\no = 1979-05-27T07:32:00.123456789+00:00\n")

# Keys: quoted, dotted with spaces, headers with spaces inside the brackets.
expect is_lossless("a . \"b c\" .'d' = 1\n\"\" = 2\n[ x . y ]\n[[ z ]]\n[[z]]\n")

# Inline tables and arrays with their trivia, and TOML 1.1's multi-line inline
# tables.
expect is_lossless("t = {  a = 1 ,b.c=2 }\narr = [ 1,\n  # c\n  2 , ]\nm = {\n  x = 1, # one\n  y = [],\n}\n")

# Every table form: header, implicit, dotted, inline, split dotted tables,
# sub-sections before their parent, arrays of tables with sub-tables.
expect is_lossless("[a.b]\nc = 1\n[a]\nfruit.apple = 1\nveg.x = 2\nfruit.pear = 3\n[[bin]]\n[bin.sub]\n")

# `parse_document` refuses what `parse` refuses, with the same error.
expect refusal("a = 1\na = 2") == Err(DuplicateKey({ line: 2, column: 1, path: [Key("a")] }))
expect refusal("a = [1,,2]") == Err(Syntax({ line: 1, column: 8, expected: "a value" }))
expect refusal("a = ${Str.repeat("[", past_limit)}${Str.repeat("]", past_limit)}") == Err(TooDeep({ line: 1, column: 4 + past_limit }))
expect refusal("[x]\n[x]") == Toml.parse("[x]\n[x]").map_ok(|_| {})

# `get` with an empty path is the whole document.
expect got(bins, []).map_err(|_| {}) == Toml.parse(bins).map_err(|_| {})
expect got("", []) == Ok(Table([]))

# `get` in each kind of table: header, implicit, dotted (split across the file),
# a sub-section before its parent, inline.
expect got("[server]\nport = 8080\n", [Key("server"), Key("port")]) == Ok(Integer(8080))
expect got("[a.b]\nc = 1\n", [Key("a")]) == Ok(Table([("b", Table([("c", Integer(1))]))]))
expect got("apple.type = 'x'\norange.type = 'y'\napple.skin = 'z'\n", [Key("apple")]) == Ok(Table([("type", String("x")), ("skin", String("z"))]))
expect got("[x.y]\nz = 1\n[x]\nw = 2\n", [Key("x"), Key("w")]) == Ok(Integer(2))
expect got("[x.y]\nz = 1\n[x]\nw = 2\n", [Key("x"), Key("y"), Key("z")]) == Ok(Integer(1))
expect got("t = { a = { b = 1 } }", [Key("t"), Key("a"), Key("b")]) == Ok(Integer(1))

# `get` through `[[bin]]` elements and array values by `Index`.
expect got(bins, [Key("bin"), Index(1), Key("name")]) == Ok(String("b"))
expect got(bins, [Key("bin"), Index(1), Key("sub"), Key("k")]) == Ok(Integer(1))
expect got(bins, [Key("bin"), Index(0)]) == Ok(Table([("name", String("a"))]))
expect got("a = [1, [2, 3]]", [Key("a"), Index(1), Index(0)]) == Ok(Integer(2))

# `NotFound` names the path through what is missing, an index past the end
# included.
expect got("[server]\nport = 1\n", [Key("server"), Key("host")]) == Err(NotFound([Key("server"), Key("host")]))
expect got("[server]\nport = 1\n", [Key("nope"), Key("x")]) == Err(NotFound([Key("nope")]))
expect got(bins, [Key("bin"), Index(2), Key("name")]) == Err(NotFound([Key("bin"), Index(2)]))

# `NotATable` and `NotAnArray` name the value the segment was applied to.
expect got("[server]\nport = 1\n", [Key("server"), Key("port"), Key("x")]) == Err(NotATable([Key("server"), Key("port")]))
expect got(bins, [Key("bin"), Key("name")]) == Err(NotATable([Key("bin")]))
expect got("[server]\nport = 1\n", [Key("server"), Index(0)]) == Err(NotAnArray([Key("server")]))
expect got("a = 1", [Index(0)]) == Err(NotAnArray([]))

# `Toml.Edit`'s fields are optional.
expect table_style({ version: V1_1 }) == Auto
expect table_style({ table: Dotted }) == Dotted
