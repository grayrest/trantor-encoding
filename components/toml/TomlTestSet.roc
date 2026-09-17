import Toml

## `set` and `set_with`: replacing in place, adding keys by the table's form,
## table styles and when they are refused, kind changes, section placement,
## empty paths and depth.
TomlTestSet :: [].{}

## The text after `set_with`, or the error it answered.
set_to : Str, List(Toml.Segment), Toml.Value, Toml.Edit -> Str
set_to = |text, path, value, edit|
	match Toml.parse_document(text) {
		Ok(document) =>
			match document.set_with(path, value, edit) {
				Ok(done) => done.to_str()
				Err(problem) => Str.inspect(problem)
			}
		Err(_) => "unreadable"
	}

keys : List(Str) -> List(Toml.Segment)
keys = |names| Toml.path(names)

one : Toml.Value
one = Table([("a", Integer(1))])

auto : Toml.Edit
auto = {}

inline : Toml.Edit
inline = { table: Inline }

header : Toml.Edit
header = { table: Header }

## Arrays inside the outermost one, whose innermost then sits one level past
## the limit when set under a root key.
arrays_inside : U64
arrays_inside = 127

first_element : Toml.Segment
first_element = Index(0)

nested_arrays : Toml.Value
nested_arrays = List.repeat({}, arrays_inside).fold(Array([]), |inner, _| Array([inner]))

dotted : Toml.Edit
dotted = { table: Dotted }

# Replacing a value keeps the key's spelling, spacing and trailing comment.
expect set_to("'a' =  1_000 # c\n", keys(["a"]), Integer(2), auto) == "'a' =  2 # c\n"
expect set_to("t = { x = 1, y = 2 }\n", keys(["t", "y"]), String("z"), auto) == "t = { x = 1, y = \"z\" }\n"
expect set_to("a = [1, 2]\n", [Key("a"), Index(1)], Integer(5), auto) == "a = [1, 5]\n"

# A new key goes after the table's last key with its indentation: in a
# header section, a dotted table, an inline table.
expect set_to("[t]\n  a = 1\n\n  # c\n[u]\n", keys(["t", "b"]), Integer(2), auto) == "[t]\n  a = 1\n  b = 2\n\n  # c\n[u]\n"
expect set_to("x = 0\nfruit . apple = 1\nz = 0\n", keys(["fruit", "pear"]), Integer(2), auto) == "x = 0\nfruit . apple = 1\nfruit . pear = 2\nz = 0\n"
expect set_to("t = { a = 1 }\n", keys(["t", "b"]), Integer(2), auto) == "t = { a = 1, b = 2 }\n"
expect set_to("t = { a.b = 1 }\n", keys(["t", "a", "c"]), Integer(2), auto) == "t = { a.b = 1, a.c = 2 }\n"

# `Auto`: inline under a header holding only inline tables, a header
# elsewhere, dotted under a dotted parent, inline for an array of tables
# under a dotted parent, `[[x]]` under the root.
expect set_to("[deps]\na = { path = \"a\" }\n", keys(["deps", "b"]), one, auto) == "[deps]\na = { path = \"a\" }\nb = { a = 1 }\n"
expect set_to("[package]\nname = \"p\"\n", keys(["package", "meta"]), one, auto) == "[package]\nname = \"p\"\n\n[package.meta]\na = 1\n"
expect set_to("p.x = 1\n", keys(["p", "t"]), one, auto) == "p.x = 1\np.t.a = 1\n"
expect set_to("p.x = 1\n", keys(["p", "list"]), Array([one]), auto) == "p.x = 1\np.list = [{ a = 1 }]\n"
expect set_to("a = 1\n", keys(["list"]), Array([one, one]), auto) == "a = 1\n\n[[list]]\na = 1\n\n[[list]]\na = 1\n"

# Forced styles, and where they cannot be.
expect set_to("[deps]\n", keys(["deps", "foo"]), one, inline) == "[deps]\nfoo = { a = 1 }\n"
expect set_to("", keys(["deps", "foo"]), one, inline) == "[deps]\nfoo = { a = 1 }\n"
expect set_to("", keys(["deps", "foo"]), one, auto) == "[deps.foo]\na = 1\n"
expect set_to("[t]\n", keys(["t", "u"]), one, dotted) == "[t]\nu.a = 1\n"
expect set_to("t = { a = 1 }\n", keys(["t", "u"]), one, header) == "StyleNotPossible({ path: [Key(\"t\"), Key(\"u\")], style: Header })"
expect set_to("p.x = 1\n", keys(["p", "u"]), one, header) == "StyleNotPossible({ path: [Key(\"p\"), Key(\"u\")], style: Header })"
expect set_to("[p.x.y]\n", keys(["p", "x", "u"]), one, dotted) == "StyleNotPossible({ path: [Key(\"p\"), Key(\"x\"), Key(\"u\")], style: Dotted })"

# A table named only by a deeper header takes keys in a section of its own,
# after the deeper one; implicit parents are not written.
expect set_to("[p.x.y]\nz = 1\n", keys(["p", "x", "k"]), Integer(1), auto) == "[p.x.y]\nz = 1\n\n[p.x]\nk = 1\n"
expect set_to("[other]\n", keys(["components", "x", "kind"]), String("roc"), auto) == "[other]\n\n[components.x]\nkind = \"roc\"\n"

# New sections after their family, spaced as it is; a new top-level section
# at the end.
expect set_to("[components.a]\nkind = 1\n\n[wiring]\n", keys(["components", "b"]), one, auto) == "[components.a]\nkind = 1\n\n[components.b]\na = 1\n\n[wiring]\n"
expect set_to("[c.a]\nk = 1\n[c.b]\nk = 2\n[w]\n", keys(["c", "d"]), one, auto) == "[c.a]\nk = 1\n[c.b]\nk = 2\n[c.d]\na = 1\n[w]\n"
expect set_to("[a]\n\n[b]\n", keys(["z"]), one, auto) == "[a]\n\n[b]\n\n[z]\na = 1\n"

# A table inside an `[[x]]` element: its section after that element's.
expect set_to("[[bin]]\nn = 1\n[[bin]]\nn = 2\n", [Key("bin"), Index(0), Key("sub"), Key("k")], Integer(3), auto) == "[[bin]]\nn = 1\n\n[bin.sub]\nk = 3\n[[bin]]\nn = 2\n"

# Kind changes: a scalar becoming a table in place (`Auto`, `Dotted`, keeping
# the comment) or as a section (`Header`); a table becoming a scalar.
expect set_to("a = 1 # c\nb = 2\n", keys(["a"]), one, auto) == "a = { a = 1 } # c\nb = 2\n"
expect set_to("a = 1 # c\nb = 2\n", keys(["a"]), Table([("x", Integer(1)), ("y", Integer(2))]), dotted) == "a.x = 1 # c\na.y = 2\nb = 2\n"
expect set_to("a = 1 # c\nb = 2\n", keys(["a"]), one, header) == "b = 2\n\n[a]\na = 1\n"
expect set_to("x = 0\n\n# the table\n[t]\nk = 1\n\n[t.sub]\n", keys(["t"]), Integer(5), auto) == "x = 0\nt = 5\n"
expect set_to("t.a = 1 # first\nt.b = 2\nz = 0\n", keys(["t"]), Integer(5), auto) == "t = 5\nz = 0\n"
expect set_to("t = { a = 1 } # c\n", keys(["t"]), Integer(5), auto) == "t = 5 # c\n"

# Replacing a table in its own style edits it key by key; `Inline` over a
# header table removes the sections first.
expect set_to("# t\n[t]\na = 1 # one\nb = 2\n", keys(["t"]), Table([("a", Integer(1)), ("c", Integer(3))]), auto) == "# t\n[t]\na = 1 # one\nc = 3\n"
expect set_to("[p.x]\na = 1\n", keys(["p", "x"]), one, inline) == "[p]\nx = { a = 1 }\n"
expect set_to("t.a = 1\nz = 0\n", keys(["t"]), Table([("b", Integer(2))]), auto) == "t.b = 2\nz = 0\n"

# The empty path: the root key by key, keeping comments; only a table.
expect set_to("# head\na = 1 # one\nb = 2\n\n[t]\nk = 1\n", [], Table([("a", Integer(1)), ("t", Table([("k", Integer(2))]))]), auto) == "# head\na = 1 # one\n\n[t]\nk = 2\n"
expect set_to("a = 1\n", [], Integer(1), auto) == "Encode(RootNotATable)"

# `version` governs what an edit writes; `set` writes TOML 1.0.
expect set_to("", keys(["t"]), LocalTime({ hour: 7, minute: 30, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 }), { version: V1_1 }) == "t = 07:30\n"
expect set_to("", keys(["t"]), String("\u(1B)"), auto) == "t = \"\\u001B\"\n"

# CRLF files: new lines and a multi-line string's breaks take CRLF.
expect set_to("[t]\r\na = 1\r\n", keys(["t", "s"]), String("x\ny"), auto) == "[t]\r\na = 1\r\ns = \"\"\"\r\nx\r\ny\"\"\"\r\n"

# A CRLF file emptied by a kind change keeps CRLF for what is added.
expect set_to("[c]\r\n", keys(["c"]), Integer(42), auto) == "c = 42\r\n"
expect set_to("[c]\r\n", keys(["c"]), Array([Table([("e", Integer(5))])]), auto) == "[[c]]\r\ne = 5\r\n"
expect set_to("[c]\r\n", keys(["c"]), String("x\ny"), auto) == "c = \"\"\"\r\nx\r\ny\"\"\"\r\n"
expect set_to("[c.d]\r\n[e]", keys(["c"]), Integer(42), auto) == "c = 42\r\n\r\n[e]"

# A file without a final line break keeps having none; a BOM stays.
expect set_to("\u(FEFF)a = 1", keys(["b"]), Integer(2), auto) == "\u(FEFF)a = 1\nb = 2"

# Depth: parents past the limit, and a value too deep, as `Encode(TooDeep)`.
expect set_to("", keys(List.repeat("k", 129)), Integer(1), auto) == Str.inspect(Encode(TooDeep(keys(List.repeat("k", 128)))))
expect set_to("", keys(["k"]), nested_arrays, auto) == Str.inspect(Encode(TooDeep([Key("k")].concat(List.repeat(first_element, arrays_inside)))))
expect Toml.parse_document("a = []\n").map_ok(|document| document.append(keys(["a"]), nested_arrays).map_ok(|_| {})) == Ok(Err(Encode(TooDeep([Key("a"), first_element].concat(List.repeat(first_element, arrays_inside - 1))))))

# Paths that go nowhere.
expect set_to("a = 1\n", keys(["a", "b"]), Integer(1), auto) == "NotATable([Key(\"a\")])"
expect set_to("a = [1]\n", [Key("a"), Index(3)], Integer(1), auto) == "NotFound([Key(\"a\"), Index(3)])"
expect set_to("[[bin]]\n", [Key("bin"), Index(0)], Integer(1), auto) == "NotATable([Key(\"bin\"), Index(0)])"

# A dotted table inside an inline table replaced by a scalar.
expect set_to("t = { a.b = 1, c = 2 }\n", keys(["t", "a"]), Integer(5), auto) == "t = { c = 2, a = 5 }\n"

# `[[x]]` replaced element by element: changed, appended, removed; by an
# array value, as a scalar.
expect set_to("[[x]]\nk = 1\n\n[[x]]\nk = 2\n", keys(["x"]), Array([Table([("k", Integer(1))]), Table([("k", Integer(3))]), Table([("k", Integer(4))])]), auto) == "[[x]]\nk = 1\n\n[[x]]\nk = 3\n\n[[x]]\nk = 4\n"
expect set_to("[[x]]\nk = 1\n\n[[x]]\nk = 2\n", keys(["x"]), Array([Table([("k", Integer(9))])]), auto) == "[[x]]\nk = 9\n"
expect set_to("a = 0\n\n[[x]]\nk = 1\n", keys(["x"]), Array([Integer(1)]), auto) == "a = 0\nx = [1]\n"

# Tables named only by headers take new keys in sections of their own: merged
# at the top level, and inside an `[[x]]` element. A dotted table extended by
# a deeper header takes dotted keys in its own section.
expect set_to("[a.b]\nk = 1\n", keys(["a"]), Table([("b", Table([("k", Integer(2))])), ("c", Integer(1))]), auto) == "[a.b]\nk = 2\n\n[a]\nc = 1\n"
expect set_to("[[bin]]\n[bin.a.b]\n", [Key("bin"), Index(0), Key("a"), Key("k")], Integer(1), auto) == "[[bin]]\n[bin.a.b]\n[bin.a]\nk = 1\n"
expect set_to("[fruit]\napple.color = 1\n\n[fruit.apple.texture]\nsmooth = true\n", keys(["fruit", "apple", "size"]), Integer(2), auto) == "[fruit]\napple.color = 1\napple.size = 2\n\n[fruit.apple.texture]\nsmooth = true\n"

# A dotted table rewritten as a header; an empty table under `Dotted` is `{}`.
expect set_to("t.a = 1\nz = 0\n", keys(["t"]), one, header) == "z = 0\n\n[t]\na = 1\n"
expect set_to("[t]\nu = 1\n", keys(["t", "v"]), Table([]), dotted) == "[t]\nu = 1\nv = {}\n"

# A table set to an empty table stays, even one that would vanish with its
# last key: a dotted table as `{}` in place of its first line, an implicit
# one as a section.
expect set_to("t = { x.y = 1, z = 2 }\n", keys(["t", "x"]), Table([]), auto) == "t = { z = 2, x = {} }\n"
expect set_to("[fruit]\napple.color = 1\n\n[fruit.apple.texture]\nsmooth = true\n", keys(["fruit", "apple"]), Table([]), auto) == "[fruit]\napple = {}\n"
expect set_to("[a.b.c]\nk = 1\n", keys(["a", "b"]), Table([]), auto) == "[a.b]\n"
expect set_to("a.b = 1\n", keys(["a"]), Table([]), inline) == "a = {}\n"
