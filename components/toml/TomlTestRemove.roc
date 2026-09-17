import Toml

## `remove` (every piece under the path with its span, D-S3-31, D-S3-40) and
## `append` (following the array's layout, `[[x]]` sections).
TomlTestRemove :: [].{}

## The text after `edit`, or the error it answered.
edited : Str, (Toml.Document -> Try(Toml.Document, Toml.EditErr)) -> Str
edited = |text, edit|
	match Toml.parse_document(text) {
		Ok(document) =>
			match edit(document) {
				Ok(done) => done.to_str()
				Err(problem) => Str.inspect(problem)
			}
		Err(_) => "unreadable"
	}

removed : Str, List(Toml.Segment) -> Str
removed = |text, path| edited(text, |document| document.remove(path))

appended : Str, List(Toml.Segment), Toml.Value -> Str
appended = |text, path, value| edited(text, |document| document.append(path, value))

# A key/value with its comment block and trailing comment; a blank line stops
# the block.
expect removed("# old\n\n# about a\na = 1 # one\nb = 2\n", [Key("a")]) == "# old\n\nb = 2\n"

# A multi-line value goes with its line.
expect removed("a = [\n  1,\n  2,\n]\nb = \"\"\"\nx\n\"\"\"\nc = 3\n", [Key("b")]) == "a = [\n  1,\n  2,\n]\nc = 3\n"

# A header table with its sub-sections, before and after it, keeping the
# sections between.
expect removed("[x.y]\nz = 1\n\n[other]\nk = 1\n\n# the x table\n[x]\nw = 2\n\n[x.v]\n", [Key("x")]) == "[other]\nk = 1\n"

# A dotted table spread over its section, and an implicit table.
expect removed("apple.type = 1\norange.type = 2\napple.skin = 3\n", [Key("apple")]) == "orange.type = 2\n"
expect removed("[a.b]\nc = 1\n\n[d]\n", [Key("a")]) == "[d]\n"

# One `[[x]]` element with its sub-sections.
expect removed("[[bin]]\nname = 'a'\n\n[[bin]]\nname = 'b'\n[bin.sub]\nk = 1\n\n[[bin]]\nname = 'c'\n", [Key("bin"), Index(1)]) == "[[bin]]\nname = 'a'\n\n[[bin]]\nname = 'c'\n"

# Inline-table entries and array elements, on one line and on several.
expect removed("t = { a = 1, b = 2, c = 3 }\n", [Key("t"), Key("b")]) == "t = { a = 1, c = 3 }\n"
expect removed("t = { a = 1, b = 2 }\n", [Key("t"), Key("b")]) == "t = { a = 1 }\n"
expect removed("t = { a = 1, b = 2 }\n", [Key("t"), Key("a")]) == "t = { b = 2 }\n"
expect removed("a = [1, 2, 3]\n", [Key("a"), Index(2)]) == "a = [1, 2]\n"
expect removed("a = [\n  1,\n  # two\n  2, # the second\n  3,\n]\n", [Key("a"), Index(1)]) == "a = [\n  1,\n  3,\n]\n"
expect removed("a = [\n  1, # one\n  2\n]\n", [Key("a"), Index(1)]) == "a = [\n  1 # one\n]\n"
expect removed("t = { a.b = 1, c = 2, a.d = 3 }\n", [Key("t"), Key("a")]) == "t = { c = 2 }\n"

# The last key of a header or inline table leaves the table; of a dotted
# table, the table goes with it.
expect removed("[t]\na = 1\n", [Key("t"), Key("a")]) == "[t]\n"
expect removed("t = { a = 1 }\n", [Key("t"), Key("a")]) == "t = {}\n"
expect removed("x.a = 1\n", [Key("x"), Key("a")]) == ""

# The last key of the root; nothing is left above the first section.
expect removed("# a\na = 1\n\n[t]\n", [Key("a")]) == "[t]\n"

# A file without a final line break keeps having none.
expect removed("a = 1\nb = 2", [Key("b")]) == "a = 1"

# Nothing there, and the empty path.
expect removed("a = 1\n", [Key("b")]) == "NotFound([Key(\"b\")])"
expect removed("a = 1\n", []) == "NotFound([])"
expect appended("a = []\n", [], Integer(1)) == "NotFound([])"

# Appending to arrays on one line, on several with and without a trailing
# comma, and empty.
expect appended("a = [1, 2] # c\n", [Key("a")], Integer(3)) == "a = [1, 2, 3] # c\n"
expect appended("a = []\n", [Key("a")], String("x")) == "a = [\"x\"]\n"
expect appended("a = [\n  1,\n  2, # two\n]\n", [Key("a")], Integer(3)) == "a = [\n  1,\n  2, # two\n  3,\n]\n"
expect appended("a = [\n  1,\n  2 # two\n]\n", [Key("a")], Integer(3)) == "a = [\n  1,\n  2, # two\n  3\n]\n"

# `[[x]]`: a section after the last element's sections, spelled like it and
# spaced like them; a non-table is refused.
expect appended("[[ bin ]]\nname = 'a'\n[bin.sub]\n\n[other]\n", [Key("bin")], Table([("name", String("b"))])) == "[[ bin ]]\nname = 'a'\n[bin.sub]\n[[ bin ]]\nname = \"b\"\n\n[other]\n"
expect appended("[[bin]]\nname = 'a'\n\n[other]\n", [Key("bin")], Table([])) == "[[bin]]\nname = 'a'\n\n[[bin]]\n\n[other]\n"
expect appended("[[bin]]\n", [Key("bin")], Integer(1)) == "NotATable([Key(\"bin\")])"
expect appended("a = 1\n", [Key("a")], Integer(1)) == "NotAnArray([Key(\"a\")])"
