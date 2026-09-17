import TomlIndex
import TomlParse
import TomlSyntax
import Toml

## A document's syntax pieces (what each line, key, array and inline table
## keeps where) and its index (the path, form and address of every piece, and
## how every table came to be), which edits build on.
TomlTestIndex :: [].{}

## The lines of a text, none when it is refused.
lines_of : Str -> List(TomlSyntax.Line)
lines_of = |text|
	match TomlParse.read(text) {
		Ok(parsed) => parsed.file.lines
		Err(_) => []
	}

index_of : Str -> TomlIndex.Index
index_of = |text| TomlIndex.build(lines_of(text))

## A line's pieces as text: indent, key spellings and dots, `=` with its spaces
## (a header's inner spaces instead), trailing text and ending.
line_pieces : TomlSyntax.Line -> List(Str)
line_pieces = |line| {
	body =
		match line.body {
			Blank => []
			Pair(pair) => [TomlSyntax.key_text(pair.key), pair.equals, TomlSyntax.node_text(pair.node)]
			Header(header) => [if header.is_array { "[[" } else { "[" }, header.open, TomlSyntax.key_text(header.key), header.close]
		}
	[line.indent].concat(body).concat([line.trailing, line.ending])
}

## The first line's key: each segment's spelling, name, and the dots.
first_key : Str -> { raws : List(Str), names : List(Str), dots : List(Str) }
first_key = |text| {
	empty = { parts: [], dots: [] }
	key =
		match first_body(text) {
			Pair(pair) => pair.key
			Header(header) => header.key
			Blank => empty
		}
	{ raws: key.parts.map(|part| part.raw), names: TomlSyntax.names(key), dots: key.dots }
}

## Each element or entry of the first line's value as its before, its own
## text, its after and whether a comma follows, then the closing trivia.
value_pieces : Str -> List(Str)
value_pieces = |text|
	match first_body(text) {
		Pair(pair) => container_pieces(pair.node)
		_ => []
	}

first_body : Str -> TomlSyntax.Body
first_body = |text|
	match lines_of(text).first() {
		Ok(line) => line.body
		Err(_) => Blank
	}

container_pieces : TomlSyntax.Node -> List(Str)
container_pieces = |node|
	match node {
		Scalar(scalar) => [scalar.raw]
		Array(array) =>
			array.elements.fold([], |pieces, element| pieces.concat([element.before, TomlSyntax.node_text(element.node), element.after, comma_mark(element.has_comma)])).append(array.close)
		InlineTable(table) =>
			table.entries.fold([], |pieces, entry| pieces.concat([entry.before, TomlSyntax.pair_text(entry.pair), entry.after, comma_mark(entry.has_comma)])).append(table.close)
	}

comma_mark : Bool -> Str
comma_mark = |has_comma| if has_comma { "," } else { "" }

## Every piece as `path form line route key_length`.
pieces_text : Str -> List(Str)
pieces_text = |text| index_of(text).pieces.map(piece_text)

piece_text : TomlIndex.Piece -> Str
piece_text = |piece| "${Str.inspect(piece.path)} ${Str.inspect(piece.form)} ${piece.address.line.to_str()} ${Str.inspect(piece.address.route)} ${piece.key_length.to_str()}"

kind_at : Str, List(Toml.Segment) -> Try(TomlIndex.Kind, [NotFound])
kind_at = |text, path| TomlIndex.container_at(index_of(text), path).map_ok(|container| container.kind)

# A key/value line: indent, dotted key with spaces, `=`, value, comment, CRLF.
expect lines_of("  a . b = 1 # c\r\n").map(line_pieces) == [["  ", "a . b", " = ", "1", " # c", "\r\n"]]
expect first_key("a . \"b c\".'d' = 1") == { raws: ["a", "\"b c\"", "'d'"], names: ["a", "b c", "d"], dots: [" . ", "."] }

# A header line: the spaces inside its brackets are kept.
expect lines_of("[[ x . y ]]  # h\n").map(line_pieces) == [["", "[[", " ", "x . y", " ", "  # h", "\n"]]

# Blank lines, comment lines and spaces at the end of the file are lines too.
expect lines_of("\n  # c\n[t]\na = 1\n  ").map(line_pieces) == [["", "", "\n"], ["  ", "# c", "\n"], ["", "[", "", "t", "", "", "\n"], ["", "a", " = ", "1", "", "\n"], ["  ", "", ""]]
expect lines_of("").is_empty()

# The byte-order mark is kept apart from the first line.
expect TomlParse.read("\u(FEFF)a = 1").map_ok(|parsed| parsed.file.bom) == Ok("\u(FEFF)")
expect TomlParse.read("a = 1").map_ok(|parsed| parsed.file.bom) == Ok("")

# Arrays: trivia before and after each element, the comma, and what closes.
expect value_pieces("a = [ 1 , 2, ]") == [" ", "1", " ", ",", " ", "2", "", ",", " "]
expect value_pieces("a = [\n  1, # one\n  2\n]") == ["\n  ", "1", "", ",", " # one\n  ", "2", "\n", "", ""]
expect value_pieces("a = []") == [""]

# Inline tables likewise, the entry's key and `=` spacing inside its pair.
expect value_pieces("t = {a=1, b . c = 2}") == ["", "a=1", "", ",", " ", "b . c = 2", "", "", ""]
expect value_pieces("t = {\n  x = 1,\n}") == ["\n  ", "x = 1", "", ",", "\n"]

# A section runs from its header to the next header or the end of the file.
expect TomlSyntax.section_end(lines_of("a = 1\n[t]\nb = 2\n\n[[u]]\nc = 3\n"), 0) == 1
expect TomlSyntax.section_end(lines_of("a = 1\n[t]\nb = 2\n\n[[u]]\nc = 3\n"), 1) == 4
expect TomlSyntax.section_end(lines_of("a = 1\n[t]\nb = 2\n\n[[u]]\nc = 3\n"), 4) == 6

# Pieces in document order; a split dotted table is found by its prefix.
expect TomlIndex.pieces_under(index_of("apple.type = 1\norange.type = 2\napple.skin = 3\n"), [Key("apple")]).map(|piece| piece.address.line) == [0, 2]

# A sub-section before its parent: every piece has its full path.
expect pieces_text("[x.y]\nz = 1\n[x]\nw = 2\n") == [
	"[Key(\"x\"), Key(\"y\")] Header 0 [] 2",
	"[Key(\"x\"), Key(\"y\"), Key(\"z\")] Pair 1 [] 1",
	"[Key(\"x\")] Header 2 [] 1",
	"[Key(\"x\"), Key(\"w\")] Pair 3 [] 1",
]

# `[[bin]]` elements carry `Index`, and so do the sections under them.
expect pieces_text("[[bin]]\n[bin.sub]\nk = 1\n[[bin]]\n") == [
	"[Key(\"bin\"), Index(0)] ArrayHeader 0 [] 1",
	"[Key(\"bin\"), Index(0), Key(\"sub\")] Header 1 [] 2",
	"[Key(\"bin\"), Index(0), Key(\"sub\"), Key(\"k\")] Pair 2 [] 1",
	"[Key(\"bin\"), Index(1)] ArrayHeader 3 [] 1",
]

# Inside values: entries and elements by route from their line.
expect pieces_text("t = { a = [1, { b = 2 }], c.d = 3 }") == [
	"[Key(\"t\")] Pair 0 [] 1",
	"[Key(\"t\"), Key(\"a\")] InlineEntry 0 [0] 1",
	"[Key(\"t\"), Key(\"a\"), Index(0)] Element 0 [0, 0] 0",
	"[Key(\"t\"), Key(\"a\"), Index(1)] Element 0 [0, 1] 0",
	"[Key(\"t\"), Key(\"a\"), Index(1), Key(\"b\")] InlineEntry 0 [0, 1, 0] 1",
	"[Key(\"t\"), Key(\"c\"), Key(\"d\")] InlineEntry 0 [1] 2",
]

# How each table and array came to be.
kinds : Str
kinds = "[a.b]\nx.y = 1\n[[c]]\nd = { e.f = 1 }\ng = []\n[fruit]\napple.color = 1\n[fruit.apple.texture]\n[p.q]\n[p]\n"

expect kind_at(kinds, []) == Ok(Root)
expect kind_at(kinds, [Key("a")]) == Ok(Implicit)
expect kind_at(kinds, [Key("a"), Key("b")]) == Ok(Header)
expect kind_at(kinds, [Key("a"), Key("b"), Key("x")]) == Ok(Dotted)
expect kind_at(kinds, [Key("c")]) == Ok(ArrayOfTables)
expect kind_at(kinds, [Key("c"), Index(0)]) == Ok(Header)
expect kind_at(kinds, [Key("c"), Index(0), Key("d")]) == Ok(Inline)
expect kind_at(kinds, [Key("c"), Index(0), Key("d"), Key("e")]) == Ok(Inline)
expect kind_at(kinds, [Key("c"), Index(0), Key("g")]) == Ok(Array)
expect kind_at(kinds, [Key("fruit"), Key("apple")]) == Ok(Dotted)
expect kind_at(kinds, [Key("fruit"), Key("apple"), Key("texture")]) == Ok(Header)
expect kind_at(kinds, [Key("p")]) == Ok(Header)
expect kind_at(kinds, [Key("nope")]) == Err(NotFound)

# The index's value is the document's.
expect index_of(kinds).value == Toml.parse(kinds) ?? Table([])
