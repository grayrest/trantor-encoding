import TomlValue

## A document's concrete syntax: every byte of the source, kept as the text
## pieces between TOML's structure, so writing the pieces back in order gives
## the source again.
##
## A file is a byte-order mark (or `""`) and its lines. A line is an
## expression with the text around it: `indent`, a `body` (nothing, a
## key/value, or a `[table]`/`[[array]]` header), `trailing` (spaces and a
## comment) and `ending` (`"\n"`, `"\r\n"`, or `""` at the end of the file).
## A key/value whose value spans lines (a multi-line string, array or inline
## table) is still one line. Sections are not nested: a header's section is
## the lines up to the next header.
##
## Keys keep each segment's spelling (`raw`, quotes included) beside its
## `name`, and the text between segments (`dots`, a `.` with its spaces).
## Values keep scalars' spellings with their `Value`, and the trivia inside
## arrays and inline tables around each element or entry: `before` it, `after`
## it up to its comma, and `close` after the last one up to the bracket.
TomlSyntax :: [].{

	Key : { parts : List({ raw : Str, name : Str }), dots : List(Str) }

	## A value as written. `Array`: `[`, each element's `before`, node,
	## `after` and `,` when `has_comma`, then `close` and `]`. `InlineTable`
	## likewise between `{` and `}`, each entry a key, `equals` (`=` with its
	## spaces) and a node.
	Node := [
		Scalar({ raw : Str, value : TomlValue.Value }),
		Array({ elements : List({ before : Str, node : Node, after : Str, has_comma : Bool }), close : Str }),
		InlineTable({ entries : List({ before : Str, pair : { key : Key, equals : Str, node : Node }, after : Str, has_comma : Bool }), close : Str }),
	]

	Pair : { key : Key, equals : Str, node : Node }

	Element : { before : Str, node : Node, after : Str, has_comma : Bool }

	Entry : { before : Str, pair : Pair, after : Str, has_comma : Bool }

	## `[`, `open` (spaces), the key, `close` (spaces), `]`; brackets doubled
	## for an array of tables.
	Header : { is_array : Bool, open : Str, key : Key, close : Str }

	Body : [Blank, Pair(Pair), Header(Header)]

	Line : { indent : Str, body : Body, trailing : Str, ending : Str }

	File : { bom : Str, lines : List(Line) }

	## The file's text.
	file_text : File -> Str
	file_text = |file| file.lines.fold(file.bom, |text, line| Str.concat(text, TomlSyntax.line_text(line)))

	line_text : Line -> Str
	line_text = |line| "${line.indent}${body_text(line.body)}${line.trailing}${line.ending}"

	key_text : Key -> Str
	key_text = |key|
		key.parts.fold_with_index("", |text, part, position|
			if position == 0 {
				part.raw
			} else {
				"${text}${key.dots.get(position - 1) ?? "."}${part.raw}"
			})

	node_text : Node -> Str
	node_text = |node|
		match node {
			Scalar(scalar) => scalar.raw
			Array(array) => {
				inner = array.elements.fold("", |text, element| "${text}${element.before}${TomlSyntax.node_text(element.node)}${element.after}${comma(element.has_comma)}")
				"[${inner}${array.close}]"
			}
			InlineTable(table) => {
				inner = table.entries.fold("", |text, entry| "${text}${entry.before}${TomlSyntax.pair_text(entry.pair)}${entry.after}${comma(entry.has_comma)}")
				"{${inner}${table.close}}"
			}
		}

	pair_text : Pair -> Str
	pair_text = |pair| "${TomlSyntax.key_text(pair.key)}${pair.equals}${TomlSyntax.node_text(pair.node)}"

	## The segment names of a key.
	names : Key -> List(Str)
	names = |key| key.parts.map(|part| part.name)

	## Where the section holding line `position` ends: the next header line's
	## position, or the number of lines.
	section_end : List(Line), U64 -> U64
	section_end = |lines, position|
		match lines.drop_first(position + 1).find_first_index(is_header) {
			Ok(offset) => position + 1 + offset
			Err(NotFound) => lines.len()
		}
}

is_header : TomlSyntax.Line -> Bool
is_header = |line|
	match line.body {
		Header(_) => True
		_ => False
	}

body_text : TomlSyntax.Body -> Str
body_text = |body|
	match body {
		Blank => ""
		Pair(pair) => TomlSyntax.pair_text(pair)
		Header(header) => {
			brackets = if header.is_array { ("[[", "]]") } else { ("[", "]") }
			"${brackets.0}${header.open}${TomlSyntax.key_text(header.key)}${header.close}${brackets.1}"
		}
	}

comma : Bool -> Str
comma = |has_comma| if has_comma { "," } else { "" }
