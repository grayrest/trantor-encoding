import EncodingPath
import EncodingText
import TomlLex
import TomlProblem
import TomlSyntax
import TomlTree
import TomlValue

## A TOML 1.1.0 document to its tables and its syntax in one left-to-right
## pass over bytes.
##
## Expressions (key/values, `[table]` and `[[array]]` headers) build a
## `TomlTree` and a `TomlSyntax.Line` each; values are read whole, arrays and
## inline tables recursively, an inline table through a tree of its own, each
## value with its syntax node. Scanning carries byte indices; the first
## problem becomes a `Toml.Err` with a line and a column once, on the way out.
TomlParse :: [].{

	## What `Toml.parse` fails with (`Toml.Err`).
	Err : [
		Syntax({ line : U64, column : U64, expected : Str }),
		DuplicateKey({ line : U64, column : U64, path : List(EncodingPath.Segment) }),
		OutOfRange({ line : U64, column : U64, text : Str }),
		TooDeep({ line : U64, column : U64 }),
	]

	## A document's tables, and its text as syntax.
	Parsed : { tree : List(TomlTree.Slot), file : TomlSyntax.File }

	parse : Str -> Try(TomlValue.Value, Err)
	parse = |text| TomlParse.read(text).map_ok(|parsed| TomlTree.to_value(parsed.tree))

	read : Str -> Try(Parsed, Err)
	read = |text| {
		source = text.to_utf8()
		bytes = EncodingText.skip_bom(source)
		bom = Str.from_utf8_lossy(source.take_first(source.len() - bytes.len()))
		match document(bytes, 0, TomlTree.new(1, []), TomlTree.root, []) {
			Ok(done) => Ok({ tree: done.tree, file: { bom, lines: done.lines } })
			Err(problem) => Err(located(bytes, problem))
		}
	}

	## Whether the byte at `index` ends its line: LF only, so in CRLF the CR is
	## the old line's last column.
	ends_line : List(U8), U64 -> Bool
	ends_line = |bytes, index| bytes.get(index) == Ok('\n')
}

Tree : List(TomlTree.Slot)

Path : List(EncodingPath.Segment)

Read : { value : TomlValue.Value, node : TomlSyntax.Node, next : U64 }

Defined : { tree : Tree, pair : TomlSyntax.Pair, next : U64 }

located : List(U8), TomlProblem.Problem -> TomlParse.Err
located = |bytes, problem| {
	at = |index| EncodingText.position_at(bytes, index, TomlParse.ends_line)
	match problem {
		Syntax(syntax) => EncodingText.syntax(at(syntax.at), syntax.expected)
		OutOfRange(range) => {
			position = at(range.at)
			OutOfRange({ line: position.line, column: position.column, text: between(bytes, range.at, range.next) })
		}
		Duplicate(duplicate) => {
			position = at(duplicate.at)
			DuplicateKey({ line: position.line, column: position.column, path: duplicate.path })
		}
		Deep(index) => {
			position = at(index)
			TooDeep({ line: position.line, column: position.column })
		}
	}
}

## The source text from `start` up to `end`.
between : List(U8), U64, U64 -> Str
between = |bytes, start, end| Str.from_utf8_lossy(bytes.sublist({ start, len: end - start }))

## Every expression from `index` on, `table` the one the latest header
## opened. Plain parameters, so the tree and lines stay uniquely owned.
document : List(U8), U64, Tree, U64, List(TomlSyntax.Line) -> Try({ tree : Tree, lines : List(TomlSyntax.Line) }, TomlProblem.Problem)
document = |bytes, index, tree, table, lines| {
	start = TomlLex.skip_space(bytes, index)
	indent = between(bytes, index, start)
	match bytes.get(start) {
		Err(_) if start == index => Ok({ tree, lines })
		Err(_) => Ok({ tree, lines: lines.append({ indent, body: Blank, trailing: "", ending: "" }) })
		Ok('[') => {
			opened = header(bytes, start, tree)?
			rest = line_rest(bytes, opened.next)?
			line = { indent, body: Header(opened.header), trailing: rest.trailing, ending: rest.ending }
			document(bytes, rest.next, opened.tree, opened.table, lines.append(line))
		}
		Ok('#') | Ok('\n') | Ok('\r') => {
			rest = line_rest(bytes, start)?
			document(bytes, rest.next, tree, table, lines.append({ indent, body: Blank, trailing: rest.trailing, ending: rest.ending }))
		}
		Ok(_) => {
			defined = key_value(bytes, start, tree, table)?
			rest = line_rest(bytes, defined.next)?
			line = { indent, body: Pair(defined.pair), trailing: rest.trailing, ending: rest.ending }
			document(bytes, rest.next, defined.tree, table, lines.append(line))
		}
	}
}

## Spaces and a comment, then the line break or the end of the file.
line_rest : List(U8), U64 -> Try({ trailing : Str, ending : Str, next : U64 }, TomlProblem.Problem)
line_rest = |bytes, index| {
	next = TomlLex.line_end(bytes, index)?
	ending = TomlLex.skip_comment(bytes, TomlLex.skip_space(bytes, index))?
	Ok({ trailing: between(bytes, index, ending), ending: between(bytes, ending, next), next })
}

## `[a.b]` or `[[a.b]]`, the brackets doubled without spaces between them.
header : List(U8), U64, Tree -> Try({ tree : Tree, table : U64, header : TomlSyntax.Header, next : U64 }, TomlProblem.Problem)
header = |bytes, open, tree| {
	is_array = bytes.get(open + 1) == Ok('[')
	brackets = if is_array { "]]" } else { "]" }
	width = Str.count_utf8_bytes(brackets)
	key_start = TomlLex.skip_space(bytes, open + width)
	key = TomlLex.key(bytes, key_start)?
	close = TomlLex.skip_space(bytes, key.next)
	if bytes.sublist({ start: close, len: width }) == brackets.to_utf8() {
		opened = if is_array { TomlTree.open_array_table(tree, key.parts)? } else { TomlTree.open_table(tree, key.parts)? }
		syntax = { is_array, open: between(bytes, open + width, key_start), key: key_syntax(bytes, key.parts), close: between(bytes, key.next, close) }
		Ok({ tree: opened.tree, table: opened.table, header: syntax, next: close + width })
	} else {
		Err(TomlProblem.syntax(close, "`${brackets}` to close the header"))
	}
}

## Each segment's spelling and the text between segments.
key_syntax : List(U8), List(TomlLex.KeyPart) -> TomlSyntax.Key
key_syntax = |bytes, parts| {
	parts: parts.map(|part| { raw: between(bytes, part.at, part.next), name: part.name }),
	dots: parts.map2(parts.drop_first(1), |part, following| between(bytes, part.next, following.at)),
}

## `key = value` into `table`; the key is checked before the value is read,
## so a duplicate is reported where it starts.
key_value : List(U8), U64, Tree, U64 -> Try(Defined, TomlProblem.Problem)
key_value = |bytes, index, tree, table| {
	key = TomlLex.key(bytes, index)?
	equals = TomlLex.skip_space(bytes, key.next)
	if bytes.get(equals) == Ok('=') {
		prepared = TomlTree.prepare_key(tree, table, key.parts)?
		value_start = TomlLex.skip_space(bytes, equals + 1)
		read = value(bytes, value_start, prepared.level, prepared.path)?
		pair = { key: key_syntax(bytes, key.parts), equals: between(bytes, key.next, value_start), node: read.node }
		Ok({ tree: TomlTree.insert(prepared.tree, prepared.table, prepared.name, read.value), pair, next: read.next })
	} else {
		Err(TomlProblem.syntax(equals, "`=` after the key"))
	}
}

## A value inside a container at `level`, found at `path`.
value : List(U8), U64, U64, Path -> Try(Read, TomlProblem.Problem)
value = |bytes, index, level, path|
	match bytes.get(index) {
		Ok('[') => array(bytes, index, level, path)
		Ok('{') => inline_table(bytes, index, level, path)
		_ => {
			read = TomlLex.scalar(bytes, index)?
			Ok({ value: read.value, node: Scalar({ raw: between(bytes, index, read.next), value: read.value }), next: read.next })
		}
	}

array : List(U8), U64, U64, Path -> Try(Read, TomlProblem.Problem)
array = |bytes, open, parent_level, path| {
	level = TomlProblem.deeper(parent_level, open)?
	elements(bytes, open + 1, level, path, [], [])
}

## Values separated by commas, a trailing comma allowed; blanks and comments
## anywhere between.
elements : List(U8), U64, U64, Path, List(TomlValue.Value), List(TomlSyntax.Element) -> Try(Read, TomlProblem.Problem)
elements = |bytes, index, level, path, values, nodes| {
	start = TomlLex.skip_blank(bytes, index)?
	before = between(bytes, index, start)
	if bytes.get(start) == Ok(']') {
		Ok({ value: Array(values), node: Array({ elements: nodes, close: before }), next: start + 1 })
	} else {
		read = value(bytes, start, level, path.append(Index(values.len())))?
		after = TomlLex.skip_blank(bytes, read.next)?
		more = values.append(read.value)
		element = |has_comma| { before, node: read.node, after: between(bytes, read.next, after), has_comma }
		match bytes.get(after) {
			Ok(',') => elements(bytes, after + 1, level, path, more, nodes.append(element(True)))
			Ok(']') => Ok({ value: Array(more), node: Array({ elements: nodes.append(element(False)), close: "" }), next: after + 1 })
			_ => Err(TomlProblem.syntax(after, "`,` or `]` in the array"))
		}
	}
}

## `{ … }` with key/values separated by commas; TOML 1.1 allows line breaks,
## comments and a trailing comma inside.
inline_table : List(U8), U64, U64, Path -> Try(Read, TomlProblem.Problem)
inline_table = |bytes, open, parent_level, path| {
	level = TomlProblem.deeper(parent_level, open)?
	entries(bytes, open + 1, TomlTree.new(level, path), [])
}

entries : List(U8), U64, Tree, List(TomlSyntax.Entry) -> Try(Read, TomlProblem.Problem)
entries = |bytes, index, tree, nodes| {
	start = TomlLex.skip_blank(bytes, index)?
	before = between(bytes, index, start)
	if bytes.get(start) == Ok('}') {
		Ok({ value: TomlTree.to_value(tree), node: InlineTable({ entries: nodes, close: before }), next: start + 1 })
	} else {
		{ tree: grown, pair, next } = key_value(bytes, start, tree, TomlTree.root)?
		after = TomlLex.skip_blank(bytes, next)?
		entry = |has_comma| { before, pair, after: between(bytes, next, after), has_comma }
		match bytes.get(after) {
			Ok(',') => entries(bytes, after + 1, grown, nodes.append(entry(True)))
			Ok('}') => Ok({ value: TomlTree.to_value(grown), node: InlineTable({ entries: nodes.append(entry(False)), close: "" }), next: after + 1 })
			_ => Err(TomlProblem.syntax(after, "`,` or `}` in the inline table"))
		}
	}
}
