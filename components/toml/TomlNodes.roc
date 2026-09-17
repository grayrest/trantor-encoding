import TomlLayout
import TomlParse
import TomlSyntax
import TomlText
import TomlValue

## Value nodes as edits change them: a node found by route below a line's
## value, a node written for a new value, and elements and entries added to
## or taken from arrays and inline tables following their layout
## (`TomlLayout`).
TomlNodes :: [].{

	Node : TomlSyntax.Node

	## `line` with the node at `route` below its value changed by `change`;
	## lines without a value are unchanged.
	update_line : TomlSyntax.Line, List(U64), (Node -> Node) -> TomlSyntax.Line
	update_line = |line, route, change|
		match line.body {
			Pair(pair) => { ..line, body: Pair({ key: pair.key, equals: pair.equals, node: TomlNodes.update(pair.node, route, change) }) }
			_ => line
		}

	## The node at `route` below `line`'s value.
	line_node : TomlSyntax.Line, List(U64) -> Try(Node, [NotFound])
	line_node = |line, route|
		match line.body {
			Pair(pair) => TomlNodes.at(pair.node, route)
			_ => Err(NotFound)
		}

	## The node at `route` below `node`: each step an element of an array or
	## the value of an inline table's entry.
	at : Node, List(U64) -> Try(Node, [NotFound])
	at = |node, route|
		match route.first() {
			Err(_) => Ok(node)
			Ok(position) => {
				child =
					match node {
						Array(array) => array.elements.get(position).map_ok(|element| element.node)
						InlineTable(table) => table.entries.get(position).map_ok(|entry| entry.pair.node)
						Scalar(_) => Err(OutOfBounds)
					}
				match child {
					Ok(inner) => TomlNodes.at(inner, route.drop_first(1))
					Err(_) => Err(NotFound)
				}
			}
		}

	## The key of the inline-table entry at `route` below `line`'s value.
	entry_key : TomlSyntax.Line, List(U64) -> Try(TomlSyntax.Key, [NotFound])
	entry_key = |line, route| {
		parent = TomlNodes.line_node(line, route.drop_last(1))?
		match (parent, route.last()) {
			(InlineTable(table), Ok(position)) => table.entries.get(position).map_ok(|entry| entry.pair.key).map_err(|_| NotFound)
			_ => Err(NotFound)
		}
	}

	update : Node, List(U64), (Node -> Node) -> Node
	update = |node, route, change|
		match route.first() {
			Err(_) => change(node)
			Ok(position) => {
				rest = route.drop_first(1)
				match node {
					Array(array) => {
						elements = array.elements.update(position, |element| { ..element, node: TomlNodes.update(element.node, rest, change) }) ?? array.elements
						Array({ elements, close: array.close })
					}
					InlineTable(table) => {
						entries = table.entries.update(position, |entry| { ..entry, pair: { ..entry.pair, node: TomlNodes.update(entry.pair.node, rest, change) } }) ?? table.entries
						InlineTable({ entries, close: table.close })
					}
					Scalar(scalar) => Scalar(scalar)
				}
			}
		}

	## `value` as `version` writes it on one line, a multi-line string's line
	## breaks written as `ending`.
	fresh : TomlValue.Value, TomlText.Version, Str -> Node
	fresh = |value, version, ending| {
		text = TomlText.inline(value, version).replace_each("\n", ending)
		match TomlParse.read("${fresh_key} = ${text}").map_ok(|parsed| parsed.file.lines.first()) {
			Ok(Ok(line)) =>
				match line.body {
					Pair(pair) => pair.node
					_ => crash unreadable_value
				}
			_ => crash unreadable_value
		}
	}

	## An array with `element` added after its last element.
	with_element : Node, Node -> Node
	with_element = |node, element|
		match node {
			Array(array) => {
				layout = TomlLayout.appended(array_layout(array), array_empty_space)
				nodes = array.elements.map(|old| old.node).append(element)
				Array({ elements: layout.gaps.map2(nodes, |gap, inner| { before: gap.before, node: inner, after: gap.after, has_comma: gap.has_comma }), close: layout.close })
			}
			other => other
		}

	## An inline table with `pair` added after its last entry.
	with_entry : Node, TomlSyntax.Pair -> Node
	with_entry = |node, pair|
		match node {
			InlineTable(table) => {
				layout = TomlLayout.appended(table_layout(table), table_empty_space)
				pairs = table.entries.map(|old| old.pair).append(pair)
				InlineTable({ entries: layout.gaps.map2(pairs, |gap, inner| { before: gap.before, pair: inner, after: gap.after, has_comma: gap.has_comma }), close: layout.close })
			}
			other => other
		}

	## An array or inline table without its child at `position`.
	without : Node, U64 -> Node
	without = |node, position|
		match node {
			Array(array) => {
				layout = TomlLayout.removed(array_layout(array), position)
				nodes = array.elements.map(|old| old.node).drop_at(position)
				Array({ elements: layout.gaps.map2(nodes, |gap, inner| { before: gap.before, node: inner, after: gap.after, has_comma: gap.has_comma }), close: layout.close })
			}
			InlineTable(table) => {
				layout = TomlLayout.removed(table_layout(table), position)
				pairs = table.entries.map(|old| old.pair).drop_at(position)
				InlineTable({ entries: layout.gaps.map2(pairs, |gap, inner| { before: gap.before, pair: inner, after: gap.after, has_comma: gap.has_comma }), close: layout.close })
			}
			Scalar(scalar) => Scalar(scalar)
		}
}

fresh_key : Str
fresh_key = "x"

unreadable_value : Str
unreadable_value = "TomlNodes: a written value did not read back"

## What an empty array holds around its first element (`[1]`), and an empty
## inline table (`{ a = 1 }`), as `TomlText` writes them.
array_empty_space : Str
array_empty_space = ""

table_empty_space : Str
table_empty_space = " "

array_layout : { elements : List(TomlSyntax.Element), close : Str } -> TomlLayout.Layout
array_layout = |array| { gaps: array.elements.map(|element| { before: element.before, after: element.after, has_comma: element.has_comma }), close: array.close }

table_layout : { entries : List(TomlSyntax.Entry), close : Str } -> TomlLayout.Layout
table_layout = |table| { gaps: table.entries.map(|entry| { before: entry.before, after: entry.after, has_comma: entry.has_comma }), close: table.close }
