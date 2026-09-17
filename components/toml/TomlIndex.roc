import EncodingPath
import TomlLex
import TomlSyntax
import TomlTree
import TomlValue

## Where everything in a document's syntax is, by path.
##
## Built by walking the lines through `TomlTree` as parsing did, so `Index`
## paths name what `Toml.parse` would: `[[x]]` elements with `Index`, tables
## split across the file under one path. Every key/value, header, inline-table
## entry and array element is a `Piece` in document order, with its full path
## and its `Address` in the syntax. Every table and array is a `Container`
## saying how it was made. The document is valid by construction (parsed, or
## edited so it stays valid), so assembling it cannot fail.
TomlIndex :: [].{

	Segment : EncodingPath.Segment

	## `Header` and `ArrayHeader` are header lines, whose sections run to the
	## next header; `Pair` a key/value line; `InlineEntry` and `Element` inside
	## a value.
	Form : [Header, ArrayHeader, Pair, InlineEntry, Element]

	## The line, then positions down through array elements and inline-table
	## entries from that line's value.
	Address : { line : U64, route : List(U64) }

	## `path` is what the piece defines: a header's table (with `Index` for an
	## `[[x]]` element), a key's value, an element's index. `key_length` is how
	## many of `path`'s last segments its key spells (0 for an element), so
	## `path` without them is the table the piece sits in.
	Piece : { path : List(Segment), form : Form, address : Address, key_length : U64 }

	## How a table or array came to be. `Root`; `Implicit` (only named on the
	## way to a header); `Header` (by `[x]`, or an `[[x]]` element); `Dotted`
	## (by dotted keys outside inline tables); `ArrayOfTables` (`[[x]]`);
	## `Inline` (an inline table, or a dotted table inside one); `Array` (an
	## array value).
	Kind : [Root, Implicit, Header, Dotted, ArrayOfTables, Inline, Array]

	Container : { path : List(Segment), kind : Kind }

	Index : { value : TomlValue.Value, pieces : List(Piece), containers : List(Container) }

	build : List(TomlSyntax.Line) -> Index
	build = |lines| {
		walked = walk_lines(lines, 0, TomlTree.new(root_level, []), TomlTree.root, { pieces: [], containers: [] })
		{
			value: TomlTree.to_value(walked.tree),
			pieces: walked.found.pieces,
			containers: document_containers(walked.tree).concat(walked.found.containers),
		}
	}

	## The container at exactly `path`.
	container_at : Index, List(Segment) -> Try(Container, [NotFound])
	container_at = |index, path| index.containers.find_first(|container| container.path == path)

	## Pieces whose path starts with `prefix`, in document order.
	pieces_under : Index, List(Segment) -> List(Piece)
	pieces_under = |index, prefix| index.pieces.keep_if(|piece| piece.path.starts_with(prefix))
}

root_level : U64
root_level = 1

Tree : List(TomlTree.Slot)

Found : { pieces : List(TomlIndex.Piece), containers : List(TomlIndex.Container) }

walk_lines : List(TomlSyntax.Line), U64, Tree, U64, Found -> { tree : Tree, found : Found }
walk_lines = |lines, position, tree, table, found|
	match lines.get(position) {
		Err(_) => { tree, found }
		Ok(line) =>
			match line.body {
				Blank => walk_lines(lines, position + 1, tree, table, found)
				Header(header) => {
					opened = open_header(tree, header)
					form = if header.is_array { ArrayHeader } else { Header }
					piece = { path: TomlTree.path_of(opened.tree, opened.table), form, address: { line: position, route: [] }, key_length: header.key.parts.len() }
					walk_lines(lines, position + 1, opened.tree, opened.table, add_piece(found, piece))
				}
				Pair(pair) => {
					placed = place_pair(tree, table, pair, { line: position, route: [] }, Pair, found)
					walk_lines(lines, position + 1, placed.tree, table, placed.found)
				}
			}
	}

open_header : Tree, TomlSyntax.Header -> { tree : Tree, table : U64 }
open_header = |tree, header| {
	parts = key_parts(header.key)
	opened = if header.is_array { TomlTree.open_array_table(tree, parts) } else { TomlTree.open_table(tree, parts) }
	match opened {
		Ok(done) => done
		Err(_) => crash invalid_document
	}
}

## A key/value into `table`, with its piece and every piece inside its value.
place_pair : Tree, U64, TomlSyntax.Pair, TomlIndex.Address, TomlIndex.Form, Found -> { tree : Tree, found : Found }
place_pair = |tree, table, pair, address, form, found|
	match TomlTree.prepare_key(tree, table, key_parts(pair.key)) {
		Ok(prepared) => {
			piece = { path: prepared.path, form, address, key_length: pair.key.parts.len() }
			read = node_value(pair.node, prepared.level, prepared.path, address, add_piece(found, piece))
			{ tree: TomlTree.insert(prepared.tree, prepared.table, prepared.name, read.value), found: read.found }
		}
		Err(_) => crash invalid_document
	}

## A value node's `Value`, recording the pieces and containers inside it.
## Fold states stay flat records: measured, a list inside a record nested in
## a fold's state is copied on every append.
node_value : TomlSyntax.Node, U64, List(TomlIndex.Segment), TomlIndex.Address, Found -> { value : TomlValue.Value, found : Found }
node_value = |node, level, path, address, found|
	match node {
		Scalar(scalar) => { value: scalar.value, found }
		Array(array) => {
			start = { values: [], pieces: found.pieces, containers: found.containers.append({ path, kind: Array }) }
			walked = array.elements.fold_with_index(start, |{ values, pieces, containers }, element, position| {
				element_path = path.append(Index(position))
				element_address = { line: address.line, route: address.route.append(position) }
				piece = { path: element_path, form: Element, address: element_address, key_length: 0 }
				read = node_value(element.node, level + 1, element_path, element_address, { pieces: pieces.append(piece), containers })
				{ values: values.append(read.value), pieces: read.found.pieces, containers: read.found.containers }
			})
			{ value: Array(walked.values), found: { pieces: walked.pieces, containers: walked.containers } }
		}
		InlineTable(table) => {
			start = { tree: TomlTree.new(level + 1, path), pieces: found.pieces, containers: found.containers.append({ path, kind: Inline }) }
			walked = table.entries.fold_with_index(start, |{ tree, pieces, containers }, entry, position| {
				entry_address = { line: address.line, route: address.route.append(position) }
				placed = place_pair(tree, TomlTree.root, entry.pair, entry_address, InlineEntry, { pieces, containers })
				{ tree: placed.tree, pieces: placed.found.pieces, containers: placed.found.containers }
			})
			{ value: TomlTree.to_value(walked.tree), found: inline_containers(walked.tree, { pieces: walked.pieces, containers: walked.containers }) }
		}
	}

## Key segments as the tree takes them; positions only locate errors, and a
## valid document has none.
key_parts : TomlSyntax.Key -> List(TomlLex.KeyPart)
key_parts = |key| key.parts.map(|part| { name: part.name, at: 0, next: 0 })

add_piece : Found, TomlIndex.Piece -> Found
add_piece = |found, piece| { pieces: found.pieces.append(piece), containers: found.containers }

add_container : Found, TomlIndex.Container -> Found
add_container = |found, container| { pieces: found.pieces, containers: found.containers.append(container) }

## The document tree's tables and arrays of tables by how they were made.
document_containers : Tree -> List(TomlIndex.Container)
document_containers = |tree|
	tree.fold_with_index([], |containers, slot, id|
		match slot {
			TableSlot(table) if id == TomlTree.root => containers.append({ path: table.path, kind: Root })
			TableSlot(table) => containers.append({ path: table.path, kind: origin_kind(table.origin) })
			ArraySlot(array) => containers.append({ path: array.path, kind: ArrayOfTables })
			Taken => containers
		})

## Dotted tables inside an inline table, which are inline too; its root is
## already recorded.
inline_containers : Tree, Found -> Found
inline_containers = |tree, found|
	tree.fold_with_index(found, |so_far, slot, id|
		match slot {
			TableSlot(table) if id != TomlTree.root => add_container(so_far, { path: table.path, kind: Inline })
			_ => so_far
		})

origin_kind : TomlTree.Origin -> TomlIndex.Kind
origin_kind = |origin|
	match origin {
		Implicit => Implicit
		Header => Header
		Dotted => Dotted
	}

invalid_document : Str
invalid_document = "TomlIndex: the document's syntax does not assemble; an edit left it invalid"
