import EncodingPath
import TomlIndex
import TomlLines
import TomlNodes
import TomlSyntax

## Where an edit puts what it adds: a key/value in a section (after the
## table's last key, D-S3-10), a dotted key after the dotted table's last key,
## an entry in the inline table holding a table, and new sections after their
## family's last section (D-S3-53).
TomlPlace :: [].{

	Segment : EncodingPath.Segment

	## The line a new key/value goes before, and the key line it follows, if
	## any, to take its indentation and line ending from.
	Spot : { position : U64, neighbour : Try(TomlSyntax.Line, [NoKeys]) }

	## After the last key line in the section of `table` (the root or a header
	## table), else after its header line, else where the root's content ends.
	section_key : TomlIndex.Index, List(TomlSyntax.Line), List(Segment) -> Spot
	section_key = |index, lines, table|
		match index.pieces.find_last(|piece| piece.form == Pair and piece.path.drop_last(piece.key_length) == table) {
			Ok(piece) => spot_after(lines, piece.address.line)
			Err(_) =>
				match index.pieces.find_first(|piece| is_header_form(piece.form) and piece.path == table) {
					Ok(header) => { position: header.address.line + 1, neighbour: Err(NoKeys) }
					Err(_) => { position: TomlLines.content_end_of_root(lines), neighbour: Err(NoKeys) }
				}
		}

	## After the last dotted key line under `table`, with the key segments
	## that spell `table` from that line's section.
	dotted_key : TomlIndex.Index, List(TomlSyntax.Line), List(Segment) -> Try({ spot : Spot, prefix : TomlLines.Prefix }, [NotFound])
	dotted_key = |index, lines, table| {
		piece = index.pieces.find_last(|candidate| candidate.form == Pair and is_inside(candidate, table)) ? |_| NotFound
		line = lines.get(piece.address.line) ? |_| NotFound
		match line.body {
			Pair(pair) => Ok({ spot: spot_after(lines, piece.address.line), prefix: TomlLines.key_prefix(pair.key, spelled_count(piece, table)) })
			_ => Err(NotFound)
		}
	}

	## The inline table node `table` is written in (its line and route), with
	## the key segments that spell `table` inside it.
	inline_node : TomlIndex.Index, List(TomlSyntax.Line), List(Segment) -> Try({ line : U64, route : List(U64), prefix : TomlLines.Prefix }, [NotFound])
	inline_node = |index, lines, table|
		match index.pieces.find_last(|candidate| candidate.form == InlineEntry and is_inside(candidate, table)) {
			Ok(entry) => {
				line = lines.get(entry.address.line) ? |_| NotFound
				key = TomlNodes.entry_key(line, entry.address.route)?
				Ok({ line: entry.address.line, route: entry.address.route.drop_last(1), prefix: TomlLines.key_prefix(key, spelled_count(entry, table)) })
			}
			Err(_) => {
				own = index.pieces.find_first(|candidate| candidate.path == table and !is_header_form(candidate.form)) ? |_| NotFound
				Ok({ line: own.address.line, route: own.address.route, prefix: TomlLines.no_prefix })
			}
		}

	## Where sections headed `path` go: after the last section of the deepest
	## family that has one (the sections under the array for an `[[x]]`
	## element, else under the parent), else at the end of the file; spaced by
	## one blank line unless the family has none between its sections.
	sections : TomlIndex.Index, List(TomlSyntax.Line), List(Segment) -> { position : U64, is_spaced : Bool }
	sections = |index, lines, path|
		match family(index, path.drop_last(1)) {
			Ok(headers) => {
				last = headers.last() ?? 0
				{ position: TomlLines.content_end(lines, last), is_spaced: is_spaced(lines, headers) }
			}
			Err(_) => { position: TomlLines.content_end_of_file(lines), is_spaced: True }
		}
}

spot_after : List(TomlSyntax.Line), U64 -> TomlPlace.Spot
spot_after = |lines, position| { position: position + 1, neighbour: lines.get(position).map_err(|_| NoKeys) }

is_header_form : TomlIndex.Form -> Bool
is_header_form = |form| form == Header or form == ArrayHeader

## Whether `piece` defines something strictly inside `table` from a section
## or inline table that `table` is in, so its key spells `table`'s segments.
is_inside : TomlIndex.Piece, List(EncodingPath.Segment) -> Bool
is_inside = |piece, table|
	piece.path.len() > table.len() and piece.path.starts_with(table) and table.starts_with(piece.path.drop_last(piece.key_length))

## How many of the piece's key segments spell `table`.
spelled_count : TomlIndex.Piece, List(EncodingPath.Segment) -> U64
spelled_count = |piece, table| table.len() - (piece.path.len() - piece.key_length)

## The header lines of the deepest family starting at `prefix`.
family : TomlIndex.Index, List(EncodingPath.Segment) -> Try(List(U64), [NoFamily])
family = |index, prefix|
	if prefix.is_empty() {
		Err(NoFamily)
	} else {
		headers = index.pieces.keep_if(|piece| is_header_form(piece.form) and piece.path.starts_with(prefix)).map(|piece| piece.address.line)
		if headers.is_empty() { family(index, prefix.drop_last(1)) } else { Ok(headers) }
	}

## Whether a blank line comes before any but the first of `headers` (and its
## comment block); a family of one section counts as spaced.
is_spaced : List(TomlSyntax.Line), List(U64) -> Bool
is_spaced = |lines, headers|
	headers.len() < 2
	or headers.drop_first(1).any(|header| {
		start = TomlLines.comment_start(lines, header)
		start > 0 and lines.get(start - 1).map_ok(TomlLines.is_empty) == Ok(True)
	})
