import EncodingPath
import TomlIndex
import TomlLines
import TomlLocate
import TomlNodes
import TomlSyntax

## `remove`: everything whose path starts with the path given (D-S3-40), each
## piece taking its span (D-S3-31).
##
## - A key/value line: the line, with a multi-line value and a trailing
##   comment, and its comment block.
## - A `[header]` or `[[x]]` section: its comment block, the section, and the
##   empty lines after it; sections under it go by their own paths, wherever
##   they are.
## - An inline-table entry or array element: by `TomlLayout`, unless its line
##   or an enclosing entry already goes.
##
## Spans that reach the end of the file take the empty lines above them.
TomlRemove :: [].{

	remove : TomlSyntax.File, List(EncodingPath.Segment) -> Try(TomlSyntax.File, TomlLocate.EditErr)
	remove = |file, path|
		if path.is_empty() {
			Err(NotFound([]))
		} else {
			index = TomlIndex.build(file.lines)
			_ = TomlLocate.value_at(index.value, path)?
			Ok(TomlRemove.without_pieces(file, TomlIndex.pieces_under(index, path)))
		}

	## `file` without `pieces` and what each takes.
	without_pieces : TomlSyntax.File, List(TomlIndex.Piece) -> TomlSyntax.File
	without_pieces = |file, pieces| {
		spans = merged(pieces.keep_oks(|piece| line_span(file.lines, piece)))
		inner = innermost_routes(pieces.keep_if(|piece| is_in_value(piece) and !spans.any(|span| covers(span, piece.address.line))))
		without_routes = inner.fold(file.lines, remove_at_route)
		{ bom: file.bom, lines: spans.fold_rev(without_routes, |span, lines| TomlLines.remove(lines, span.start, span.end)) }
	}
}

Span : { start : U64, end : U64 }

is_in_value : TomlIndex.Piece -> Bool
is_in_value = |piece| piece.form == InlineEntry or piece.form == Element

line_span : List(TomlSyntax.Line), TomlIndex.Piece -> Try(Span, [InValue])
line_span = |lines, piece|
	match piece.form {
		Header | ArrayHeader => Ok(TomlLines.section_span(lines, piece.address.line))
		Pair => Ok({ start: TomlLines.comment_start(lines, piece.address.line), end: piece.address.line + 1 })
		InlineEntry | Element => Err(InValue)
	}

covers : Span, U64 -> Bool
covers = |span, line| line >= span.start and line < span.end

## Spans in order with overlapping and touching ones joined.
## `take_first(len - 1)`, not `drop_last(1)`: `drop_last(1).append(…)` corrupts
## memory in a compiled app (a segfault in `TomlRemove.merged`).
merged : List(Span) -> List(Span)
merged = |spans|
	spans
	.sort_by(|span| span.start)
	.fold([], |joined, span|
		match joined.last() {
			Ok(previous) if span.start <= previous.end => joined.take_first(joined.len() - 1).append({ start: previous.start, end: U64.max(previous.end, span.end) })
			_ => joined.append(span)
		})

## Addresses of the pieces not inside another one's value, last first, so
## taking one out leaves the positions of the rest unchanged.
innermost_routes : List(TomlIndex.Piece) -> List(TomlIndex.Address)
innermost_routes = |pieces| {
	addresses = pieces.map(|piece| piece.address)
	addresses
	.keep_if(|address| !addresses.any(|other| other.line == address.line and other.route.len() < address.route.len() and address.route.starts_with(other.route)))
	.sort_with(later_first)
}

later_first : TomlIndex.Address, TomlIndex.Address -> [Before, Same, After]
later_first = |left, right|
	if left.line != right.line {
		if left.line > right.line { Before } else { After }
	} else {
		route_order(left.route, right.route)
	}

## Routes compared position by position, the greater first.
route_order : List(U64), List(U64) -> [Before, Same, After]
route_order = |left, right|
	match (left.first(), right.first()) {
		(Ok(a), Ok(b)) if a == b => route_order(left.drop_first(1), right.drop_first(1))
		(Ok(a), Ok(b)) => if a > b { Before } else { After }
		(Ok(_), Err(_)) => Before
		(Err(_), Ok(_)) => After
		(Err(_), Err(_)) => Same
	}

remove_at_route : List(TomlSyntax.Line), TomlIndex.Address -> List(TomlSyntax.Line)
remove_at_route = |lines, address|
	match address.route.last() {
		Ok(position) => lines.update(address.line, |line| TomlNodes.update_line(line, address.route.drop_last(1), |node| TomlNodes.without(node, position))) ?? lines
		Err(_) => lines
	}
