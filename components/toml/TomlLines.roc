import EncodingPath
import TomlSyntax
import TomlText

## Lines as edits see them: comment blocks, where a section's content ends,
## the line ending new lines take, and inserting and removing lines so a file
## without a final line break keeps having none.
##
## A comment line is a blank line holding a comment; an empty line holds
## nothing but spaces. The comment block of a line is the comment lines
## directly above it, a blank line or anything else stopping it.
TomlLines :: [].{

	Line : TomlSyntax.Line

	is_comment : Line -> Bool
	is_comment = |line| is_blank(line) and line.trailing != ""

	is_empty : Line -> Bool
	is_empty = |line| is_blank(line) and line.trailing == ""

	## Where the comment block above `position` starts (`position` when there
	## is none).
	comment_start : List(Line), U64 -> U64
	comment_start = |lines, position|
		if position > 0 and lines.get(position - 1).map_ok(TomlLines.is_comment) == Ok(True) {
			TomlLines.comment_start(lines, position - 1)
		} else {
			position
		}

	## Where the content of the section headed at `header` ends: before the
	## next header's comment block and the empty lines before it.
	content_end : List(Line), U64 -> U64
	content_end = |lines, header| {
		next = TomlSyntax.section_end(lines, header)
		before_empty_lines(lines, before_next_block(lines, next), header + 1)
	}

	## What removing the section headed at `header` takes: its comment block,
	## the section, and the empty lines after it, stopping at the next
	## header's comment block.
	section_span : List(Line), U64 -> { start : U64, end : U64 }
	section_span = |lines, header| {
		start: TomlLines.comment_start(lines, header),
		end: before_next_block(lines, TomlSyntax.section_end(lines, header)),
	}

	## Where the file's content ends: before its empty last lines.
	content_end_of_file : List(Line) -> U64
	content_end_of_file = |lines| before_empty_lines(lines, lines.len(), 0)

	## Where the root table's content ends: before the first header's comment
	## block and the empty lines above it.
	content_end_of_root : List(Line) -> U64
	content_end_of_root = |lines| {
		first_header = lines.find_first_index(is_header) ?? lines.len()
		before_empty_lines(lines, before_next_block(lines, first_header), 0)
	}

	## The file's first line ending, or a line feed when it has none.
	file_ending : List(Line) -> Str
	file_ending = |lines| TomlLines.file_ending_or(lines, "\n")

	## The file's first line ending, or `fallback` when it has none.
	file_ending_or : List(Line), Str -> Str
	file_ending_or = |lines, fallback| lines.find_first(|line| line.ending != "").map_ok(|line| line.ending) ?? fallback

	## `line`'s ending, or the file's when it is the last line and has none.
	ending_near : List(Line), Line -> Str
	ending_near = |lines, line| if line.ending == "" { TomlLines.file_ending(lines) } else { line.ending }

	## `added` before `position`. At the end of a file with no final line
	## break, the old last line takes a break and the new last line has none.
	insert : List(Line), U64, List(Line) -> List(Line)
	insert = |lines, position, added| {
		split = lines.split_at(position)
		is_unterminated_end = split.others.is_empty() and split.before.last().map_ok(|line| line.ending == "") == Ok(True)
		match (added.first(), added.last()) {
			(Ok(first), Ok(_)) if is_unterminated_end => {
				before = with_last_ending(split.before, first.ending)
				before.concat(with_last_ending(added, ""))
			}
			_ => split.before.concat(added).concat(split.others)
		}
	}

	## Lines `start` up to `end` taken out. A span reaching the end of the
	## file also takes the empty lines above it, and a last line without a
	## break leaves the new last line without one; a span starting the file
	## takes the empty lines below it.
	remove : List(Line), U64, U64 -> List(Line)
	remove = |lines, start, end|
		if end >= lines.len() {
			from = before_empty_lines(lines, start, 0)
			kept = lines.take_first(from)
			if lines.last().map_ok(|line| line.ending == "") == Ok(True) { with_last_ending(kept, "") } else { kept }
		} else if start == 0 {
			lines.drop_first(after_empty_lines(lines, end))
		} else {
			lines.take_first(start).concat(lines.drop_first(end))
		}

	## Line `position` replaced by `replacement`.
	replace : List(Line), U64, List(Line) -> List(Line)
	replace = |lines, position, replacement| lines.take_first(position).concat(replacement).concat(lines.drop_first(position + 1))

	## A key spelled for `version`, segments joined by `.`.
	key : List(Str), TomlText.Version -> TomlSyntax.Key
	key = |names, version| {
		parts: names.map(|name| { raw: TomlText.key(name, version), name }),
		dots: names.drop_first(1).map(|_| key_dot),
	}

	## Key segments new segments are spelled after, and the dot that joins
	## them.
	Prefix : { key : TomlSyntax.Key, dot : Str }

	no_prefix : Prefix
	no_prefix = { key: { parts: [], dots: [] }, dot: key_dot }

	## The first `count` segments of `whole`, joined to what follows as
	## `whole` joins its next segment.
	key_prefix : TomlSyntax.Key, U64 -> Prefix
	key_prefix = |whole, count| {
		parts = whole.parts.take_first(count)
		inner = parts.drop_first(1).len()
		dots = whole.dots.take_first(inner)
		{ key: { parts, dots }, dot: whole.dots.drop_first(inner).first() ?? (whole.dots.first() ?? key_dot) }
	}

	## `prefix` then `names`.
	joined_key : Prefix, List(Str), TomlText.Version -> TomlSyntax.Key
	joined_key = |prefix, names, version| {
		added = TomlLines.key(names, version)
		between = if prefix.key.parts.is_empty() or names.is_empty() { [] } else { [prefix.dot] }
		{ parts: prefix.key.parts.concat(added.parts), dots: prefix.key.dots.concat(between).concat(added.dots.map(|_| prefix.dot)) }
	}

	## What new pairs put between their key and value.
	equals : Str
	equals = " = "

	## A key/value on a line of its own.
	pair_line : Str, TomlSyntax.Pair, Str -> Line
	pair_line = |indent, pair, ending| { indent, body: Pair(pair), trailing: "", ending }

	## `[a.b]` or `[[a.b]]` for a table at `path`; indices are not spelled.
	header_line : List(EncodingPath.Segment), Bool, TomlText.Version, Str -> Line
	header_line = |path, is_array, version, ending| {
		names = path.keep_oks(key_name)
		{ indent: "", body: Header({ is_array, open: "", key: TomlLines.key(names, version), close: "" }), trailing: "", ending }
	}

	blank_line : Str -> Line
	blank_line = |ending| { indent: "", body: Blank, trailing: "", ending }
}

key_dot : Str
key_dot = "."

key_name : EncodingPath.Segment -> Try(Str, [NotAKey])
key_name = |segment|
	match segment {
		Key(name) => Ok(name)
		Index(_) => Err(NotAKey)
	}

is_blank : TomlLines.Line -> Bool
is_blank = |line|
	match line.body {
		Blank => True
		_ => False
	}

is_header : TomlLines.Line -> Bool
is_header = |line|
	match line.body {
		Header(_) => True
		_ => False
	}

## Before the comment block of the header at `position`; `position` itself at
## the end of the file.
before_next_block : List(TomlLines.Line), U64 -> U64
before_next_block = |lines, position| if position < lines.len() { TomlLines.comment_start(lines, position) } else { position }

## Back from `position` over empty lines, not past `floor`.
before_empty_lines : List(TomlLines.Line), U64, U64 -> U64
before_empty_lines = |lines, position, floor|
	if position > floor and lines.get(position - 1).map_ok(TomlLines.is_empty) == Ok(True) {
		before_empty_lines(lines, position - 1, floor)
	} else {
		position
	}

## Forward from `position` over empty lines.
after_empty_lines : List(TomlLines.Line), U64 -> U64
after_empty_lines = |lines, position|
	if lines.get(position).map_ok(TomlLines.is_empty) == Ok(True) {
		after_empty_lines(lines, position + 1)
	} else {
		position
	}

## `take_first(len - 1)`, not `drop_last(1)`: `drop_last(1).append(…)` corrupts
## memory in a compiled app (a segfault in `TomlRemove.merged`).
with_last_ending : List(TomlLines.Line), Str -> List(TomlLines.Line)
with_last_ending = |lines, ending|
	match lines.last() {
		Ok(line) => lines.take_first(lines.len() - 1).append({ ..line, ending })
		Err(_) => lines
	}
