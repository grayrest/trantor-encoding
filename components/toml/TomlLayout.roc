## The trivia of an array or inline table as a new child is added or a child
## is taken out, following the container's layout (D-S3-31, D-S3-42).
##
## A layout is each child's gap (`before` it, `after` it up to its comma,
## whether a comma follows) and `close`, the trivia after the last comma.
##
## A container is written line by line when a child's `before` holds a line
## break. There a new child takes a line of its own, indented like the last
## one that starts a line, with a comma after it when the last child had one;
## a comment trailing the old last line stays on that line. A removed child
## takes its line with its trailing comment and the comment lines directly
## above it. On one line, a new child follows `, `, and a removed child takes
## one adjacent comma.
TomlLayout :: [].{

	Gap : { before : Str, after : Str, has_comma : Bool }

	Layout : { gaps : List(Gap), close : Str }

	## A gap for a new last child. `empty_space` is what an empty container
	## puts on either side of its first child.
	appended : Layout, Str -> Layout
	appended = |layout, empty_space|
		match layout.gaps.last() {
			Err(_) => {
				after = if layout.close == "" { empty_space } else { layout.close }
				{ gaps: [{ before: empty_space, after, has_comma: False }], close: "" }
			}
			Ok(last) =>
				match layout.gaps.find_last(|gap| gap.before.contains(line_feed)) {
					Ok(starting) => appended_on_lines(layout, last, line_lead(starting.before))
					Err(_) => appended_in_line(layout, last)
				}
		}

	## The layout without the child at `position`.
	removed : Layout, U64 -> Layout
	removed = |layout, position|
		match layout.gaps.get(position) {
			Err(_) => layout
			Ok(gap) => {
				is_last = position + 1 == layout.gaps.len()
				following = if is_last { layout.close } else { layout.gaps.get(position + 1).map_ok(|next| next.before) ?? "" }
				trail = Str.concat(gap.after, following)
				if gap.before.contains(line_feed) and trail.contains(line_feed) {
					removed_on_lines(layout, position, gap, Str.concat(line_head(gap.before), from_line_break(trail)))
				} else {
					removed_in_line(layout, position, gap)
				}
			}
		}
}

line_feed : Str
line_feed = "\n"

carriage_return : Str
carriage_return = "\r"

comment_mark : Str
comment_mark = "#"

in_line_separator : Str
in_line_separator = " "

appended_on_lines : TomlLayout.Layout, TomlLayout.Gap, Str -> TomlLayout.Layout
appended_on_lines = |layout, last, lead|
	if last.has_comma {
		split = at_first_break(layout.close)
		{ gaps: layout.gaps.append({ before: Str.concat(split.head, lead), after: "", has_comma: True }), close: split.tail }
	} else {
		split = at_first_break(last.after)
		gaps = with_last(layout.gaps, { before: last.before, after: "", has_comma: True })
		{ gaps: gaps.append({ before: Str.concat(split.head, lead), after: split.tail, has_comma: False }), close: layout.close }
	}

appended_in_line : TomlLayout.Layout, TomlLayout.Gap -> TomlLayout.Layout
appended_in_line = |layout, last| {
	separator = if layout.gaps.len() > 1 { last.before } else { in_line_separator }
	if last.has_comma {
		{ gaps: layout.gaps.append({ before: separator, after: "", has_comma: True }), close: layout.close }
	} else {
		gaps = with_last(layout.gaps, { before: last.before, after: "", has_comma: True })
		{ gaps: gaps.append({ before: separator, after: last.after, has_comma: False }), close: layout.close }
	}
}

## `joined` is what stays between the neighbours: the text before the removed
## child's comment block, then the rest after its line.
removed_on_lines : TomlLayout.Layout, U64, TomlLayout.Gap, Str -> TomlLayout.Layout
removed_on_lines = |layout, position, gap, joined| {
	gaps = layout.gaps.drop_at(position)
	if position < gaps.len() {
		{ gaps: gaps.update(position, |next| { ..next, before: joined }) ?? gaps, close: layout.close }
	} else if gap.has_comma or position == 0 {
		{ gaps, close: joined }
	} else {
		{ gaps: gaps.update(position - 1, |previous| { before: previous.before, after: Str.concat(previous.after, joined), has_comma: False }) ?? gaps, close: layout.close }
	}
}

removed_in_line : TomlLayout.Layout, U64, TomlLayout.Gap -> TomlLayout.Layout
removed_in_line = |layout, position, gap| {
	gaps = layout.gaps.drop_at(position)
	if position < gaps.len() {
		{ gaps: gaps.update(position, |next| { ..next, before: gap.before }) ?? gaps, close: layout.close }
	} else if gap.has_comma or position == 0 {
		{ gaps, close: layout.close }
	} else {
		{ gaps: gaps.update(position - 1, |previous| { before: previous.before, after: Str.concat(previous.after, gap.after), has_comma: False }) ?? gaps, close: layout.close }
	}
}

## `take_first(len - 1)`, not `drop_last(1)`: `drop_last(1).append(…)` corrupts
## memory in a compiled app (a segfault in `TomlRemove.merged`).
with_last : List(TomlLayout.Gap), TomlLayout.Gap -> List(TomlLayout.Gap)
with_last = |gaps, gap| if gaps.is_empty() { [gap] } else { gaps.take_first(gaps.len() - 1).append(gap) }

## Text before its first line break (`head`) and from the break on (`tail`,
## empty without one).
at_first_break : Str -> { head : Str, tail : Str }
at_first_break = |text|
	match text.split_first(line_feed) {
		Ok({ before, after }) if before.ends_with(carriage_return) => { head: before.drop_suffix(carriage_return), tail: "\r\n${after}" }
		Ok({ before, after }) => { head: before, tail: "\n${after}" }
		Err(_) => { head: text, tail: "" }
	}

from_line_break : Str -> Str
from_line_break = |text| at_first_break(text).tail

## The last line break and the indentation after it.
line_lead : Str -> Str
line_lead = |text|
	match text.split_last(line_feed) {
		Ok({ before, after }) if before.ends_with(carriage_return) => "\r\n${after}"
		Ok(parts) => "\n${parts.after}"
		Err(_) => text
	}

## Text before the line a child starts on and before the comment lines
## directly above it.
line_head : Str -> Str
line_head = |text|
	match text.split_last(line_feed) {
		Ok(parts) => {
			head = parts.before.drop_suffix(carriage_return)
			if last_line(head).trim_start().starts_with(comment_mark) and head.contains(line_feed) { line_head(head) } else { head }
		}
		Err(_) => text
	}

last_line : Str -> Str
last_line = |text|
	match text.split_last(line_feed) {
		Ok(parts) => parts.after
		Err(_) => text
	}
