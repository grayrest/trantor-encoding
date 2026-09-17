import EncodingPath
import TomlAdd
import TomlAppend
import TomlCheck
import TomlIndex
import TomlLines
import TomlLocate
import TomlNodes
import TomlPlan
import TomlProblem
import TomlRemove
import TomlSyntax
import TomlText
import TomlValue

## `set` and `set_with`: a value replaced where it is, merged into the table
## already there, or added where its parent puts it (D-S3-10, D-S3-29,
## D-S3-41, D-S3-46, D-S3-47, D-S3-55.17).
##
## - A scalar, an array value or an inline table replaced by a value that is
##   not a table: its node replaced in place, the key and trailing comment
##   kept.
## - A table replaced by a table in its own style (`Auto`, or the style it
##   has): per-key edits under it, equal values untouched, new keys added,
##   missing keys removed. `[[x]]` elements likewise, element by element.
##   `set([], table)` is this at the root.
## - A scalar or array value becoming a table: `Auto` and `Inline` in place,
##   `Dotted` as dotted key lines in place of its line, `Header` removed and
##   added as a section.
## - Any other change of kind or style: the old pieces removed by path
##   (`TomlRemove`), then the value added, except that a dotted table
##   replaced by key/values keeps the place of its first line.
## - A new key: its missing parents and itself planned by `TomlPlan` from the
##   table that exists, then written by `TomlAdd`.
TomlSet :: [].{

	set : TomlSyntax.File, List(EncodingPath.Segment), TomlValue.Value, TomlLocate.Edit -> Try(TomlSyntax.File, TomlLocate.EditErr)
	set = |file, path, value, edit| settled(file, path, value, edit.?table ?? Auto, edit.?version ?? V1_0)
}

Path : List(EncodingPath.Segment)

Kind : Try(TomlIndex.Kind, [NotFound])

settled : TomlSyntax.File, Path, TomlValue.Value, TomlLocate.Style, TomlText.Version -> Try(TomlSyntax.File, TomlLocate.EditErr)
settled = |file, path, value, style, version| written(file, path, value, style, version, TomlLines.file_ending(file.lines))

## `settled`, with `ending` the line ending for what is added when the file
## has none left.
written : TomlSyntax.File, Path, TomlValue.Value, TomlLocate.Style, TomlText.Version, Str -> Try(TomlSyntax.File, TomlLocate.EditErr)
written = |file, path, value, style, version, ending| {
	_ = checked(value, path)?
	if path.is_empty() {
		match value {
			Table(entries) => merged_table(file, [], entries, version)
			_ => Err(Encode(RootNotATable))
		}
	} else {
		index = TomlIndex.build(file.lines)
		match TomlLocate.value_at(index.value, path) {
			Ok(_) => replaced(file, index, path, value, style, version)
			Err(NotFound(missing)) => created(file, index, path, missing, value, style, version, ending)
			Err(problem) => Err(problem)
		}
	}
}

## The value's own problems, and parents that would nest past the limit.
checked : TomlValue.Value, Path -> Try({}, TomlLocate.EditErr)
checked = |value, path|
	if path.len() > TomlProblem.max_depth {
		Err(Encode(TooDeep(path.take_first(TomlProblem.max_depth))))
	} else {
		TomlCheck.value_at(value, path).map_err(|problem| Encode(problem))
	}

created : TomlSyntax.File, TomlIndex.Index, Path, Path, TomlValue.Value, TomlLocate.Style, TomlText.Version, Str -> Try(TomlSyntax.File, TomlLocate.EditErr)
created = |file, index, path, missing, value, style, version, ending| {
	base = missing.drop_last(1)
	names = path.drop_first(base.len()).keep_oks(key_name)
	if names.len() + base.len() == path.len() {
		plan = TomlPlan.chain(context_at(index, base), base, names, value, style) ? |NotPossible(chosen)| StyleNotPossible({ path, style: chosen })
		Ok(TomlAdd.add(file, base, plan, version, ending))
	} else {
		Err(NotFound(missing))
	}
}

replaced : TomlSyntax.File, TomlIndex.Index, Path, TomlValue.Value, TomlLocate.Style, TomlText.Version -> Try(TomlSyntax.File, TomlLocate.EditErr)
replaced = |file, index, path, value, style, version| {
	kind = TomlIndex.container_at(index, path).map_ok(|container| container.kind).map_err(|_| NotFound)
	is_element = kind == Ok(Header) and path.last().map_ok(is_index) == Ok(True)
	match kind {
		_ if is_emptied(kind, value, style) => restyled(file, index, path, kind, value, if kind == Ok(Dotted) { Dotted } else { Header }, version)
		Ok(Header) if is_element => replaced_element(file, path, value, style, version)
		Ok(Header) | Ok(Implicit) | Ok(Dotted) | Ok(ArrayOfTables) =>
			if keeps_style(kind, value, style) {
				merged(file, index, path, value, version)
			} else {
				restyled(file, index, path, kind, value, style, version)
			}
		_ => replaced_leaf(file, index, path, kind, value, style, version)
	}
}

## A dotted or implicit table set to an empty table in its own style: merged
## key by key it would vanish with its last key (D-S3-40), so it is written
## again as an empty table, `name = {}` in place of a dotted table's first
## line (`Dotted` writes an empty table as `{}`) and a `[name]` section for an
## implicit one (D-S3-46).
is_emptied : Kind, TomlValue.Value, TomlLocate.Style -> Bool
is_emptied = |kind, value, style| (kind == Ok(Dotted) or kind == Ok(Implicit)) and value == Table([]) and keeps_style(kind, value, style)

## An `[[x]]` element stays one: a table merges into it; `Dotted` cannot be.
replaced_element : TomlSyntax.File, Path, TomlValue.Value, TomlLocate.Style, TomlText.Version -> Try(TomlSyntax.File, TomlLocate.EditErr)
replaced_element = |file, path, value, style, version|
	match value {
		Table(_) if style == Dotted => Err(StyleNotPossible({ path, style: Dotted }))
		Table(entries) => merged_table(file, path, entries, version)
		_ => Err(NotATable(path))
	}

## A scalar, array value or inline table replaced. A dotted table inside an
## inline table has no piece of its own, so it goes and comes back; merged
## into an empty table it would vanish.
replaced_leaf : TomlSyntax.File, TomlIndex.Index, Path, Kind, TomlValue.Value, TomlLocate.Style, TomlText.Version -> Try(TomlSyntax.File, TomlLocate.EditErr)
replaced_leaf = |file, index, path, kind, value, style, version| {
	is_plain = !is_section(value) or style == Auto or style == Inline
	match index.pieces.find_first(|candidate| candidate.path == path and candidate.form != Header and candidate.form != ArrayHeader) {
		Err(_) if value == Table([]) => readded(file, path, value, style, version)
		_ if is_plain and kind == Ok(Inline) and is_table(value) => merged(file, index, path, value, version)
		Err(_) => readded(file, path, value, style, version)
		Ok(piece) if is_plain => Ok(in_place(file, piece, value, version))
		Ok(piece) =>
			match (style, planned(index, path, value, style)?) {
				(Dotted, Entries(entries)) if piece.form == Pair => Ok(replaced_line(file, piece, path, entries, version, KeepComment))
				(Dotted, _) => Ok(in_place(file, piece, value, version))
				_ => readded(file, path, value, style, version)
			}
	}
}

## A table or array of tables written in another form: removed and added
## again, a dotted table replaced by key/values in place of its first line.
restyled : TomlSyntax.File, TomlIndex.Index, Path, Kind, TomlValue.Value, TomlLocate.Style, TomlText.Version -> Try(TomlSyntax.File, TomlLocate.EditErr)
restyled = |file, index, path, kind, value, style, version|
	match (kind, planned(index, path, value, style)?) {
		(Ok(Dotted), Entries(entries)) => {
			pieces = TomlIndex.pieces_under(index, path)
			match pieces.find_first(|piece| piece.form == Pair) {
				Ok(first) => {
					others = pieces.drop_if(|piece| piece.address.line == first.address.line)
					Ok(replaced_line(TomlRemove.without_pieces(file, others), first, path, entries, version, DropComment))
				}
				Err(_) => readded(file, path, value, style, version)
			}
		}
		_ => readded(file, path, value, style, version)
	}

## The file's line ending is taken before the removal, which can leave a
## file with no line break to read it from.
readded : TomlSyntax.File, Path, TomlValue.Value, TomlLocate.Style, TomlText.Version -> Try(TomlSyntax.File, TomlLocate.EditErr)
readded = |file, path, value, style, version| {
	ending = TomlLines.file_ending(file.lines)
	removed = TomlRemove.remove(file, path)?
	written(removed, path, value, style, version, ending)
}

## The plan for `value` at `path` from its parent, refused as that style.
planned : TomlIndex.Index, Path, TomlValue.Value, TomlLocate.Style -> Try(TomlPlan.Plan, TomlLocate.EditErr)
planned = |index, path, value, style| {
	parent = path.drop_last(1)
	TomlPlan.target(context_at(index, parent), path, value, style).map_err(|NotPossible(chosen)| StyleNotPossible({ path, style: chosen }))
}

keeps_style : Kind, TomlValue.Value, TomlLocate.Style -> Bool
keeps_style = |kind, value, style|
	match (kind, style) {
		(Ok(Header), Auto) | (Ok(Header), Header) | (Ok(Implicit), Auto) | (Ok(Implicit), Header) => is_table(value)
		(Ok(Dotted), Auto) | (Ok(Dotted), Dotted) => is_table(value)
		(Ok(ArrayOfTables), Auto) | (Ok(ArrayOfTables), Header) => is_table_array(value)
		_ => False
	}

merged : TomlSyntax.File, TomlIndex.Index, Path, TomlValue.Value, TomlText.Version -> Try(TomlSyntax.File, TomlLocate.EditErr)
merged = |file, index, path, value, version|
	match (value, TomlLocate.value_at(index.value, path)) {
		(Table(entries), _) => merged_table(file, path, entries, version)
		(Array(items), Ok(Array(old))) => merged_elements(file, path, items, old, version)
		_ => Err(NotATable(path))
	}

## New and changed keys set first, so a dotted table never loses its last key
## before its new ones are in; then keys the new table lacks removed.
merged_table : TomlSyntax.File, Path, List((Str, TomlValue.Value)), TomlText.Version -> Try(TomlSyntax.File, TomlLocate.EditErr)
merged_table = |file, path, entries, version| {
	old = match TomlLocate.value_at(TomlIndex.build(file.lines).value, path) {
		Ok(Table(found)) => found
		_ => []
	}
	updated = entries.fold_try(file, |so_far, (name, value)|
		if old.any(|(key, current)| key == name and current == value) {
			Ok(so_far)
		} else {
			settled(so_far, path.append(Key(name)), value, Auto, version)
		})?
	old.fold_try(updated, |so_far, (name, _)|
		if entries.any(|(key, _)| key == name) {
			Ok(so_far)
		} else {
			TomlRemove.remove(so_far, path.append(Key(name)))
		})
}

## Elements set in place, new ones appended, extra ones removed from the end.
merged_elements : TomlSyntax.File, Path, List(TomlValue.Value), List(TomlValue.Value), TomlText.Version -> Try(TomlSyntax.File, TomlLocate.EditErr)
merged_elements = |file, path, items, old, version| {
	kept = items.take_first(old.len()).map_with_index(|item, position| (item, position))
	updated = kept.fold_try(file, |so_far, (item, position)|
		if old.get(position) == Ok(item) { Ok(so_far) } else { settled(so_far, path.append(Index(position)), item, Auto, version) })?
	appended = items.drop_first(old.len()).fold_try(updated, |so_far, item| TomlAppend.append(so_far, path, item, version))?
	extra = List.repeat({}, old.len() - items.take_first(old.len()).len()).map_with_index(|_, offset| old.len() - 1 - offset)
	extra.fold_try(appended, |so_far, position| TomlRemove.remove(so_far, path.append(Index(position))))
}

## The piece's value node replaced by `value`'s, written with its line's
## ending.
in_place : TomlSyntax.File, TomlIndex.Piece, TomlValue.Value, TomlText.Version -> TomlSyntax.File
in_place = |file, piece, value, version| {
	updated = file.lines.update(piece.address.line, |line| {
		node = TomlNodes.fresh(value, version, TomlLines.ending_near(file.lines, line))
		TomlNodes.update_line(line, piece.address.route, |_| node)
	})
	{ bom: file.bom, lines: updated ?? file.lines }
}

## The key/value line of `piece` replaced by `entries` (relative to the
## parent of `path`) spelled through the line's own key segments for `path`,
## with its indentation; the last line keeps its ending.
replaced_line : TomlSyntax.File, TomlIndex.Piece, Path, List(TomlPlan.Entry), TomlText.Version, [KeepComment, DropComment] -> TomlSyntax.File
replaced_line = |file, piece, path, entries, version, comment|
	match file.lines.get(piece.address.line) {
		Ok(line) =>
			match line.body {
				Pair(pair) => {
					prefix = TomlLines.key_prefix(pair.key, path.len() - (piece.path.len() - piece.key_length))
					ending = TomlLines.ending_near(file.lines, line)
					last = entries.len() - 1
					lines = entries.map_with_index(|(keys, value), position| {
						indent: line.indent,
						body: Pair(TomlAdd.pair(prefix, (keys.drop_first(1), value), version, ending)),
						trailing: if position == 0 and comment == KeepComment { line.trailing } else { "" },
						ending: if position == last { line.ending } else { ending },
					})
					{ bom: file.bom, lines: TomlLines.replace(file.lines, piece.address.line, lines) }
				}
				_ => file
			}
		Err(_) => file
	}

## How a new table under `table` is chosen.
context_at : TomlIndex.Index, Path -> TomlPlan.Context
context_at = |index, table|
	match TomlIndex.container_at(index, table).map_ok(|container| container.kind) {
		Ok(Inline) | Ok(Array) => InInline
		Ok(Dotted) => InDotted
		Ok(Implicit) => InImplicit
		Ok(Header) if has_only_inline_children(index, table) => InInlineChildren
		_ => InHeader
	}

## A header table (not the root) with children, all of them inline tables.
has_only_inline_children : TomlIndex.Index, Path -> Bool
has_only_inline_children = |index, table|
	match TomlLocate.value_at(index.value, table) {
		Ok(Table(entries)) if !entries.is_empty() =>
			entries.all(|(name, _)| TomlIndex.container_at(index, table.append(Key(name))).map_ok(|container| container.kind) == Ok(Inline))
		_ => False
	}

key_name : EncodingPath.Segment -> Try(Str, [NotAKey])
key_name = |segment|
	match segment {
		Key(name) => Ok(name)
		Index(_) => Err(NotAKey)
	}

is_index : EncodingPath.Segment -> Bool
is_index = |segment|
	match segment {
		Index(_) => True
		Key(_) => False
	}

is_table : TomlValue.Value -> Bool
is_table = |value|
	match value {
		Table(_) => True
		_ => False
	}

is_table_array : TomlValue.Value -> Bool
is_table_array = |value|
	match value {
		Array(items) => TomlPlan.is_table_array(items)
		_ => False
	}

is_section : TomlValue.Value -> Bool
is_section = |value| is_table(value) or is_table_array(value)
