import EncodingPath
import TomlIndex
import TomlLines
import TomlNodes
import TomlPlace
import TomlPlan
import TomlSyntax
import TomlText
import TomlValue

## A plan written into a file: entries into the table they belong to as that
## table was made, blocks as new sections (D-S3-53).
##
## - The root or a `[header]` table: key/value lines after its last key line,
##   with that line's indentation and line ending (D-S3-10, D-S3-20).
## - A table only named on the way to a header: a new `[header]` section for
##   it (D-S3-46).
## - A dotted table: dotted keys after its last one, spelled through the same
##   key segments.
## - An inline table: entries after its last one (`TomlLayout`).
##
## New lines take the ending of the line they follow where there is one, else
## the file's first; a multi-line string's line breaks are that ending
## (D-S3-43).
TomlAdd :: [].{

	## How the first block's header line is written: generated, or like an
	## existing header line (a new `[[x]]` element copies the last one's).
	Heading : [Written, Like(TomlSyntax.Line)]

	## `fallback` is the line ending when the file has none left (an edit that
	## removed every line first).
	add : TomlSyntax.File, List(EncodingPath.Segment), TomlPlan.Plan, TomlText.Version, Str -> TomlSyntax.File
	add = |file, table, plan, version, fallback| {
		ending = TomlLines.file_ending_or(file.lines, fallback)
		match plan {
			Entries(entries) => add_entries(file, table, entries, version, ending)
			Blocks(blocks) => TomlAdd.add_blocks(file, blocks, version, Written, ending)
		}
	}

	## `blocks` as sections where the first one's family puts them; `ending`
	## is the file's line ending.
	add_blocks : TomlSyntax.File, List(TomlPlan.Block), TomlText.Version, Heading, Str -> TomlSyntax.File
	add_blocks = |file, blocks, version, heading, ending|
		match blocks.first() {
			Err(_) => file
			Ok(first) => {
				index = TomlIndex.build(file.lines)
				place = TomlPlace.sections(index, file.lines, first.path)
				separator = if place.is_spaced { [TomlLines.blank_line(ending)] } else { [] }
				sections = blocks.map_with_index(|block, position| block_lines(block, version, ending, if position == 0 { heading } else { Written }))
				leading = if place.position > 0 { separator } else { [] }
				added = leading.concat(sections.intersperse(separator).join())
				{ bom: file.bom, lines: TomlLines.insert(file.lines, place.position, added) }
			}
		}

	## The pair `keys = value` spelled after `prefix`, its value written with
	## `ending`.
	pair : TomlLines.Prefix, (List(Str), TomlValue.Value), TomlText.Version, Str -> TomlSyntax.Pair
	pair = |prefix, (keys, value), version, ending| {
		key: TomlLines.joined_key(prefix, keys, version),
		equals: TomlLines.equals,
		node: TomlNodes.fresh(value, version, ending),
	}
}

entries_missing : Str
entries_missing = "TomlAdd: the table to add entries to has no place in the document"

add_entries : TomlSyntax.File, List(EncodingPath.Segment), List(TomlPlan.Entry), TomlText.Version, Str -> TomlSyntax.File
add_entries = |file, table, entries, version, ending| {
	index = TomlIndex.build(file.lines)
	match TomlIndex.container_at(index, table).map_ok(|container| container.kind) {
		Ok(Root) | Ok(Header) => in_section(file, index, table, entries, version, ending)
		Ok(Implicit) => TomlAdd.add_blocks(file, [{ path: table, is_array: False, entries }], version, Written, ending)
		Ok(Dotted) => as_dotted_keys(file, index, table, entries, version, ending)
		Ok(Inline) => in_inline_table(file, index, table, entries, version, ending)
		_ => crash entries_missing
	}
}

in_section : TomlSyntax.File, TomlIndex.Index, List(EncodingPath.Segment), List(TomlPlan.Entry), TomlText.Version, Str -> TomlSyntax.File
in_section = |file, index, table, entries, version, ending| {
	spot = TomlPlace.section_key(index, file.lines, table)
	lines = entry_lines(spot, TomlLines.no_prefix, entries, version, ending)
	# A first root key directly above a header or its comments gets a blank
	# line after it.
	is_crowded = table.is_empty() and spot.neighbour.is_err() and file.lines.get(spot.position).map_ok(|line| !TomlLines.is_empty(line)) == Ok(True)
	spaced = if is_crowded { lines.append(TomlLines.blank_line(ending)) } else { lines }
	{ bom: file.bom, lines: TomlLines.insert(file.lines, spot.position, spaced) }
}

as_dotted_keys : TomlSyntax.File, TomlIndex.Index, List(EncodingPath.Segment), List(TomlPlan.Entry), TomlText.Version, Str -> TomlSyntax.File
as_dotted_keys = |file, index, table, entries, version, ending|
	match TomlPlace.dotted_key(index, file.lines, table) {
		Ok(found) => { bom: file.bom, lines: TomlLines.insert(file.lines, found.spot.position, entry_lines(found.spot, found.prefix, entries, version, ending)) }
		Err(_) => crash entries_missing
	}

in_inline_table : TomlSyntax.File, TomlIndex.Index, List(EncodingPath.Segment), List(TomlPlan.Entry), TomlText.Version, Str -> TomlSyntax.File
in_inline_table = |file, index, table, entries, version, ending|
	match TomlPlace.inline_node(index, file.lines, table) {
		Ok(found) => {
			updated = file.lines.update(found.line, |line| {
				near = own_ending(line, ending)
				TomlNodes.update_line(line, found.route, |node| entries.fold(node, |so_far, entry| TomlNodes.with_entry(so_far, TomlAdd.pair(found.prefix, entry, version, near))))
			})
			{ bom: file.bom, lines: updated ?? file.lines }
		}
		Err(_) => crash entries_missing
	}

## Key/value lines placed at `spot`, indented and ended like its neighbour.
entry_lines : TomlPlace.Spot, TomlLines.Prefix, List(TomlPlan.Entry), TomlText.Version, Str -> List(TomlSyntax.Line)
entry_lines = |spot, prefix, entries, version, file_ending| {
	indent = spot.neighbour.map_ok(|line| line.indent) ?? ""
	ending = spot.neighbour.map_ok(|line| own_ending(line, file_ending)) ?? file_ending
	entries.map(|entry| TomlLines.pair_line(indent, TomlAdd.pair(prefix, entry, version, ending), ending))
}

## `line`'s ending, or the file's when it has none (the last line).
own_ending : TomlSyntax.Line, Str -> Str
own_ending = |line, file_ending| if line.ending == "" { file_ending } else { line.ending }

block_lines : TomlPlan.Block, TomlText.Version, Str, TomlAdd.Heading -> List(TomlSyntax.Line)
block_lines = |block, version, ending, heading| {
	header =
		match heading {
			Written => TomlLines.header_line(block.path, block.is_array, version, ending)
			Like(line) => { ..line, trailing: "", ending }
		}
	[header].concat(block.entries.map(|entry| TomlLines.pair_line("", TomlAdd.pair(TomlLines.no_prefix, entry, version, ending), ending)))
}
