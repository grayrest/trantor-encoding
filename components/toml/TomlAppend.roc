import EncodingPath
import TomlAdd
import TomlCheck
import TomlIndex
import TomlLines
import TomlLocate
import TomlNodes
import TomlPlan
import TomlSyntax
import TomlText
import TomlValue

## `append`: a value after the last element of an array (D-S3-31).
##
## An array value gains an element following its layout (`TomlLayout`). An
## array of tables gains an `[[x]]` section after its last element's sections,
## its header spelled like the last element's; only a table can be one
## (`NotATable`). What the append writes is `version`'s.
TomlAppend :: [].{

	append : TomlSyntax.File, List(EncodingPath.Segment), TomlValue.Value, TomlText.Version -> Try(TomlSyntax.File, TomlLocate.EditErr)
	append = |file, path, value, version|
		if path.is_empty() {
			Err(NotFound([]))
		} else {
			index = TomlIndex.build(file.lines)
			match TomlLocate.value_at(index.value, path)? {
				Array(items) => {
					element = path.append(Index(items.len()))
					_ = TomlCheck.value_at(value, element) ? |problem| Encode(problem)
					if TomlIndex.container_at(index, path).map_ok(|container| container.kind) == Ok(ArrayOfTables) {
						table_element(file, index, path, items.len(), value, version)
					} else {
						Ok(array_element(file, index, path, value, version))
					}
				}
				_ => Err(NotAnArray(path))
			}
		}
}

no_array : Str
no_array = "TomlAppend: the array has no node in the document"

table_element : TomlSyntax.File, TomlIndex.Index, List(EncodingPath.Segment), U64, TomlValue.Value, TomlText.Version -> Try(TomlSyntax.File, TomlLocate.EditErr)
table_element = |file, index, path, count, value, version|
	match value {
		Table(_) => {
			blocks = TomlPlan.element_blocks(path, [value], count)
			last_header = index.pieces.find_last(|piece| piece.form == ArrayHeader and piece.path.drop_last(1) == path).map_ok(|piece| piece.address.line)
			heading =
				match last_header.map_ok(|line| file.lines.get(line)) {
					Ok(Ok(line)) => Like(line)
					_ => Written
				}
			Ok(TomlAdd.add_blocks(file, blocks, version, heading, TomlLines.file_ending(file.lines)))
		}
		_ => Err(NotATable(path))
	}

array_element : TomlSyntax.File, TomlIndex.Index, List(EncodingPath.Segment), TomlValue.Value, TomlText.Version -> TomlSyntax.File
array_element = |file, index, path, value, version|
	match index.pieces.find_first(|piece| piece.path == path and piece.form != Header and piece.form != ArrayHeader) {
		Ok(piece) => {
			updated = file.lines.update(piece.address.line, |line| {
				ending = TomlLines.ending_near(file.lines, line)
				TomlNodes.update_line(line, piece.address.route, |node| TomlNodes.with_element(node, TomlNodes.fresh(value, version, ending)))
			})
			{ bom: file.bom, lines: updated ?? file.lines }
		}
		Err(_) => crash no_array
	}
