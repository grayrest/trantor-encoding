import EncodingPath
import TomlAppend
import TomlIndex
import TomlLocate
import TomlParse
import TomlRemove
import TomlSet
import TomlSyntax
import TomlValue

## `Toml.Document`: a parsed document that keeps its text, and what reading
## and editing it answer.
##
## The document is its `TomlSyntax.File`; `to_value` and `get` assemble the
## value from it through `TomlIndex`. Edits work on the file (`file`,
## `from_file`) and locate what they change through `index`.
TomlDocument :: [].{

	Segment : EncodingPath.Segment

	## What `get`, `set`, `set_with`, `remove` and `append` fail with
	## (`Toml.EditErr`).
	EditErr : TomlLocate.EditErr

	## How an edit writes what it creates (`Toml.Edit`).
	Edit : TomlLocate.Edit

	Document :: { bom : Str, lines : List(TomlSyntax.Line) }.{

		## The document's text: the source itself until it is edited.
		to_str : Document -> Str
		to_str = |document| TomlSyntax.file_text(TomlDocument.file(document))

		## The document as a table, as `Toml.parse` reads its text.
		to_value : Document -> TomlValue.Value
		to_value = |document| TomlDocument.index(document).value

		## The value at `path`; `[]` is the whole document. `NotFound` names
		## the path through the missing key or index, `NotATable` and
		## `NotAnArray` the path of the value a key or an index was applied to.
		get : Document, List(Segment) -> Try(TomlValue.Value, EditErr)
		get = |document, path| TomlLocate.value_at(Document.to_value(document), path)

		## `value` at `path`, written by `TomlWrite`'s rules in TOML 1.0 where
		## the edit creates something and following the document's layout
		## where it changes what is there; `[]` takes a table and edits the
		## root key by key.
		set : Document, List(Segment), TomlValue.Value -> Try(Document, EditErr)
		set = |document, path, value| Document.set_with(document, path, value, {})

		## `set` with the version new text is written in and the style of new
		## tables.
		set_with : Document, List(Segment), TomlValue.Value, Edit -> Try(Document, EditErr)
		set_with = |document, path, value, edit| TomlSet.set(TomlDocument.file(document), path, value, edit).map_ok(TomlDocument.from_file)

		## `value` after the last element of the array at `path`: an element
		## following the array's layout, or an `[[x]]` section for an array of
		## tables, which takes only tables.
		append : Document, List(Segment), TomlValue.Value -> Try(Document, EditErr)
		append = |document, path, value| TomlAppend.append(TomlDocument.file(document), path, value, V1_0).map_ok(TomlDocument.from_file)

		## The document without everything whose path starts with `path`, each
		## key/value and section with its comment block; `[]` is `NotFound`.
		remove : Document, List(Segment) -> Try(Document, EditErr)
		remove = |document, path| TomlRemove.remove(TomlDocument.file(document), path).map_ok(TomlDocument.from_file)
	}

	## A document from text, refused as `Toml.parse` refuses it.
	parse : Str -> Try(Document, TomlParse.Err)
	parse = |text| TomlParse.read(text).map_ok(|parsed| TomlDocument.from_file(parsed.file))

	file : Document -> TomlSyntax.File
	file = |document| { bom: document.bom, lines: document.lines }

	from_file : TomlSyntax.File -> Document
	from_file = |syntax| { bom: syntax.bom, lines: syntax.lines }

	index : Document -> TomlIndex.Index
	index = |document| TomlIndex.build(document.lines)
}
