import TomlText
import TomlValue

## A checked document as text, laid out as D-S3-15 has it.
##
## In each table: its scalars and arrays as `key = value` lines, then its
## sub-tables as `[a.b]` sections, then its arrays of tables as `[[a.b]]`
## sections, each group in the table's order. A non-empty array holding only
## tables is an array of tables; tables anywhere else (inside arrays or inline
## tables) are inline. Sections are separated by a blank line, the document
## ends with a line feed, and an empty root writes nothing.
TomlWrite :: [].{

	document : List((Str, TomlValue.Value)), TomlText.Version -> Str
	document = |entries, version| {
		root = lines(entries, version)
		blocks = if root.is_empty() { [] } else { [root] }
		Str.join_with(blocks.concat(sections(entries, [], version)), "\n")
	}
}

## One section per sub-table and per element of each array of tables under
## the table at `path`, depth first.
sections : List((Str, TomlValue.Value)), List(Str), TomlText.Version -> List(Str)
sections = |entries, path, version| {
	tables = entries.keep_oks(|(name, value)| sub_table(name, value)).map(|(name, inner)| table_sections(path.append(name), inner, "[", "]", version))
	arrays = entries.keep_oks(|(name, value)| table_array(name, value)).map(|(name, elements)| elements.map(|inner| table_sections(path.append(name), inner, "[[", "]]", version)).join())
	tables.join().concat(arrays.join())
}

table_sections : List(Str), List((Str, TomlValue.Value)), Str, Str, TomlText.Version -> List(Str)
table_sections = |path, entries, open, close, version| {
	header = "${open}${TomlText.dotted(path, version)}${close}\n"
	[Str.concat(header, lines(entries, version))].concat(sections(entries, path, version))
}

## The `key = value` lines of a table's scalars and inline arrays.
lines : List((Str, TomlValue.Value)), TomlText.Version -> Str
lines = |entries, version|
	entries
	.keep_if(|(_, value)| !is_section(value))
	.map(|entry| "${TomlText.key_value(entry, version)}\n")
	|> Str.join_with("")

is_section : TomlValue.Value -> Bool
is_section = |value|
	match value {
		Table(_) => True
		Array(items) => is_table_array(items)
		_ => False
	}

is_table_array : List(TomlValue.Value) -> Bool
is_table_array = |items|
	!items.is_empty()
	and items.all(|item|
		match item {
			Table(_) => True
			_ => False
		})

sub_table : Str, TomlValue.Value -> Try((Str, List((Str, TomlValue.Value))), [NotATable])
sub_table = |name, value|
	match value {
		Table(inner) => Ok((name, inner))
		_ => Err(NotATable)
	}

table_array : Str, TomlValue.Value -> Try((Str, List(List((Str, TomlValue.Value)))), [NotATableArray])
table_array = |name, value|
	match value {
		Array(items) if is_table_array(items) => Ok((name, items.keep_oks(table_entries)))
		_ => Err(NotATableArray)
	}

table_entries : TomlValue.Value -> Try(List((Str, TomlValue.Value)), [NotATable])
table_entries = |value|
	match value {
		Table(inner) => Ok(inner)
		_ => Err(NotATable)
	}
