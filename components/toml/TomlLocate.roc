import EncodingPath
import TomlCheck
import TomlValue

## What reading and editing a document by path share: the error, the edit
## options, and finding the value at a path.
##
## Defined below `TomlDocument` so the edit modules it delegates to can use
## them; `TomlDocument` and `Toml` alias them.
TomlLocate :: [].{

	Segment : EncodingPath.Segment

	## `Toml.EditErr`.
	EditErr : [
		NotFound(List(Segment)),
		NotATable(List(Segment)),
		NotAnArray(List(Segment)),
		StyleNotPossible({ path : List(Segment), style : [Header, Dotted] }),
		Encode(TomlCheck.EncodeErr),
	]

	## `Toml.Edit`.
	Edit : { version ?: [V1_0, V1_1], table ?: Style }

	## How a new table is written: chosen from its place (`Auto`), as an
	## inline table, as a `[header]` section, or as dotted keys.
	Style : [Auto, Inline, Header, Dotted]

	## The value at `path` in `value`. `NotFound` names the path through the
	## missing key or index, `NotATable` and `NotAnArray` the path of the value
	## a key or an index was applied to.
	value_at : TomlValue.Value, List(Segment) -> Try(TomlValue.Value, EditErr)
	value_at = |value, path| value_from(value, path, 0)
}

value_from : TomlValue.Value, List(EncodingPath.Segment), U64 -> Try(TomlValue.Value, TomlLocate.EditErr)
value_from = |value, path, position|
	match path.get(position) {
		Err(_) => Ok(value)
		Ok(segment) => {
			through = path.take_first(position + 1)
			match (segment, value) {
				(Key(name), Table(entries)) =>
					match entries.find_first(|(key, _)| key == name) {
						Ok((_, inner)) => value_from(inner, path, position + 1)
						Err(_) => Err(NotFound(through))
					}
				(Index(index), Array(items)) =>
					match items.get(index) {
						Ok(inner) => value_from(inner, path, position + 1)
						Err(_) => Err(NotFound(through))
					}
				(Key(_), _) => Err(NotATable(path.take_first(position)))
				(Index(_), _) => Err(NotAnArray(path.take_first(position)))
			}
		}
	}
