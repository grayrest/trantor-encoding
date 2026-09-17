import Toml

## Size, not behaviour: the scanner recurses per byte and per expression, the
## tree grows per key, so a call taken out of tail position or a table copied
## per insert shows up here on sizes no other suite reaches.
TomlStress :: [].{}

key_count : U64
key_count = 10000

string_bytes : U64
string_bytes = 200000

## Entries in the table under `key`, or the array's length.
size_under : Try(Toml.Value, Toml.Err), Str -> U64
size_under = |parsed, key|
	match parsed {
		Ok(Table(entries)) =>
			match entries.find_first(|(name, _)| name == key) {
				Ok((_, Table(inner))) => inner.len()
				Ok((_, Array(items))) => items.len()
				_ => 0
			}
		_ => 0
	}

numbered_keys : Str
numbered_keys = List.repeat({}, key_count).map_with_index(|_, index| "k${index.to_str()} = ${index.to_str()}\n") |> Str.join_with("")

# 10,000 keys in one table, 10,000 `[[x]]` elements, 10,000 array elements.
expect size_under(Toml.parse("[t]\n${numbered_keys}"), "t") == key_count
expect size_under(Toml.parse(Str.repeat("[[x]]\na = 1\n", key_count)), "x") == key_count
expect size_under(Toml.parse("a = [${Str.repeat("1,", key_count)}]"), "a") == key_count

# The same sizes through a document: text back whole, the value assembled from
# its syntax, and a lookup near the end.
expect Toml.parse_document("[t]\n${numbered_keys}").map_ok(|document| document.to_str()) == Ok("[t]\n${numbered_keys}")
expect Toml.parse_document("[t]\n${numbered_keys}").map_ok(|document| size_under(Ok(document.to_value()), "t")) == Ok(key_count)
expect Toml.parse_document(Str.repeat("[[x]]\na = 1\n", key_count)).map_ok(|document| document.get([Key("x"), Index(key_count - 1), Key("a")])) == Ok(Ok(Integer(1)))
expect Toml.parse_document("a = [${Str.repeat("1,", key_count)}]").map_ok(|document| size_under(Ok(document.to_value()), "a")) == Ok(key_count)

# Long strings of each kind, a long comment and a long bare key.
expect Toml.parse("s = \"${Str.repeat("a", string_bytes)}\"").is_ok()
expect Toml.parse("s = \"${Str.repeat("\\t", string_bytes)}\"").is_ok()
expect Toml.parse("s = '${Str.repeat("a", string_bytes)}'").is_ok()
expect Toml.parse("s = \"\"\"${Str.repeat("a\n", string_bytes)}\"\"\"").is_ok()
expect Toml.parse("s = '''${Str.repeat("a\r\n", string_bytes)}'''").is_ok()
expect Toml.parse("# ${Str.repeat("c", string_bytes)}\n${Str.repeat("k", string_bytes)} = 1").is_ok()
