import Toml

## Documents through `Toml.parse`: structure, keys, strings, line endings and
## the byte-order mark.
TomlTestParse :: [].{}

## The value under `key` in a one-key document.
only : Str -> Try(Toml.Value, [NotOneKey])
only = |text|
	match Toml.parse(text) {
		Ok(Table([(_, value)])) => Ok(value)
		_ => Err(NotOneKey)
	}

## The keys of the table under `key` (the root for `""`), in their order.
keys_in : Try(Toml.Value, Toml.Err), Str -> List(Str)
keys_in = |parsed, key|
	match parsed {
		Ok(Table(entries)) if key == "" => entries.map(|(name, _)| name)
		Ok(Table(entries)) =>
			match entries.find_first(|(name, _)| name == key) {
				Ok((_, Table(inner))) => inner.map(|(name, _)| name)
				_ => []
			}
		_ => []
	}

float : F64 -> Toml.Value
float = |value| Float(Toml.float_from_f64(value))

# Tables by headers, dotted keys and inline tables; arrays of tables.
expect Toml.parse("a = 1\n[t]\nb.c = 'x'\n[[arr]]\nd = [1, [2]]\n[[arr]]\ne = { f = 1.5 }\n") == Ok(Table([
	("a", Integer(1)),
	("t", Table([("b", Table([("c", String("x"))]))])),
	("arr", Array([Table([("d", Array([Integer(1), Array([Integer(2)])]))]), Table([("e", Table([("f", float(1.5))]))])])),
]))
expect Toml.parse("") == Ok(Table([]))
expect Toml.parse("# only a comment\n\n") == Ok(Table([]))

# Document order is kept; a super-table defined after its child keeps its place.
expect keys_in(Toml.parse("[x.y]\nz = 1\n[x]\nw = 2"), "x") == ["y", "w"]
expect keys_in(Toml.parse("b = 1\na = 2\nc.d = 3\nx = { z = 1, y = 2 }"), "") == ["b", "a", "c", "x"]

# Headers reach into the latest `[[x]]` element and through dotted tables.
expect Toml.parse("[[fruit]]\nname = 'apple'\n[fruit.physical]\ncolor = 'red'\n[[fruit]]\nname = 'banana'") == Ok(Table([
	("fruit", Array([
		Table([("name", String("apple")), ("physical", Table([("color", String("red"))]))]),
		Table([("name", String("banana"))]),
	])),
]))
expect Toml.parse("[fruit]\napple.color = 'red'\n[fruit.apple.texture]\nsmooth = true") == Ok(Table([
	("fruit", Table([("apple", Table([("color", String("red")), ("texture", Table([("smooth", Boolean(True))]))]))])),
]))

# TOML 1.1: inline tables over lines with comments and a trailing comma;
# seconds omitted; `\e` and `\x` escapes.
expect only("t = {\n  a = 1, # one\n  b.c = 2,\n}") == Ok(Table([("a", Integer(1)), ("b", Table([("c", Integer(2))]))]))
expect only("t = 07:32") == Ok(LocalTime({ hour: 7, minute: 32, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 }))
expect only("s = \"\\e\\xE9\"") == Ok(String("\u(1B)é"))

# Arrays: mixed kinds, line breaks, comments, a trailing comma.
expect only("a = [\n  1, # c\n  'two',\n  [3.0],\n]") == Ok(Array([Integer(1), String("two"), Array([float(3.0)])]))
expect only("a = []") == Ok(Array([]))

# Keys: quoted keys equal to bare ones collide; empty quoted keys are keys.
expect Toml.parse("\"\" = 1\n'' . x = 2").is_err()
expect Toml.parse("\"a b\".'c' = 1") == Ok(Table([("a b", Table([("c", Integer(1))]))]))
expect Toml.parse("3.14159 = 'pi'") == Ok(Table([("3", Table([("14159", String("pi"))]))]))

# Values of each kind.
expect only("x = 1979-05-27T07:32:00.999999-07:00") == Ok(OffsetDatetime({ date: { year: 1979, month: 5, day: 27 }, time: { hour: 7, minute: 32, second: 0, millisecond: 999, microsecond: 999, nanosecond: 0 }, offset: { minutes: -420 } }))
expect only("x = 1979-05-27T07:32:00Z") == only("x = 1979-05-27 07:32:00+00:00")
expect only("x = 0xff") == Ok(Integer(255))
expect only("x = -inf") == Ok(float(-F64.infinity))
expect only("x = nan") == Ok(float(F64.nan))
expect only("x = false") == Ok(Boolean(False))

# A leading byte-order mark is skipped, and positions count from after it.
expect Toml.parse("\u(FEFF)a = 1") == Ok(Table([("a", Integer(1))]))
expect Toml.parse("\u(FEFF)a = ?") == Err(Syntax({ line: 1, column: 5, expected: "a value" }))
expect Toml.parse("a = \u(FEFF)").is_err()

# CRLF reads as LF does.
expect Toml.parse("a = 1\r\n[t]\r\nb = [\r\n  2,\r\n]\r\n") == Toml.parse("a = 1\n[t]\nb = [\n  2,\n]\n")
expect Toml.parse("a = 1\r\n").is_ok()

# Multi-line strings: LF and CRLF files read equal; an escaped `\r` stays;
# `"""` inside, a trailing quote and a leading line break.
expect only("s = \"\"\"\nx\r\ny\"\"\"") == Ok(String("x\ny"))
expect only("s = \"\"\"\r\nx\r\ny\"\"\"") == only("s = \"\"\"\nx\ny\"\"\"")
expect only("s = '''\r\nx\r\ny'''") == Ok(String("x\ny"))
expect only("s = \"\"\"a\\r\nb\"\"\"") == Ok(String("a\r\nb"))
expect only("s = \"\"\"three \"\"\\\" quotes\"\"\"") == Ok(String("three \"\"\" quotes"))
expect only("s = \"\"\"ends in a quote\"\"\"\"") == Ok(String("ends in a quote\""))
expect only("s = \"\"\"\nleading break dropped\"\"\"") == Ok(String("leading break dropped"))
expect only("s = \"\"\"\n\nonly the first\"\"\"") == Ok(String("\nonly the first"))

# `Toml.path` spells keys as segments.
expect Toml.path(["a", "b"]) == [Key("a"), Key("b")]
expect Toml.path([]) == []
