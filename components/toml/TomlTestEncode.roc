import Toml

## Golden output in both write modes: escapes, keys, float spellings, dates,
## layout and dict order. These bytes are promised within a major version.
TomlTestEncode :: [].{}

# The module's example.
expect Toml.encode({ server: { port: 8080.U16 } }) == Ok("[server]\nport = 8080\n")

v1_1 : Toml.Write
v1_1 = { version: V1_1 }

## Every escape class, a line feed and non-ASCII text.
escapes : Str
escapes = "q\" b\\ \u(8)\t\u(C)\r \u(0)\u(1F)\u(1B)\u(7F) é😀"

escaped_value : Toml.Value
escaped_value = Table([("s", String(escapes)), ("m", String("a\nb\r\n\"\"\"")), (escapes, Integer(1)), ("", Integer(2)), ("bare-Key_9", Integer(3))])

# Escapes: named where named, else `\u00XX` (1.0) or `\e`/`\xHH` (1.1), uppercase hex.
expect Toml.to_str(escaped_value) == Ok("s = \"q\\\" b\\\\ \\b\\t\\f\\r \\u0000\\u001F\\u001B\\u007F é😀\"\nm = \"\"\"\na\nb\\r\n\\\"\\\"\\\"\"\"\"\n\"q\\\" b\\\\ \\b\\t\\f\\r \\u0000\\u001F\\u001B\\u007F é😀\" = 1\n\"\" = 2\nbare-Key_9 = 3\n")
expect Toml.to_str_with(escaped_value, v1_1) == Ok("s = \"q\\\" b\\\\ \\b\\t\\f\\r \\x00\\x1F\\e\\x7F é😀\"\nm = \"\"\"\na\nb\\r\n\\\"\\\"\\\"\"\"\"\n\"q\\\" b\\\\ \\b\\t\\f\\r \\x00\\x1F\\e\\x7F é😀\" = 1\n\"\" = 2\nbare-Key_9 = 3\n")

# A string starting with a line feed keeps it: the one after `"""` is dropped on reading.
expect Toml.encode({ s: "\nx" }) == Ok("s = \"\"\"\n\nx\"\"\"\n")

# Floats: shortest spellings recognisably floats, lowercase specials, `F32` by its own digits, `Dec` exact.
expect Toml.encode({ a: 1.0.F64, b: -0.0.F64, c: 1e300.F64, d: 1e-7.F64, e: F64.infinity, f: -F64.infinity, g: F64.nan, h: 0.1.F32, i: 12345678.123456789012345678.Dec }) == Ok("a = 1.0\nb = -0.0\nc = 1e300\nd = 1e-7\ne = inf\nf = -inf\ng = nan\nh = 0.1\ni = 12345678.123456789012345678\n")
expect Toml.encode_with({ h: 0.1.F32, n: -F32.infinity }, v1_1) == Ok("h = 0.1\nn = -inf\n")
expect Toml.encode_value({ h: 0.1.F32 }).map_ok(Toml.to_str) == Ok(Ok("h = 0.1\n"))

# Parsed floats keep the spelling they were written with.
expect Toml.parse("x = 1_000.5e+3\ny = +inf\nz = -0.0\nw = 3.14000").map_ok(Toml.to_str) == Ok(Ok("x = 1_000.5e+3\ny = +inf\nz = -0.0\nw = 3.14000\n"))

Moments : { l : Toml.LocalDatetime, o : Toml.OffsetDatetime, o2 : Toml.OffsetDatetime, t : Toml.LocalTime, t2 : Toml.LocalTime, d : Toml.LocalDate }

moments : Moments
moments = {
	l: { date: { year: 1979, month: 5, day: 27 }, time: { hour: 7, minute: 32, second: 9, millisecond: 0, microsecond: 0, nanosecond: 0 } },
	o: { date: { year: 1979, month: 5, day: 27 }, time: { hour: 0, minute: 0, second: 0, millisecond: 0, microsecond: 0, nanosecond: 1 }, offset: { minutes: 0 } },
	o2: { date: { year: 1, month: 1, day: 1 }, time: { hour: 23, minute: 59, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 }, offset: { minutes: -1439 } },
	t: { hour: 7, minute: 32, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 },
	t2: { hour: 7, minute: 32, second: 0, millisecond: 120, microsecond: 30, nanosecond: 0 },
	d: { year: 9999, month: 12, day: 31 },
}

# Dates: `T`, `Z` for offset 0, fraction digits trimmed; `V1_1` drops zero seconds without a fraction.
expect Toml.encode(moments) == Ok("d = 9999-12-31\nl = 1979-05-27T07:32:09\no = 1979-05-27T00:00:00.000000001Z\no2 = 0001-01-01T23:59:00-23:59\nt = 07:32:00\nt2 = 07:32:00.12003\n")
expect Toml.encode_with(moments, v1_1) == Ok("d = 9999-12-31\nl = 1979-05-27T07:32:09\no = 1979-05-27T00:00:00.000000001Z\no2 = 0001-01-01T23:59-23:59\nt = 07:32\nt2 = 07:32:00.12003\n")

# A dict's keys in byte order, quoted when not bare; the empty key quoted.
expect Toml.encode({ d: Dict.from_list([("b", 1.U8), ("a", 2), ("é", 3), ("Z", 4), ("", 5), ("a b", 6)]) }) == Ok("[d]\n\"\" = 5\nZ = 4\na = 2\n\"a b\" = 6\nb = 1\n\"é\" = 3\n")

layout : Toml.Value
layout = Table([
	("title", String("x")),
	("owner", Table([("name", String("n")), ("dob", Table([("y", Integer(1))]))])),
	("empty", Table([])),
	("bins", Array([Table([("name", String("a")), ("sub", Table([("k", Integer(1))])), ("inner", Array([Table([("z", Boolean(True))])]))]), Table([])])),
	("mixed", Array([Integer(1), Table([("a", Integer(1))]), Array([Table([])])])),
	("after", Integer(9)),
	("no.bare", Table([("x", Array([]))])),
])

# Layout: scalars and arrays first, then `[sections]`, then `[[sections]]`, each in
# the table's order and depth first; tables inside arrays inline; a blank line
# between sections, a final line feed, and nothing for an empty document.
expect Toml.to_str(layout) == Ok("title = \"x\"\nmixed = [1, { a = 1 }, [{}]]\nafter = 9\n\n[owner]\nname = \"n\"\n\n[owner.dob]\ny = 1\n\n[empty]\n\n[\"no.bare\"]\nx = []\n\n[[bins]]\nname = \"a\"\n\n[bins.sub]\nk = 1\n\n[[bins.inner]]\nz = true\n\n[[bins]]\n")
expect Toml.to_str(Table([])) == Ok("")
expect Toml.encode({ inner: { a: [{ b: 1.U8 }] } }) == Ok("[inner]\n\n[[inner.a]]\nb = 1\n")
