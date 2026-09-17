app [main!] { pf: platform "../target/trantor/app/platform/main.roc" }

import pf.OsStr
import pf.Stdout
import pf.Base64
import pf.Hex
import pf.Csv
import pf.Toml

## README.md's examples, section by section, so they keep compiling and giving
## the results its comments show. Each result prints with `Str.inspect`, line
## breaks shown as `\n`, so `expected` reads like the comments. Where the
## README chains `?` over error types one function cannot return together,
## this uses `??` with a fallback; the calls are the README's.

base64_and_hex : {} -> List(Str)
base64_and_hex = |{}| [
	Str.inspect(Base64.encode("hello".to_utf8())),
	Str.inspect(Base64.encode_url([0xFB, 0xFF])),
	Str.inspect(Base64.decode("aGVsbG8")),
	Str.inspect(Base64.decode("aGVsbG9=")),
	Str.inspect(Hex.encode([0xCA, 0xFE])),
	Str.inspect(Hex.decode("CAFE")),
	Str.inspect(Hex.decode("caf")),
]

rows_and_tables : {} -> List(Str)
rows_and_tables = |{}| {
	rows = Csv.parse("name,age\nada,36\n\"hopper, grace\",45") ?? []
	table = Csv.table("name,age\nada,36\ngrace,45") ?? { headers: [], rows: [] }
	row = table.rows.get(1) ?? Csv.Row.new([], [])
	[
		Str.inspect(rows),
		Str.inspect(Csv.to_str(rows)),
		Str.inspect(Csv.to_str_with(rows, Csv.Dialect.tsv)),
		Str.inspect(table.column("name")),
		Str.inspect(row.get("age")),
	]
}

Person : { name : Str, age : U32, email ?: Str }

## Decoded people as the README's comment describes them, or where decoding
## stopped.
described : Try(List(Person), [Parse(Csv.Err), Mismatch({ path : List(Csv.Segment), expected : Str }), MissingRequiredField(Str)]) -> Str
described = |people|
	match people {
		Ok(values) => Str.join_with(values.map(|person| "${person.name} ${person.age.to_str()} ${person.?email ?? "with no email"}"), "; ")
		Err(Mismatch(problem)) => "Mismatch at ${Str.inspect(problem.path)}"
		Err(problem) => Str.inspect(problem)
	}

rows_example : List(Person)
rows_example = [{ name: "ada", age: 36, email: "ada@example.com" }, { name: "grace", age: 45 }]

typed_records : {} -> List(Str)
typed_records = |{}| {
	people : Try(List(Person), _)
	people = Csv.decode("name,age,email\nada,36,ada@example.com\ngrace,45,")

	rows : List(Person)
	rows = rows_example

	bad : Try(List(Person), _)
	bad = Csv.decode("name,age\nada,36\ngrace,old")

	[
		described(people),
		Str.inspect(Csv.encode(rows)),
		Str.inspect(Csv.encode_columns(rows, ["name", "age", "email"])),
		Str.inspect(Csv.encode_columns(rows, ["name", "age"])),
		described(bad),
	]
}

cell_forms : {} -> List(Str)
cell_forms = |{}| {
	rounded : Try(List({ x : F64 }), _)
	rounded = Csv.decode("x\n9007199254740993")
	[Str.inspect(rounded.map_ok(|records| records.map(|record| record.x)))]
}

Event : { name : Str, on : Toml.LocalDate, at : Toml.OffsetDatetime }

dates_in_cells : {} -> List(Str)
dates_in_cells = |{}| {
	events : Try(List(Event), _)
	events = Csv.decode("name,on,at\nlaunch,2026-03-08,2026-03-08T09:30:00-05:00")

	late : Try(List({ at : Toml.LocalTime }), _)
	late = Csv.decode("at\n24:00:00")

	[
		Str.inspect(Csv.encode(events ?? [])),
		Str.inspect(late.map_ok(|_| {})),
	]
}

excel : {} -> List(Str)
excel = |{}| {
	rows : List(Person)
	rows = rows_example
	text = "\u(FEFF)${Csv.encode(rows) ?? ""}"
	back : Try(List(Person), _)
	back = Csv.decode(text)
	[described(back)]
}

## `Toml.err_to_str` of a refused document.
message : Try(Toml.Value, Toml.Err) -> Str
message = |parsed|
	match parsed {
		Err(problem) => Toml.err_to_str(problem)
		Ok(_) => "parsed"
	}

reading : {} -> List(Str)
reading = |{}| {
	config = Toml.parse("title = \"demo\"\n\n[server]\nport = 8080\n")
	shown_in_readme : Toml.Value
	shown_in_readme = Table([("title", String("demo")), ("server", Table([("port", Integer(8080))]))])
	[
		Str.inspect(config == Ok(shown_in_readme)),
		Str.inspect(Toml.parse("a = 1\na = 2")),
		message(Toml.parse("a = 1\na = 2")),
		Str.inspect(Toml.parse("t = 23:59:60")),
	]
}

Config : {
	name : Str,
	released : Toml.LocalDate,
	tags : List(Str),
	limits : Dict(Str, U32),
	server : { host : Str, port : U16 },
}

empty_config : Config
empty_config = { name: "", released: { year: 0, month: 1, day: 1 }, tags: [], limits: Dict.empty(), server: { host: "", port: 0 } }

## Where a typed decode stopped.
stopped_at : Try({}, [Parse(Toml.Err), Mismatch({ path : List(Toml.Segment), expected : Str }), MissingRequiredField(Str)]) -> Str
stopped_at = |decoded|
	match decoded {
		Ok({}) => "decoded"
		Err(Mismatch(problem)) => "Mismatch at ${Str.inspect(problem.path)}"
		Err(problem) => Str.inspect(problem)
	}

typed_values : {} -> List(Str)
typed_values = |{}| {
	config : Try(Config, _)
	config = Toml.decode("name = \"demo\"\nreleased = 2026-03-08\ntags = [\"a\", \"b\"]\n\n[server]\nhost = \"localhost\"\nport = 8080\n\n[limits]\nrequests = 100\n")

	wrong : Try({ server : { port : U16 } }, _)
	wrong = Toml.decode("[server]\nport = \"8080\"")

	missing : Try({ server : { host : Str, port : U16 } }, _)
	missing = Toml.decode("[server]\nhost = \"localhost\"")

	exact : Try({ x : F64 }, _)
	exact = Toml.decode("x = 9007199254740993")

	[
		Str.inspect(Toml.encode(config ?? empty_config)),
		stopped_at(wrong.map_ok(|_| {})),
		stopped_at(missing.map_ok(|_| {})),
		stopped_at(exact.map_ok(|_| {})),
	]
}

writing : {} -> List(Str)
writing = |{}| {
	value : Toml.Value
	value = Table([
		("name", String("demo")),
		("ratio", Float(Toml.float_from_f64(1.5))),
		("server", Table([("port", Integer(8080))])),
	])

	alarm : Toml.Value
	alarm = Table([("at", LocalTime({ hour: 7, minute: 30, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 })), ("bell", String("\u(7)"))])

	[
		Str.inspect(Toml.to_str(value)),
		Str.inspect(Toml.to_str(alarm)),
		Str.inspect(Toml.to_str_with(alarm, { version: V1_1 })),
	]
}

floats_and_dec : {} -> List(Str)
floats_and_dec = |{}| {
	price : Try({ price : Dec }, _)
	price = Toml.decode("price = 12345678.123456789012345678")

	rough : Try({ price : F64 }, _)
	rough = Toml.decode("price = 12345678.123456789012345678")

	cut : Try({ x : Dec, y : Dec }, _)
	cut = Toml.decode("x = 0.9999999999999999999\ny = 1e-30")

	huge : Try({ x : F64 }, _)
	huge = Toml.decode("x = 1e400")

	[
		Str.inspect(price),
		Str.inspect(rough),
		Str.inspect(Toml.encode({ price: 12345678.123456789012345678.Dec })),
		Str.inspect(cut),
		Str.inspect(huge),
		Str.inspect(Toml.float_from_dec(0.1).to_dec()),
		Str.inspect(Toml.float_from_f64(F64.nan).to_dec()),
	]
}

editing : Str -> Try(List(Str), Toml.Err)
editing = |original| {
	doc = Toml.parse_document(original)?

	added = doc.set(Toml.path(["deps", "trantor-encoding"]), Table([("path", String("../trantor-encoding"))])) ?? doc

	header : Toml.Edit
	header = { table: Header }
	sectioned = doc.set_with(Toml.path(["deps", "trantor-encoding"]), Table([("path", String("../trantor-encoding"))]), header) ?? doc

	removed = added.remove(Toml.path(["deps", "trantor-cli"])) ?? doc

	tags = Toml.parse_document("tags = [\"a\", \"b\"]  # sorted\n")?
	appended = tags.append(Toml.path(["tags"]), String("c")) ?? tags

	dotted = Toml.parse_document("fruit.apple = 1\n")?
	no_apple = dotted.remove(Toml.path(["fruit", "apple"])) ?? dotted

	headed = Toml.parse_document("[fruit]\napple = 1\n")?
	empty_fruit = headed.remove(Toml.path(["fruit", "apple"])) ?? headed

	Ok([
		Str.inspect(doc.to_str() == original),
		Str.inspect(added.to_str()),
		Str.inspect(sectioned.to_str()),
		Str.inspect(removed.to_str()),
		Str.inspect(added.get([Key("deps"), Key("trantor-cli"), Key("path")]) == Ok(String("../trantor-cli"))),
		Str.inspect(doc.get(Toml.path(["deps", "nope"])).map_ok(|_| {})),
		Str.inspect(appended.to_str()),
		Str.inspect(no_apple.get(Toml.path(["fruit"])).map_ok(|_| {})),
		Str.inspect(empty_fruit.get(Toml.path(["fruit"])) == Ok(Table([]))),
	])
}

## A section's name and its results, one per line.
section : Str, List(Str) -> List(Str)
section = |name, results| ["## ${name}"].concat(results.map(|result| result.replace_each("\n", "\\n").replace_each("\t", "\\t")))

main! : List(OsStr) => Try({}, _)
main! = |_args| {
	edits = editing("# what this world builds on\n[deps]\ntrantor-cli = { path = \"../trantor-cli\" }  # the baseline\n") ?? ["an editing example does not parse"]
	sections = [
		section("Base64 and Hex", base64_and_hex({})),
		section("Rows and tables", rows_and_tables({})),
		section("Typed records", typed_records({})),
		section("Cell forms", cell_forms({})),
		section("Dates in cells", dates_in_cells({})),
		section("Excel", excel({})),
		section("Reading", reading({})),
		section("Typed values", typed_values({})),
		section("Writing", writing({})),
		section("Floats and Dec", floats_and_dec({})),
		section("Editing", edits),
	]
	Stdout.line!(Str.join_with(sections.join(), "\n"))
}
