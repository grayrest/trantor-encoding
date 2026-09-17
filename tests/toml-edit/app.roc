app [main!] { pf: platform "../target/trantor/app/platform/main.roc" }

import pf.OsStr exposing [OsStr]
import pf.Path
import pf.Stdout
import pf.Toml

## Edit snapshots: each case directory holds `before.toml`, an `edit` (itself
## TOML, see README.md) and `after.toml`. The edit applied to `before.toml`
## must give `after.toml` byte for byte, `after.toml` must parse, and reading
## the edited path back must give what was set or appended (`NotFound` after
## a removal; an array one element shorter after removing an index). A case whose `edit` names an `error` must fail with exactly
## that error and has no `after.toml`.
##
## Arguments: the cases directory, then the case names. Prints one `FAIL`
## line per failing case and a count.
main! : List(OsStr) => Try({}, _)
main! = |args| {
	strings = args.map(OsStr.display)
	cases = strings.get(1) ?? "cases"
	names = strings.drop_first(2)
	var $failed = 0
	for name in names {
		match check_case!(cases, name) {
			Ok({}) => {}
			Err(reason) => {
				$failed = $failed + 1
				Stdout.line!("FAIL ${name}: ${reason}")?
			}
		}
	}
	Stdout.line!("edit cases: ${(names.len() - $failed).to_str()} passed, ${$failed.to_str()} failed")
}

## A value typed as `trantor add` would hold a dependency, for the case that
## sets a typed record through `encode_value`.
Dependency : { path : Str, optional : Bool, features : List(Str) }

## What an `edit` file asks for.
Edit : { operation : Str, path : List(Toml.Segment), value : Try(Toml.Value, [Absent]), edit : Toml.Edit, error : Try(Str, [Absent]) }

check_case! : Str, Str => Try({}, Str)
check_case! = |cases, name| {
	dir = "${cases}/${name}"
	before = read_text!("${dir}/before.toml")?
	edit = parsed_edit(read_text!("${dir}/edit")?)?
	document = Toml.parse_document(before) ? |problem| "before.toml does not parse: ${Toml.err_to_str(problem)}"
	outcome = applied(document, edit)?
	match (outcome.result, edit.error) {
		(Err(problem), Ok(expected)) => if Str.inspect(problem) == expected { Ok({}) } else { Err("failed with ${Str.inspect(problem)}, expected ${expected}") }
		(Err(problem), Err(Absent)) => Err("failed with ${Str.inspect(problem)}")
		(Ok(_), Ok(expected)) => Err("succeeded, expected ${expected}")
		(Ok(edited), Err(Absent)) => {
			after = read_text!("${dir}/after.toml")?
			compared(edited.to_str(), after)?
			read_back = Toml.parse_document(after) ? |problem| "after.toml does not parse: ${Toml.err_to_str(problem)}"
			_ = Toml.parse(after) ? |problem| "after.toml does not parse: ${Toml.err_to_str(problem)}"
			looked_up(read_back, edit, outcome.expected)
		}
	}
}

## The edited document (or the error) and the value the edited path must
## then hold.
## What reading the edited path back must find.
Expected : [Holds(Toml.Value), Gone, Shortened(U64)]

applied : Toml.Document, Edit -> Try({ result : Try(Toml.Document, Toml.EditErr), expected : Expected }, Str)
applied = |document, edit|
	match (edit.operation, edit.value) {
		("remove", _) => Ok({ result: document.remove(edit.path), expected: removal_expected(document, edit.path) })
		(_, Err(Absent)) => Err("the edit has no value")
		("set", Ok(value)) => Ok({ result: document.set(edit.path, value), expected: Holds(value) })
		("set_with", Ok(value)) => Ok({ result: document.set_with(edit.path, value, edit.edit), expected: Holds(value) })
		("append", Ok(value)) => Ok({ result: document.append(edit.path, value), expected: Holds(value) })
		("set_typed", Ok(value)) => {
			typed = typed_record(value)?
			encoded = Toml.encode_value(typed) ? |problem| "the record does not encode: ${Str.inspect(problem)}"
			Ok({ result: document.set(edit.path, encoded), expected: Holds(encoded) })
		}
		(other, _) => Err("unknown op ${other}")
	}

typed_record : Toml.Value -> Try(Dependency, Str)
typed_record = |value| {
	decoded : Try(Dependency, [Mismatch({ path : List(Toml.Segment), expected : Str }), MissingRequiredField(Str)])
	decoded = Toml.decode_value(value)
	decoded.map_err(|problem| "the value is not a dependency: ${Str.inspect(problem)}")
}

## After removing an element by index the array is one shorter; after
## removing anything else the path is gone.
removal_expected : Toml.Document, List(Toml.Segment) -> Expected
removal_expected = |document, path|
	match (path.last(), document.get(path.drop_last(1))) {
		(Ok(Index(_)), Ok(Array(items))) => Shortened(items.len() - 1)
		_ => Gone
	}

## Reading the path back: the value set, the last element appended, nothing
## after a removal, or a shorter array.
looked_up : Toml.Document, Edit, Expected -> Try({}, Str)
looked_up = |document, edit, expected|
	match expected {
		Holds(value) =>
			match document.get(looked_up_path(document, edit)) {
				Ok(found) if found == value => Ok({})
				Ok(_) => Err("get gives a different value")
				Err(problem) => Err("get fails with ${Str.inspect(problem)}")
			}
		Gone =>
			match document.get(edit.path) {
				Err(NotFound(_)) => Ok({})
				Ok(_) => Err("get still finds the removed path")
				Err(problem) => Err("get fails with ${Str.inspect(problem)}")
			}
		Shortened(count) =>
			match document.get(edit.path.drop_last(1)) {
				Ok(Array(items)) if items.len() == count => Ok({})
				Err(NotFound(_)) if count == 0 => Ok({})
				_ => Err("the array is not one element shorter")
			}
	}

## The path set, or the last element's after an append.
looked_up_path : Toml.Document, Edit -> List(Toml.Segment)
looked_up_path = |document, edit|
	match (edit.operation, document.get(edit.path)) {
		("append", Ok(Array(items))) if !items.is_empty() => edit.path.append(Index(items.len() - 1))
		_ => edit.path
	}

compared : Str, Str -> Try({}, Str)
compared = |got, expected|
	if got == expected {
		Ok({})
	} else {
		Err("output differs from after.toml:\n--- got\n${got}--- expected\n${expected}---")
	}

## An `edit` file: `op`, `path` (strings are keys, integers indices), and as
## the op needs them `value`, `version`, `table` and `error`.
parsed_edit : Str -> Try(Edit, Str)
parsed_edit = |text| {
	table = Toml.parse(text) ? |problem| "edit does not parse: ${Toml.err_to_str(problem)}"
	operation = string_field(table, "op")?
	path = path_of(field(table, "path") ? |_| "edit has no path")?
	version = optional_string(table, "version")
	style = optional_string(table, "table")
	edit_options = options(version, style)?
	Ok({ operation, path, value: field(table, "value"), edit: edit_options, error: optional_string(table, "error") })
}

options : Try(Str, [Absent]), Try(Str, [Absent]) -> Try(Toml.Edit, Str)
options = |version, style| {
	chosen_version = version_named(version ?? "V1_0")?
	chosen_style = style_named(style ?? "Auto")?
	edit : Toml.Edit
	edit = { version: chosen_version, table: chosen_style }
	Ok(edit)
}

version_named : Str -> Try([V1_0, V1_1], Str)
version_named = |name|
	match name {
		"V1_0" => Ok(V1_0)
		"V1_1" => Ok(V1_1)
		other => Err("unknown version ${other}")
	}

style_named : Str -> Try([Auto, Inline, Header, Dotted], Str)
style_named = |name|
	match name {
		"Auto" => Ok(Auto)
		"Inline" => Ok(Inline)
		"Header" => Ok(Header)
		"Dotted" => Ok(Dotted)
		other => Err("unknown table style ${other}")
	}

path_of : Toml.Value -> Try(List(Toml.Segment), Str)
path_of = |value|
	match value {
		Array(items) => items.map_try(segment_of)
		_ => Err("path is not an array")
	}

segment_of : Toml.Value -> Try(Toml.Segment, Str)
segment_of = |value|
	match value {
		String(name) => Ok(Key(name))
		Integer(index) => index.to_u64_try().map_ok(|position| Index(position)).map_err(|_| "a negative index")
		_ => Err("a path segment is neither a string nor an integer")
	}

field : Toml.Value, Str -> Try(Toml.Value, [Absent])
field = |table, name|
	match table {
		Table(entries) => entries.find_first(|(key, _)| key == name).map_ok(|(_, value)| value).map_err(|_| Absent)
		_ => Err(Absent)
	}

string_field : Toml.Value, Str -> Try(Str, Str)
string_field = |table, name|
	match field(table, name) {
		Ok(String(text)) => Ok(text)
		_ => Err("edit has no string ${name}")
	}

optional_string : Toml.Value, Str -> Try(Str, [Absent])
optional_string = |table, name|
	match field(table, name) {
		Ok(String(text)) => Ok(text)
		_ => Err(Absent)
	}

read_text! : Str => Try(Str, Str)
read_text! = |path| {
	bytes = Path.Utf8(path).read_bytes!() ? |_| "cannot read ${path}"
	Str.from_utf8(bytes).map_err(|_| "${path} is not UTF-8")
}
