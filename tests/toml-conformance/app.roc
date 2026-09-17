app [main!] { pf: platform "../target/trantor/app/platform/main.roc" }

import pf.OsStr exposing [OsStr]
import pf.Path
import pf.Stdout
import pf.Toml
import pf.Utc
import ExpectedJson
import StrictToml

## toml-test through `Toml.parse`: every file a list names, valid files
## compared with their expected JSON as `Toml.Value`s, invalid files required
## to fail. Every valid file is then written back in both modes and must parse
## to the same value, the default mode's text passing the strict 1.0 checker,
## which is itself held to the 1.0.0 lists. Every valid file also goes through
## `parse_document`, whose `to_str` must be the file's bytes and `to_value`
## `parse`'s value; every invalid file must be refused with `parse`'s error.
## Prints one `FAIL` line per failing file and a count per list.
##
## Also times a compare of two 10,000-key tables holding their keys in
## opposite orders, which a linear-lookup equality made take seconds.
main! : List(OsStr) => Try({}, _)
main! = |args| {
	corpus = args.get(1).map_ok(OsStr.display) ?? "corpus"
	run_list!(corpus, "1.1.0 valid", "files-toml-1.1.0", "valid/")?
	run_list!(corpus, "1.1.0 invalid", "files-toml-1.1.0", "invalid/")?
	run_list!(corpus, "1.0.0 valid", "files-toml-1.0.0", "valid/")?
	run_checks!(corpus, "1.1.0 valid round trip", "files-toml-1.1.0", "valid/", check_round_trip!)?
	run_checks!(corpus, "1.0.0 valid round trip", "files-toml-1.0.0", "valid/", check_round_trip!)?
	run_checks!(corpus, "1.1.0 valid document", "files-toml-1.1.0", "valid/", check_document!)?
	run_checks!(corpus, "1.0.0 valid document", "files-toml-1.0.0", "valid/", check_document!)?
	run_checks!(corpus, "1.1.0 invalid document", "files-toml-1.1.0", "invalid/", check_document_refuses!)?
	run_checks!(corpus, "strict 1.0 checker, 1.0.0 valid", "files-toml-1.0.0", "valid/", check_strict_accepts!)?
	run_checks!(corpus, "strict 1.0 checker, 1.0.0 invalid", "files-toml-1.0.0", "invalid/", check_strict_refuses!)?
	timed_compare!()
}

key_count : U64
key_count = 10000

compare_budget_ms : U128
compare_budget_ms = 2000

run_list! : Str, Str, Str, Str => Try({}, _)
run_list! = |corpus, label, list_file, prefix|
	if prefix == "valid/" {
		run_checks!(corpus, label, list_file, prefix, check_valid!)
	} else {
		run_checks!(corpus, label, list_file, prefix, check_invalid!)
	}

## `check!` on every `.toml` file under `prefix` that `list_file` names.
run_checks! : Str, Str, Str, Str, (Str, Str => Try({}, Str)) => Try({}, _)
run_checks! = |corpus, label, list_file, prefix, check!| {
	listing = Path.Utf8("${corpus}/${list_file}").read_utf8!()?
	names = listing.split_on("\n").keep_if(|name| name.starts_with(prefix) and name.ends_with(".toml"))
	failures = check_all!(corpus, names, check!)
	for failure in failures {
		Stdout.line!("FAIL ${label} ${failure}")?
	}
	Stdout.line!("${label}: ${(names.len() - failures.len()).to_str()} passed, ${failures.len().to_str()} failed")
}

check_all! : Str, List(Str), (Str, Str => Try({}, Str)) => List(Str)
check_all! = |corpus, names, check!| {
	var $failures = []
	for name in names {
		match check!(corpus, name) {
			Ok({}) => {}
			Err(reason) => {
				$failures = $failures.append("${name}: ${reason}")
			}
		}
	}
	$failures
}

## `parse(to_str_with(v, mode)) == v` in both modes; the 1.0 text strict.
check_round_trip! : Str, Str => Try({}, Str)
check_round_trip! = |corpus, name| {
	source = read_text!("${corpus}/${name}")?
	value = Toml.parse(source) ? |problem| "refused: ${Toml.err_to_str(problem)}"
	v1_0 = written_back(value, { version: V1_0 }) ? |problem| "1.0: ${problem}"
	StrictToml.check(v1_0) ? |problem| "1.0 output is not strict TOML 1.0: ${problem}"
	_ = written_back(value, { version: V1_1 }) ? |problem| "1.1: ${problem}"
	Ok({})
}

## The text `value` writes as, when it parses back to `value`.
written_back : Toml.Value, Toml.Write -> Try(Str, Str)
written_back = |value, write| {
	text = Toml.to_str_with(value, write) ? |_| "refused to write"
	match Toml.parse(text) {
		Ok(back) if back == value => Ok(text)
		Ok(_) => Err("reads back as a different value")
		Err(problem) => Err("does not parse: ${Toml.err_to_str(problem)}")
	}
}

## `parse_document` then `to_str` gives the file's bytes back, and `to_value`
## the value `parse` reads.
check_document! : Str, Str => Try({}, Str)
check_document! = |corpus, name| {
	source = read_text!("${corpus}/${name}")?
	document = Toml.parse_document(source) ? |problem| "refused: ${Toml.err_to_str(problem)}"
	value = Toml.parse(source) ? |problem| "parse refused: ${Toml.err_to_str(problem)}"
	if document.to_str() != source {
		Err("to_str differs from the source")
	} else if document.to_value() != value {
		Err("to_value differs from parse")
	} else {
		Ok({})
	}
}

## `parse_document` refuses what `parse` refuses, with the same error.
check_document_refuses! : Str, Str => Try({}, Str)
check_document_refuses! = |corpus, name| {
	bytes = Path.Utf8("${corpus}/${name}").read_bytes!() ? |_| "unreadable"
	match Str.from_utf8(bytes) {
		Err(_) => Ok({})
		Ok(source) =>
			match (Toml.parse_document(source), Toml.parse(source)) {
				(Err(from_document), Err(from_parse)) if from_document == from_parse => Ok({})
				(Err(_), Err(_)) => Err("a different error from parse's")
				_ => Err("accepted")
			}
	}
}

check_strict_accepts! : Str, Str => Try({}, Str)
check_strict_accepts! = |corpus, name| {
	source = read_text!("${corpus}/${name}")?
	StrictToml.check(source)
}

## Files that are not UTF-8 count as refused, as in `check_invalid!`.
check_strict_refuses! : Str, Str => Try({}, Str)
check_strict_refuses! = |corpus, name| {
	bytes = Path.Utf8("${corpus}/${name}").read_bytes!() ? |_| "unreadable"
	match Str.from_utf8(bytes) {
		Err(_) => Ok({})
		Ok(source) =>
			match StrictToml.check(source) {
				Err(_) => Ok({})
				Ok({}) => Err("accepted")
			}
	}
}

check_valid! : Str, Str => Try({}, Str)
check_valid! = |corpus, name| {
	source = read_text!("${corpus}/${name}")?
	expected_text = read_text!("${corpus}/${name.drop_suffix(".toml")}.json")?
	expected = ExpectedJson.document(expected_text) ? |problem| "expected JSON unreadable: ${problem}"
	match Toml.parse(source) {
		Err(problem) => Err("refused: ${Toml.err_to_str(problem)}")
		Ok(parsed) => if parsed == expected { Ok({}) } else { Err("value differs from the expected JSON") }
	}
}

## A file that is not UTF-8 cannot become a `Str`, so it never reaches
## `parse`; that counts as refused.
check_invalid! : Str, Str => Try({}, Str)
check_invalid! = |corpus, name| {
	bytes = Path.Utf8("${corpus}/${name}").read_bytes!() ? |_| "unreadable"
	match Str.from_utf8(bytes) {
		Err(_) => Ok({})
		Ok(source) =>
			match Toml.parse(source) {
				Err(_) => Ok({})
				Ok(_) => Err("accepted")
			}
	}
}

read_text! : Str => Try(Str, Str)
read_text! = |path| {
	bytes = Path.Utf8(path).read_bytes!() ? |_| "cannot read ${path}"
	Str.from_utf8(bytes).map_err(|_| "${path} is not UTF-8")
}

timed_compare! : () => Try({}, _)
timed_compare! = || {
	entries : List((Str, Toml.Value))
	entries = List.repeat({}, key_count).map_with_index(|_, index| ("key${index.to_str()}", Integer(U64.to_i64_wrap(index))))
	forward : Toml.Value
	forward = Table(entries)
	backward : Toml.Value
	backward = Table(entries.rev())
	start = Utc.now!()
	is_equal = forward == backward
	elapsed = Utc.delta_as_millis(Utc.now!(), start)
	verdict = if is_equal and elapsed <= compare_budget_ms { "ok" } else { "FAIL" }
	Stdout.line!("reversed ${key_count.to_str()}-key compare: ${verdict}, ${elapsed.to_str()} ms (budget ${compare_budget_ms.to_str()} ms)")
}
