import EncodingText

## The scanner: text to rows of fields in one left-to-right pass over bytes.
##
## Every byte the grammar looks at (delimiter, quote, CR, LF) is ASCII, and in
## UTF-8 an ASCII byte never occurs inside a multi-byte sequence, so a byte scan
## cannot split a code point. Errors carry a byte index while scanning and are
## turned into a line and column once, on the way out.
CsvParse :: [].{

	## What `Csv.parse` and friends fail with (`Csv.Err`).
	Err : [
		Syntax({ line : U64, column : U64, expected : Str }),
		RaggedRow({ line : U64, width : U64, found : U64 }),
		MissingHeader,
		DuplicateHeader({ line : U64, column : U64, name : Str }),
	]

	## The dialect fields scanning reads.
	Syntax : {
		delimiter : U8,
		quote : U8,
		comment : [NoComment, CommentPrefix(U8)],
		trim : Bool,
		skip_blank_lines : Bool,
		ragged : [Reject, Allow],
	}

	## Every record, as fields.
	rows : Str, Syntax -> Try(List(List(Str)), Err)
	rows = |text, syntax| {
		scan = { bytes: EncodingText.skip_bom(text.to_utf8()), syntax }
		document(scan, 0, []).map_err(|problem| located(scan.bytes, problem))
	}

	## The first record as column names, and the records after it. The names
	## are checked before any record is read, so a repeated name is reported
	## ahead of a problem further down.
	with_header : Str, Syntax -> Try({ header : List(Str), records : List(List(Str)) }, Err)
	with_header = |text, syntax| {
		scan = { bytes: EncodingText.skip_bom(text.to_utf8()), syntax }
		start = first_record_start(scan, 0)
		if start >= scan.bytes.len() {
			Err(MissingHeader)
		} else {
			header = fields(scan, start, [], []).map_err(|problem| located(scan.bytes, problem))?
			match CsvParse.first_repeat(header.values) {
				Ok(index) => Err(duplicate_header(scan, header, index))
				Err(NotFound) => {
					all = document(scan, header.next, [header.values]).map_err(|problem| located(scan.bytes, problem))?
					Ok({ header: header.values, records: all.drop_first(1) })
				}
			}
		}
	}

	## The index of the first name equal to an earlier one.
	first_repeat : List(Str) -> Try(U64, [NotFound])
	first_repeat = |names| repeat_from(names, 1)

	## Whether the byte at `index` ends its line: LF, CRLF (at the LF) and a
	## lone CR.
	ends_line : List(U8), U64 -> Bool
	ends_line = |bytes, index|
		match bytes.get(index) {
			Ok('\n') => True
			Ok('\r') => bytes.get(index + 1) != Ok('\n')
			_ => False
		}
}

Scan : { bytes : List(U8), syntax : CsvParse.Syntax }

## A scan failure located by byte index.
Problem : [
	Unterminated(U64),
	AfterClosingQuote(U64),
	QuoteInUnquoted(U64),
	Ragged({ at : U64, width : U64, found : U64 }),
]

## A field and what ended it: the record loop's next move differs for each.
Field : { value : Str, next : U64, term : [Delimiter, LineEnd, End] }

expected_closing_quote : Str
expected_closing_quote = "a closing quote for the field opened here"

expected_after_quote : Str
expected_after_quote = "a delimiter or line break after the closing quote (a literal quote is doubled)"

expected_quoted_field : Str
expected_quoted_field = "the field quoted, with its quotes doubled"

located : List(U8), Problem -> CsvParse.Err
located = |bytes, problem| {
	at = |index| EncodingText.position_at(bytes, index, CsvParse.ends_line)
	match problem {
		Unterminated(index) => EncodingText.syntax(at(index), expected_closing_quote)
		AfterClosingQuote(index) => EncodingText.syntax(at(index), expected_after_quote)
		QuoteInUnquoted(index) => EncodingText.syntax(at(index), expected_quoted_field)
		Ragged(ragged) => RaggedRow({ line: at(ragged.at).line, width: ragged.width, found: ragged.found })
	}
}

## The records from `index` on. Plain parameters rather than a folded record:
## a list held in a rebuilt record stops being uniquely owned and every append
## copies (measured in the playground: 4.8 s against 0.15 s on 20,000 rows).
document : Scan, U64, List(List(Str)) -> Try(List(List(Str)), Problem)
document = |scan, index, records|
	if index >= scan.bytes.len() {
		Ok(records)
	} else if is_skipped_line(scan, index) {
		document(scan, past_line(scan.bytes, index), records)
	} else {
		record = fields(scan, index, [], [])?
		check_width(scan.syntax, records, record.values, index)?
		document(scan, record.next, records.append(record.values))
	}

is_skipped_line : Scan, U64 -> Bool
is_skipped_line = |scan, index| {
	byte = scan.bytes.get(index)
	is_comment =
		match scan.syntax.comment {
			NoComment => False
			CommentPrefix(prefix) => byte == Ok(prefix)
		}
	is_comment or (scan.syntax.skip_blank_lines and is_blank_line(scan, index))
}

## Nothing before the line break or the end; under `trim`, nothing but spaces
## and tabs.
is_blank_line : Scan, U64 -> Bool
is_blank_line = |scan, index| {
	content = if scan.syntax.trim { skip_space(scan, index) } else { index }
	match scan.bytes.get(content) {
		Ok('\n') | Ok('\r') | Err(_) => True
		Ok(_) => False
	}
}

## Widths are measured against the first record, so one drifted record is one
## error.
check_width : CsvParse.Syntax, List(List(Str)), List(Str), U64 -> Try({}, Problem)
check_width = |syntax, records, values, at|
	match (syntax.ragged, records.first()) {
		(Reject, Ok(first)) if first.len() != values.len() => Err(Ragged({ at, width: first.len(), found: values.len() }))
		_ => Ok({})
	}

## One record's fields, and where each began.
fields : Scan, U64, List(Str), List(U64) -> Try({ values : List(Str), starts : List(U64), next : U64 }, Problem)
fields = |scan, index, values, starts| {
	start = if scan.syntax.trim { skip_space(scan, index) } else { index }
	field = if scan.bytes.get(start) == Ok(scan.syntax.quote) {
		quoted(scan, start, start + 1, [])?
	} else {
		unquoted(scan, start, start)?
	}
	more_values = values.append(field.value)
	more_starts = starts.append(start)
	match field.term {
		Delimiter => fields(scan, field.next, more_values, more_starts)
		LineEnd | End => Ok({ values: more_values, starts: more_starts, next: field.next })
	}
}

unquoted : Scan, U64, U64 -> Try(Field, Problem)
unquoted = |scan, start, index|
	match scan.bytes.get(index) {
		Err(_) => Ok({ value: text_between(scan, start, index), next: index, term: End })
		Ok(byte) if byte == scan.syntax.delimiter => Ok({ value: text_between(scan, start, index), next: index + 1, term: Delimiter })
		Ok('\n') | Ok('\r') => Ok({ value: text_between(scan, start, index), next: past_line_break(scan.bytes, index), term: LineEnd })
		Ok(byte) if byte == scan.syntax.quote => Err(QuoteInUnquoted(index))
		Ok(_) => unquoted(scan, start, index + 1)
	}

## A quoted field in segments between doubled quotes; a field without one is a
## single slice.
quoted : Scan, U64, U64, List(U8) -> Try(Field, Problem)
quoted = |scan, open, index, body|
	match find_byte(scan.bytes, index, scan.syntax.quote) {
		Err(NotFound) => Err(Unterminated(open))
		Ok(close) => {
			more = body.concat(scan.bytes.sublist({ start: index, len: close - index }))
			if scan.bytes.get(close + 1) == Ok(scan.syntax.quote) {
				quoted(scan, open, close + 2, more.append(scan.syntax.quote))
			} else {
				after_quote(scan, close + 1, Str.from_utf8_lossy(more))
			}
		}
	}

## After a closing quote: a delimiter, a line break or the end, with spaces
## first under `trim`.
after_quote : Scan, U64, Str -> Try(Field, Problem)
after_quote = |scan, from, value| {
	index = if scan.syntax.trim { skip_space(scan, from) } else { from }
	match scan.bytes.get(index) {
		Err(_) => Ok({ value, next: index, term: End })
		Ok(byte) if byte == scan.syntax.delimiter => Ok({ value, next: index + 1, term: Delimiter })
		Ok('\n') | Ok('\r') => Ok({ value, next: past_line_break(scan.bytes, index), term: LineEnd })
		Ok(_) => Err(AfterClosingQuote(index))
	}
}

past_line_break : List(U8), U64 -> U64
past_line_break = |bytes, index|
	if bytes.get(index) == Ok('\r') and bytes.get(index + 1) == Ok('\n') {
		index + 2
	} else {
		index + 1
	}

past_line : List(U8), U64 -> U64
past_line = |bytes, index|
	match bytes.get(index) {
		Err(_) => index
		Ok('\n') | Ok('\r') => past_line_break(bytes, index)
		Ok(_) => past_line(bytes, index + 1)
	}

## Spaces and tabs, never the delimiter: a tab separates TSV fields.
skip_space : Scan, U64 -> U64
skip_space = |scan, index|
	match scan.bytes.get(index) {
		Ok(byte) if byte != scan.syntax.delimiter and (byte == ' ' or byte == '\t') => skip_space(scan, index + 1)
		_ => index
	}

find_byte : List(U8), U64, U8 -> Try(U64, [NotFound])
find_byte = |bytes, index, wanted|
	match bytes.get(index) {
		Err(_) => Err(NotFound)
		Ok(byte) if byte == wanted => Ok(index)
		Ok(_) => find_byte(bytes, index + 1, wanted)
	}

## An unquoted field's text, without trailing spaces and tabs under `trim`
## (leading ones were already skipped). Other white space is data, as it is
## around a quoted field.
text_between : Scan, U64, U64 -> Str
text_between = |scan, from, to| {
	end = if scan.syntax.trim { before_space(scan.bytes, from, to) } else { to }
	Str.from_utf8_lossy(scan.bytes.sublist({ start: from, len: end - from }))
}

## Where trailing spaces and tabs before `to` begin, no earlier than `from`.
before_space : List(U8), U64, U64 -> U64
before_space = |bytes, from, to|
	if to > from and (bytes.get(to - 1) == Ok(' ') or bytes.get(to - 1) == Ok('\t')) {
		before_space(bytes, from, to - 1)
	} else {
		to
	}

repeat_from : List(Str), U64 -> Try(U64, [NotFound])
repeat_from = |names, index|
	match names.get(index) {
		Err(_) => Err(NotFound)
		Ok(name) if names.take_first(index).contains(name) => Ok(index)
		Ok(_) => repeat_from(names, index + 1)
	}

## `DuplicateHeader` at the repeated name, where its field starts.
duplicate_header : Scan, { values : List(Str), starts : List(U64), next : U64 }, U64 -> CsvParse.Err
duplicate_header = |scan, header, index| {
	position = EncodingText.position_at(scan.bytes, header.starts.get(index) ?? 0, CsvParse.ends_line)
	DuplicateHeader({ line: position.line, column: position.column, name: header.values.get(index) ?? "" })
}

first_record_start : Scan, U64 -> U64
first_record_start = |scan, index|
	if index < scan.bytes.len() and is_skipped_line(scan, index) {
		first_record_start(scan, past_line(scan.bytes, index))
	} else {
		index
	}
