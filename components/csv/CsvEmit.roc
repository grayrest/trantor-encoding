## Rows back to text. Anything written here, `CsvParse` reads back as the same
## rows, which is why quoting is eager: an unneeded quote costs a byte, a
## missing one corrupts the file.
CsvEmit :: [].{

	## The dialect fields writing reads.
	Syntax : {
		delimiter : U8,
		quote : U8,
		comment : [NoComment, CommentPrefix(U8)],
		newline : [Lf, Crlf],
	}

	## Rows joined by the dialect's newline, with no final line break.
	rows : List(List(Str)), Syntax -> Str
	rows = |records, syntax|
		Str.join_with(records.map(|record| row(record, syntax)), newline(syntax))

	## One field, quoted when it holds the delimiter, a quote, CR or LF, has
	## leading or trailing whitespace, or (as a record's first field) starts
	## with the comment prefix or U+FEFF, which reading would skip as a
	## byte-order mark at the start of the text.
	field : Str, Syntax, [First, Later] -> Str
	field = |value, syntax, place| {
		quote = byte_text(syntax.quote)
		if needs_quoting(value, syntax, place) {
			"${quote}${value.replace_each(quote, "${quote}${quote}")}${quote}"
		} else {
			value
		}
	}
}

## A lone empty field is quoted: unquoted it is a blank line, which reading
## skips or, at the end of the text, never sees.
row : List(Str), CsvEmit.Syntax -> Str
row = |values, syntax|
	if values == [""] {
		quote = byte_text(syntax.quote)
		"${quote}${quote}"
	} else {
		fields = values.map_with_index(|value, index| CsvEmit.field(value, syntax, if index == 0 { First } else { Later }))
		Str.join_with(fields, byte_text(syntax.delimiter))
	}

newline : CsvEmit.Syntax -> Str
newline = |syntax|
	match syntax.newline {
		Lf => "\n"
		Crlf => "\r\n"
	}

needs_quoting : Str, CsvEmit.Syntax, [First, Later] -> Bool
needs_quoting = |value, syntax, place| {
	structural = [syntax.delimiter, syntax.quote, '\n', '\r']
	is_structural = value.to_utf8().any(|byte| structural.contains(byte))
	is_padded = value != value.trim()
	is_structural or is_padded or (place == First and (starts_comment(value, syntax) or value.starts_with(byte_order_mark)))
}

byte_order_mark : Str
byte_order_mark = "\u(FEFF)"

starts_comment : Str, CsvEmit.Syntax -> Bool
starts_comment = |value, syntax|
	match syntax.comment {
		NoComment => False
		CommentPrefix(prefix) => value.to_utf8().first() == Ok(prefix)
	}

byte_text : U8 -> Str
byte_text = |byte| Str.from_utf8_lossy([byte])

csv : CsvEmit.Syntax
csv = { delimiter: ',', quote: '"', comment: NoComment, newline: Lf }

tsv : CsvEmit.Syntax
tsv = { ..csv, delimiter: '\t' }

expect CsvEmit.rows([["a", "b"], ["c", "d"]], csv) == "a,b\nc,d"

# The delimiter, a quote, a line break and padding each force quoting; quotes double.
expect CsvEmit.field("a,b", csv, Later) == "\"a,b\""
expect CsvEmit.field("say \"hi\"", csv, Later) == "\"say \"\"hi\"\"\""
expect CsvEmit.field("two\nlines", csv, Later) == "\"two\nlines\""
expect CsvEmit.field(" padded ", csv, Later) == "\" padded \""
expect CsvEmit.field("plain", csv, Later) == "plain"

# A comma is ordinary text in a TSV field.
expect CsvEmit.field("a,b", tsv, Later) == "a,b"
expect CsvEmit.field("a\tb", tsv, Later) == "\"a\tb\""

# Only a record's first field can be mistaken for a comment line.
expect {
	commented = { ..csv, comment: CommentPrefix('#') }
	CsvEmit.field("#1", commented, First) == "\"#1\"" and CsvEmit.field("#1", commented, Later) == "#1"
}

expect CsvEmit.rows([["a"], ["b"]], { ..csv, newline: Crlf }) == "a\r\nb"

expect CsvEmit.rows([["a"], [""]], csv) == "a\n\"\""
expect CsvEmit.rows([["", ""]], csv) == ","
expect CsvEmit.field("\u(FEFF)x", csv, First) == "\"\u(FEFF)x\"" and CsvEmit.field("\u(FEFF)x", csv, Later) == "\u(FEFF)x"
