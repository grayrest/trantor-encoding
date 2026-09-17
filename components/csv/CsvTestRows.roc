import Csv

## Rows: shape, line breaks, quoting, error positions, Unicode, dialects and
## round trips, ported from the playground's suite.
##
## Rows render as fields joined by `|` and records by `/`; an error renders as
## its tag and position, never its message text, which is not promised.
CsvTestRows :: [].{}

shape : Str -> Str
shape = |text| render(Csv.parse(text))

shape_with : Str, Csv.Dialect -> Str
shape_with = |text, dialect| render(Csv.parse_with(text, dialect))

render : Try(List(List(Str)), Csv.Err) -> Str
render = |result|
	match result {
		Ok(rows) => Str.join_with(rows.map(|fields| Str.join_with(fields, "|")), "/")
		Err(problem) => "ERR ${problem_tag(problem)}"
	}

problem_tag : Csv.Err -> Str
problem_tag = |problem|
	match problem {
		Syntax(at) => "Syntax ${at.line.to_str()}:${at.column.to_str()}"
		RaggedRow(ragged) => "RaggedRow ${ragged.line.to_str()} ${ragged.width.to_str()}/${ragged.found.to_str()}"
		MissingHeader => "MissingHeader"
		DuplicateHeader(duplicate) => "DuplicateHeader ${duplicate.line.to_str()}:${duplicate.column.to_str()} ${duplicate.name}"
	}

round_trip : List(List(Str)) -> Bool
round_trip = |rows| Csv.parse(Csv.to_str(rows)) == Ok(rows)

round_trip_with : List(List(Str)), Csv.Dialect -> Bool
round_trip_with = |rows, dialect| Csv.parse_with(Csv.to_str_with(rows, dialect), dialect) == Ok(rows)

commented : Csv.Dialect
commented = { ..Csv.Dialect.csv, comment: CommentPrefix('#') }

# Shape: a trailing line break ends the last record; empty fields are values.
expect Csv.parse("a,b\nc,d") == Ok([["a", "b"], ["c", "d"]])
expect shape("a,b,c") == "a|b|c"
expect shape("a,b\nc,d") == "a|b/c|d"
expect shape("a,b\n") == "a|b"
expect shape("a,b\r\n") == "a|b"
expect shape(",") == "|"
expect shape("a,,b") == "a||b"
expect shape(",a,") == "|a|"

# Empty input is no records, not one empty record.
expect shape("") == ""
expect shape("\n") == ""
expect shape("\n\n\n") == ""
expect Csv.parse("") == Ok([])

# LF, CRLF and a lone CR each end a record, whatever the dialect.
expect shape("a\nb") == "a/b"
expect shape("a\r\nb") == "a/b"
expect shape("a\rb") == "a/b"
expect shape("a\r\n\r\nb") == "a/b"

# Quoting: delimiters and line breaks inside, doubled quotes, literal content.
expect shape("\"a\",\"b\"") == "a|b"
expect shape("\"a,b\",c") == "a,b|c"
expect shape("\"two\nlines\",c") == "two\nlines|c"
expect shape("\"say \"\"hi\"\"\"") == "say \"hi\""
expect shape("\"\"\"\"") == "\""
expect shape("\"\",a") == "|a"
expect shape("\",\"") == ","
expect shape("\" a \"") == " a "
expect shape("\"x\r\ny\"") == "x\r\ny"

# Quote errors: an unterminated field at its opening quote, the others at the offending character.
expect shape("a,\"b") == "ERR Syntax 1:3"
expect shape("a,b\nc,\"d") == "ERR Syntax 2:3"
expect shape("a\n\"b,c\nd") == "ERR Syntax 2:1"
expect shape("\"a\"x") == "ERR Syntax 1:4"
expect shape("a,\"b\"  ,c") == "ERR Syntax 1:6"
expect shape("a\"b") == "ERR Syntax 1:2"
expect shape("a,b\r\nc\"d") == "ERR Syntax 2:2"
expect shape("a,b\rc\"d") == "ERR Syntax 2:2"

# Ragged records: the record's first line, the first record's width, the count found.
expect shape("a,b\nc") == "ERR RaggedRow 2 2/1"
expect shape("a,b\nc,d,e") == "ERR RaggedRow 2 2/3"
expect shape("a,b\nc\nd,e") == "ERR RaggedRow 2 2/1"
expect shape("a,b\n\"c\nd\"") == "ERR RaggedRow 2 2/1"
expect shape_with("a,b\n\n# x\nc,d\n\"e\nf\",g\nh", commented) == "ERR RaggedRow 7 2/1"
expect Csv.parse("a,b\nc") == Err(RaggedRow({ line: 2, width: 2, found: 1 }))

# Unicode: columns count code points, after two-, three- and four-byte characters.
expect shape("é,ü") == "é|ü"
expect shape("\"日本語,x\"") == "日本語,x"
expect shape("———\"a") == "ERR Syntax 1:4"
expect shape("———,\"a") == "ERR Syntax 1:5"
expect shape("😀é,x\"") == "ERR Syntax 1:5"
expect shape("a\r\n😀😀,\"x") == "ERR Syntax 2:4"

# One leading byte-order mark is skipped; column 1 is what follows it.
expect shape("\u(FEFF)a,b") == "a|b"
expect shape("\u(FEFF)\"a") == "ERR Syntax 1:1"
expect shape("\u(FEFF)\u(FEFF)a") == "\u(FEFF)a"

# Dialects: other delimiters, comments at a line's start only, blank lines, trimming, ragged records.
expect Csv.parse_with("a;b", { ..Csv.Dialect.csv, delimiter: ';' }) == Ok([["a", "b"]])
expect shape_with("a|b", { ..Csv.Dialect.csv, delimiter: '|' }) == "a|b"
expect shape_with("# note\na,b", commented) == "a|b"
expect shape_with("a,#b", commented) == "a|#b"
expect shape_with("a,b\n# note\nc,d", commented) == "a|b/c|d"
expect shape_with("a\n\nb", { ..Csv.Dialect.csv, skip_blank_lines: False }) == "a//b"
expect shape_with("a,b\n\nc,d", { ..Csv.Dialect.csv, skip_blank_lines: False }) == "ERR RaggedRow 2 2/1"
expect shape_with(" a , b ", { ..Csv.Dialect.csv, trim: True }) == "a|b"
expect shape_with(" a , b ", Csv.Dialect.csv) == " a | b "
expect shape_with("\" a \",b", { ..Csv.Dialect.csv, trim: True }) == " a |b"
expect shape_with(" \"a\" , b", { ..Csv.Dialect.csv, trim: True }) == "a|b"
# Under `trim` a line of spaces and tabs is blank.
expect Csv.parse_with("a,b\n   \nc,d", { ..Csv.Dialect.csv, trim: True }) == Ok([["a", "b"], ["c", "d"]])
expect Csv.parse_with("a,b\n \t\r\nc,d\n  ", { ..Csv.Dialect.csv, trim: True }) == Ok([["a", "b"], ["c", "d"]])
expect shape_with("a,b\n   \nc,d", Csv.Dialect.csv) == "ERR RaggedRow 2 2/1"
# `trim` strips spaces and tabs only, as around a quoted field; other white space is data.
expect Csv.parse_with("\u(00A0)a\u(00A0),b", { ..Csv.Dialect.csv, trim: True }) == Ok([["\u(00A0)a\u(00A0)", "b"]])
expect Csv.parse_with("\u(3000)a\u(3000), b\t", { ..Csv.Dialect.csv, trim: True }) == Ok([["\u(3000)a\u(3000)", "b"]])
expect shape_with("a,b\nc", { ..Csv.Dialect.csv, ragged: Allow }) == "a|b/c"
expect shape_with("a\nb,c,d", { ..Csv.Dialect.csv, ragged: Allow }) == "a/b|c|d"

# The tab preset: commas are data, quoting still works, empty fields stay.
expect Csv.Dialect.csv.delimiter == ','
expect Csv.Dialect.tsv.delimiter == '\t' and Csv.Dialect.tsv.quote == '"'
expect Csv.parse_with("a\tb\nc\td", Csv.Dialect.tsv) == Ok([["a", "b"], ["c", "d"]])
expect Csv.parse_with("a,b\tc", Csv.Dialect.tsv) == Ok([["a,b", "c"]])
expect Csv.parse_with("\"a\tb\"\tc", Csv.Dialect.tsv) == Ok([["a\tb", "c"]])
expect Csv.parse_with("a\t\tb", Csv.Dialect.tsv) == Ok([["a", "", "b"]])
expect Csv.to_str_with([["a,1", "b"]], Csv.Dialect.tsv) == "a,1\tb"
expect {
	dialect = { ..Csv.Dialect.csv, delimiter: ';' }
	dialect.delimiter == ';' and dialect.quote == '"'
}

# Writing rows: quote only what needs it (golden).
expect Csv.to_str([["a", "b"], ["c", "d"]]) == "a,b\nc,d"
expect Csv.to_str([]) == ""
expect Csv.to_str([[""]]) == "\"\""
expect Csv.to_str([["a"], [""], ["b"]]) == "a\n\"\"\nb"
expect Csv.to_str([["plain", "a,b"]]) == "plain,\"a,b\""
expect Csv.to_str([["say \"hi\""]]) == "\"say \"\"hi\"\"\""
expect Csv.to_str([["two\nlines"]]) == "\"two\nlines\""
expect Csv.to_str([["has\rcr"]]) == "\"has\rcr\""
expect Csv.to_str([[" pad "]]) == "\" pad \""
expect Csv.to_str_with([["a", "b"]], { ..Csv.Dialect.csv, newline: Crlf }) == "a,b"
expect Csv.to_str_with([["a"], ["b"]], { ..Csv.Dialect.csv, newline: Crlf }) == "a\r\nb"

# Round trips: what `to_str` writes, `parse` reads back as the same rows.
expect round_trip([["a", "b"], ["c", "d"]])
expect round_trip([["a,b", "c"]])
expect round_trip([["say \"hi\"", "x"]])
expect round_trip([["two\nlines", "x"]])
expect round_trip([["crlf\r\ninside", "x"]])
expect round_trip([[" leading", "trailing "]])
expect round_trip([["", ""], ["", ""]])
# A record of one empty field is written `""`, not as a blank line reading skips.
expect round_trip([["a"], [""], ["b"]])
expect round_trip([["a"], [""]])
expect round_trip([[""]])
# A leading U+FEFF is quoted, so reading does not take it for a byte-order mark.
expect round_trip([["\u(FEFF)x", "y"]])
expect round_trip([["a"], ["\u(FEFF)x"]])
expect Csv.to_str([["\u(FEFF)x", "\u(FEFF)y"]]) == "\"\u(FEFF)x\",\u(FEFF)y"
expect round_trip([["\"", "\"\""]])
expect round_trip([["日本語", "é"]])
expect round_trip([["a\tb", "c"]])
expect round_trip_with([["a\tb", "c,d"]], Csv.Dialect.tsv)
expect round_trip_with([["a;b", "c"]], { ..Csv.Dialect.csv, delimiter: ';' })
expect round_trip_with([["#hash", "x"], ["y", "#z"]], commented)
expect round_trip_with([["a", "b"], ["c", "d"]], { ..Csv.Dialect.csv, newline: Crlf })
expect round_trip_with([[" pad ", "x"]], { ..Csv.Dialect.csv, trim: True })
