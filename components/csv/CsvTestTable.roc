import Csv

## Tables and rows: the header record, lookups by name, duplicate names and
## error messages.
CsvTestTable :: [].{}

## A table rendered header first, fields joined by `|` and records by `/`.
table_shape : Str -> Str
table_shape = |text|
	match Csv.table(text) {
		Err(MissingHeader) => "ERR MissingHeader"
		Err(RaggedRow(ragged)) => "ERR RaggedRow ${ragged.line.to_str()}"
		Err(_) => "ERR"
		Ok(table) =>
			Str.join_with([table.headers].concat(table.rows.map(Csv.Row.fields)).map(|fields| Str.join_with(fields, "|")), "/")
	}

duplicate_at : Str -> Try({ line : U64, column : U64, name : Str }, [Other])
duplicate_at = |text|
	match Csv.table(text) {
		Err(DuplicateHeader(duplicate)) => Ok(duplicate)
		_ => Err(Other)
	}

# Rows answer by name; a short record (under `ragged: Allow`) reads a column it does not reach as empty.
expect {
	row = Csv.Row.new(["name", "email"], ["ada", "ada@example.com"])
	Csv.Row.get(row, "email") == Ok("ada@example.com") and Csv.Row.get(row, "nope") == Err(NoSuchColumn)
}
expect Csv.Row.get(Csv.Row.new(["a", "b", "c"], ["1"]), "c") == Ok("")
expect Csv.Row.get(Csv.Row.new(["a", "a"], ["first", "second"]), "a") == Ok("first")
expect {
	row = Csv.Row.new(["a", "b"], ["1", "2"])
	Csv.Row.at(row, 1) == Ok("2") and Csv.Row.at(row, 2) == Err(OutOfBounds) and Csv.Row.len(row) == 2 and Csv.Row.has_column(row, "b") and !Csv.Row.has_column(row, "c")
}

# The header record names the columns and is not data.
expect table_shape("name,email\nada,ada@example.com") == "name|email/ada|ada@example.com"
expect table_shape("") == "ERR MissingHeader"
expect table_shape("\n# not a comment under csv\n") == "# not a comment under csv"
expect table_shape("a,b") == "a|b"
expect {
	match Csv.table("a,b\n") {
		Ok(table) => Csv.Table.is_empty(table) and table.headers == ["a", "b"]
		Err(_) => False
	}
}
expect {
	match Csv.table("name,email\nada,ada@example.com\ngrace,grace@example.com") {
		Err(_) => False
		Ok(table) =>
			Csv.Table.len(table) == 2 and Csv.Table.column(table, "name") == Ok(["ada", "grace"]) and Csv.Table.column(table, "nope") == Err(NoSuchColumn)
	}
}
expect {
	match Csv.table("name,email\nada,ada@example.com") {
		Err(_) => False
		Ok(table) =>
			match table.rows {
				[row] => Csv.Row.get(row, "email") == Ok("ada@example.com") and Csv.Row.get(row, "x") == Err(NoSuchColumn)
				_ => False
			}
	}
}
expect {
	match Csv.table("a,b\n1,2\n3,4") {
		Ok(table) => Csv.Table.column(table, "b") == Ok(["2", "4"])
		Err(_) => False
	}
}

# The width check applies against the header record.
expect Csv.table("a,b\n1") == Err(RaggedRow({ line: 2, width: 2, found: 1 }))
expect table_shape("a,b\n1,2\n3") == "ERR RaggedRow 3"

# A quoted header name is read like any other field.
expect {
	match Csv.table("\"first name\",b\nada,2") {
		Err(_) => False
		Ok(table) => Csv.Table.column(table, "first name") == Ok(["ada"])
	}
}

# Other dialects, and back to text header first.
expect {
	match Csv.table_with("a;b\n1;2", { ..Csv.Dialect.csv, delimiter: ';' }) {
		Ok(table) => Csv.Table.column(table, "b") == Ok(["2"])
		Err(_) => False
	}
}
expect {
	match Csv.table("name,email\nada,a@e.com") {
		Err(_) => False
		Ok(table) => Csv.Table.to_str(table, Csv.Dialect.csv) == "name,email\nada,a@e.com"
	}
}
expect Csv.table("a\n1") == Csv.table("a\n1")
# A header of one empty name is written `""` and read back as the header.
expect {
	match Csv.table("\"\"\nx") {
		Err(_) => False
		Ok(table) => Csv.table(Csv.Table.to_str(table, Csv.Dialect.csv)) == Ok(table)
	}
}
expect {
	match Csv.table("\"\"") {
		Err(_) => False
		Ok(table) => Csv.Table.to_str(table, Csv.Dialect.csv) == "\"\""
	}
}

# A repeated header name, byte-exact, at its second occurrence; one empty name is allowed, a second is not.
expect duplicate_at("a,b,a\n1,2,3") == Ok({ line: 1, column: 5, name: "a" })
expect duplicate_at("a,b,b,a") == Ok({ line: 1, column: 5, name: "b" })
expect duplicate_at("\n\nx,y,\"x\"") == Ok({ line: 3, column: 5, name: "x" })
expect duplicate_at("😀,b,😀") == Ok({ line: 1, column: 5, name: "😀" })
expect table_shape("a,A,é,e\u(301)") == "a|A|é|e\u(301)"
expect table_shape(",a") == "|a"
expect duplicate_at(",a,") == Ok({ line: 1, column: 4, name: "" })
# A repeated name is reported before any record after the header is read.
expect duplicate_at("a,a\n1,2\n3") == Ok({ line: 1, column: 3, name: "a" })
expect duplicate_at("a,a\n1,\"2") == Ok({ line: 1, column: 3, name: "a" })
expect {
	match Csv.table_with(" a , b,a", { ..Csv.Dialect.csv, trim: True }) {
		Err(DuplicateHeader(duplicate)) => duplicate == { line: 1, column: 8, name: "a" }
		_ => False
	}
}
expect Csv.parse("a,a") == Ok([["a", "a"]])

# Messages carry the position (message text is not promised).
expect Csv.err_to_str(RaggedRow({ line: 4, width: 5, found: 3 })) == "line 4: expected 5 fields, found 3"
expect Csv.err_to_str(Syntax({ line: 12, column: 30, expected: "a closing quote" })) == "line 12, column 30: expected a closing quote"
expect Csv.err_to_str(DuplicateHeader({ line: 1, column: 5, name: "a" })) == "line 1, column 5: the column name \"a\" is repeated"
expect Csv.err_to_str(MissingHeader) == "expected a header record, but the input has no records"
