import CsvCell
import CsvDate
import CsvEmit
import CsvParse
import EncodingDate
import EncodingPath
import EncodingText

## CSV and every delimiter-separated dialect shaped like it: RFC 4180 quoting,
## LF, CRLF and lone-CR line breaks, a leading byte-order mark skipped.
##
## Three layers, each with a `_with` form taking a `Dialect`:
##
## - `parse`/`to_str`: text to rows of fields and back;
## - `table`: the first record as column names, rows readable by name;
## - `decode`/`encode`: records typed by the caller, cells in XML Schema 1.1's
##   lexical forms (`CsvCell`, and `CsvDate` for the date methods).
##
## ```roc
## people : Try(List({ name : Str, age : U32 }), _)
## people = Csv.decode("name,age\nada,36")
## Csv.encode([{ name: "ada", age: 36.U32 }]) == Ok("age,name\n36,ada")
## ```
##
## Lines are 1-based and columns count code points. Reading stops at the first
## error. Encoded output is byte-stable within a major version; error message
## text is not.
Csv :: [].{

	Segment : EncodingPath.Segment

	## `Syntax` (quote errors, at the opening quote of an unterminated field and
	## at the offending character otherwise), `RaggedRow` (`line` the record's
	## first line, `width` the first record's field count), `MissingHeader` and
	## `DuplicateHeader` (at the second occurrence).
	Err : CsvParse.Err

	## Writing failures, each at `[Index(record), Key(column)]`:
	## `InvalidDate` (not a day on the ISO calendar), `InvalidTime` (hour 24,
	## second 60, a sub-second field over 999) and `InvalidOffset` (beyond
	## ±14:00).
	EncodeErr : CsvDate.EncodeErr

	## What separates fields and how they are quoted. `csv` and `tsv` are the
	## presets; anything else is one of them with fields replaced:
	## `{ ..Csv.Dialect.csv, delimiter: ';' }`.
	Dialect := {
		delimiter : U8,
		quote : U8,
		## Lines whose first byte is this are dropped whole.
		comment : [NoComment, CommentPrefix(U8)],
		## Strip spaces and tabs around unquoted fields and around quotes.
		trim : Bool,
		## Drop empty lines; under `trim`, lines of only spaces and tabs too.
		skip_blank_lines : Bool,
		## Whether records may differ in width from the first.
		ragged : [Reject, Allow],
		## Writing only: reading accepts LF, CRLF and lone CR.
		newline : [Lf, Crlf],
	}.{
		## RFC 4180: comma-delimited, double-quoted, LF written.
		csv : Dialect
		csv = {
			delimiter: ',',
			quote: '"',
			comment: NoComment,
			trim: False,
			skip_blank_lines: True,
			ragged: Reject,
			newline: Lf,
		}

		## Tab-delimited, quoted as CSV is: files in the wild quote that way,
		## and a file that never quotes reads the same either way.
		tsv : Dialect
		tsv = { ..Dialect.csv, delimiter: '\t' }
	}

	## A data record with the column names it is read against.
	Row := { headers : List(Str), fields : List(Str) }.{
		new : List(Str), List(Str) -> Row
		new = |headers, fields| { headers, fields }

		## The field under `name`; `""` for a column a short record (only under
		## `ragged: Allow`) does not reach. The first column of that name wins.
		get : Row, Str -> Try(Str, [NoSuchColumn])
		get = |row, name|
			match row.headers.find_first_index(|header| header == name) {
				Err(_) => Err(NoSuchColumn)
				Ok(index) => Ok(row.fields.get(index) ?? "")
			}

		at : Row, U64 -> Try(Str, [OutOfBounds])
		at = |row, index| row.fields.get(index).map_err(|_| OutOfBounds)

		fields : Row -> List(Str)
		fields = |row| row.fields

		len : Row -> U64
		len = |row| row.fields.len()

		has_column : Row, Str -> Bool
		has_column = |row, name| row.headers.contains(name)

		is_eq : Row, Row -> Bool
		is_eq = |left, right| left.headers == right.headers and left.fields == right.fields
	}

	## A header record and the data records under it.
	Table := { headers : List(Str), rows : List(Row) }.{

		## Every value under `name`, top to bottom.
		column : Table, Str -> Try(List(Str), [NoSuchColumn])
		column = |table, name|
			if table.headers.contains(name) {
				Ok(table.rows.map(|row| Row.get(row, name) ?? ""))
			} else {
				Err(NoSuchColumn)
			}

		## Data records; the header is not counted.
		len : Table -> U64
		len = |table| table.rows.len()

		is_empty : Table -> Bool
		is_empty = |table| table.rows.is_empty()

		## Header first, so `table_with` and `to_str` compose to the identity on
		## any table read back.
		to_str : Table, Dialect -> Str
		to_str = |table, dialect|
			CsvEmit.rows([table.headers].concat(table.rows.map(Row.fields)), emit_syntax(dialect))

		is_eq : Table, Table -> Bool
		is_eq = |left, right| left.headers == right.headers and left.rows == right.rows
	}

	## Typed reading's cursor over one record: `column` indexes `headers` and
	## `cells`; empty cells are skipped (absent) unless their column is in
	## `offered_empty`.
	DecodeState :: {
		headers : List(Str),
		cells : List(Str),
		column : U64,
		record : U64,
		offered_empty : List(Str),
		is_in_record : Bool,
	}.{
		key_path : DecodeState -> List(Segment)
		key_path = |state| [Index(state.record), Key(state.headers.get(state.column) ?? "")]
	}

	## Typed writing's state: the record's index and the column being written,
	## the record's names and cells, the cell a value writer produced, and the
	## record's first problem. Problems are kept rather than returned, so a
	## record that cannot be written still gives its names to
	## `encode_columns`'s checks.
	EncodeState :: {
		record : U64,
		column : Str,
		names : List(Str),
		cells : List(Str),
		cell : Str,
		problem : Problem,
	}

	## A record's fields while they are written.
	EncodeFields :: { record : U64, names : List(Str), cells : List(Str), problem : Problem }

	Problem : [Clean, Failed(EncodeErr)]

	## The format typed reading and writing go through.
	Format := [Default].{
		rename_field : Format, Str -> Str
		rename_field = |_, name| name

		parse_str : Format, DecodeState -> Try({ value : Str, rest : DecodeState }, [Mismatch({ path : List(Segment), expected : Str })])
		parse_str = |_, state| Ok({ value: cell_text(state), rest: next_cell(state) })

		parse_bool : Format, DecodeState -> Try({ value : Bool, rest : DecodeState }, [Mismatch({ path : List(Segment), expected : Str })])
		parse_bool = |_, state| read_cell(state, CsvCell.boolean, EncodingPath.expected_bool)

		parse_u8 : Format, DecodeState -> Try({ value : U8, rest : DecodeState }, [Mismatch({ path : List(Segment), expected : Str })])
		parse_u8 = |_, state| read_cell(state, |text| CsvCell.unsigned(text, U128.to_u8_try), EncodingPath.expected_u8)

		parse_i8 : Format, DecodeState -> Try({ value : I8, rest : DecodeState }, [Mismatch({ path : List(Segment), expected : Str })])
		parse_i8 = |_, state| read_cell(state, |text| CsvCell.signed(text, I128.to_i8_try), EncodingPath.expected_i8)

		parse_u16 : Format, DecodeState -> Try({ value : U16, rest : DecodeState }, [Mismatch({ path : List(Segment), expected : Str })])
		parse_u16 = |_, state| read_cell(state, |text| CsvCell.unsigned(text, U128.to_u16_try), EncodingPath.expected_u16)

		parse_i16 : Format, DecodeState -> Try({ value : I16, rest : DecodeState }, [Mismatch({ path : List(Segment), expected : Str })])
		parse_i16 = |_, state| read_cell(state, |text| CsvCell.signed(text, I128.to_i16_try), EncodingPath.expected_i16)

		parse_u32 : Format, DecodeState -> Try({ value : U32, rest : DecodeState }, [Mismatch({ path : List(Segment), expected : Str })])
		parse_u32 = |_, state| read_cell(state, |text| CsvCell.unsigned(text, U128.to_u32_try), EncodingPath.expected_u32)

		parse_i32 : Format, DecodeState -> Try({ value : I32, rest : DecodeState }, [Mismatch({ path : List(Segment), expected : Str })])
		parse_i32 = |_, state| read_cell(state, |text| CsvCell.signed(text, I128.to_i32_try), EncodingPath.expected_i32)

		parse_u64 : Format, DecodeState -> Try({ value : U64, rest : DecodeState }, [Mismatch({ path : List(Segment), expected : Str })])
		parse_u64 = |_, state| read_cell(state, |text| CsvCell.unsigned(text, U128.to_u64_try), EncodingPath.expected_u64)

		parse_i64 : Format, DecodeState -> Try({ value : I64, rest : DecodeState }, [Mismatch({ path : List(Segment), expected : Str })])
		parse_i64 = |_, state| read_cell(state, |text| CsvCell.signed(text, I128.to_i64_try), EncodingPath.expected_i64)

		parse_u128 : Format, DecodeState -> Try({ value : U128, rest : DecodeState }, [Mismatch({ path : List(Segment), expected : Str })])
		parse_u128 = |_, state| read_cell(state, |text| CsvCell.unsigned(text, |size| Ok(size)), EncodingPath.expected_u128)

		parse_i128 : Format, DecodeState -> Try({ value : I128, rest : DecodeState }, [Mismatch({ path : List(Segment), expected : Str })])
		parse_i128 = |_, state| read_cell(state, |text| CsvCell.signed(text, |value| Ok(value)), EncodingPath.expected_i128)

		parse_f32 : Format, DecodeState -> Try({ value : F32, rest : DecodeState }, [Mismatch({ path : List(Segment), expected : Str })])
		parse_f32 = |_, state| read_cell(state, CsvCell.f32, EncodingPath.expected_f32)

		parse_f64 : Format, DecodeState -> Try({ value : F64, rest : DecodeState }, [Mismatch({ path : List(Segment), expected : Str })])
		parse_f64 = |_, state| read_cell(state, CsvCell.f64, EncodingPath.expected_f64)

		parse_dec : Format, DecodeState -> Try({ value : Dec, rest : DecodeState }, [Mismatch({ path : List(Segment), expected : Str })])
		parse_dec = |_, state| read_cell(state, CsvCell.dec, EncodingPath.expected_dec)

		## A record is one CSV record; a record nested in a field has no cells
		## of its own to read.
		parse_record_start : Format, DecodeState -> Try([Counted({ len : U64, rest : DecodeState }), Uncounted(DecodeState)], [Mismatch({ path : List(Segment), expected : Str })])
		parse_record_start = |_, state|
			if state.is_in_record {
				Err(EncodingPath.mismatch_at(state, expected_cell))
			} else {
				Ok(Uncounted({ ..state, is_in_record: True }))
			}

		## Offers the column under the cursor by name, or skips an empty cell
		## so an optional or `Try` field reads it as absent.
		parse_record_field : Format,
		Encoding.FieldName.FieldNames(_shape),
		DecodeState -> Try(
			[
				Field({ field : Encoding.FieldName(_shape), rest : DecodeState }),
				TryField({ name : Str, rest : DecodeState }),
				TryFieldCaseless({ name : Str, rest : DecodeState }),
				Continue(DecodeState),
				Done(DecodeState),
			],
			[Mismatch({ path : List(Segment), expected : Str })],
		)
		parse_record_field = |_, _, state|
			match state.headers.get(state.column) {
				Err(_) => Ok(Done(state))
				Ok(name) =>
					if cell_text(state).is_empty() and !state.offered_empty.contains(name) {
						Ok(Continue(next_cell(state)))
					} else {
						Ok(TryField({ name, rest: state }))
					}
			}

		parse_record_after_field : Format, DecodeState -> Try([Continue(DecodeState), Done(DecodeState)], [Mismatch({ path : List(Segment), expected : Str })])
		parse_record_after_field = |_, state| Ok(Continue(state))

		skip_record_field : Format, DecodeState -> Try(DecodeState, [Mismatch({ path : List(Segment), expected : Str })])
		skip_record_field = |_, state| Ok(next_cell(state))

		parse_local_date : Format, DecodeState -> Try({ value : EncodingDate.Date, rest : DecodeState }, [Mismatch({ path : List(Segment), expected : Str })])
		parse_local_date = |_, state| read_cell(state, CsvDate.local_date, EncodingPath.expected_local_date)

		parse_local_time : Format, DecodeState -> Try({ value : EncodingDate.Time, rest : DecodeState }, [Mismatch({ path : List(Segment), expected : Str })])
		parse_local_time = |_, state| read_cell(state, CsvDate.local_time, EncodingPath.expected_local_time)

		parse_local_datetime : Format, DecodeState -> Try({ value : CsvDate.LocalDatetime, rest : DecodeState }, [Mismatch({ path : List(Segment), expected : Str })])
		parse_local_datetime = |_, state| read_cell(state, CsvDate.local_datetime, EncodingPath.expected_local_datetime)

		parse_offset_datetime : Format, DecodeState -> Try({ value : CsvDate.OffsetDatetime, rest : DecodeState }, [Mismatch({ path : List(Segment), expected : Str })])
		parse_offset_datetime = |_, state| read_cell(state, CsvDate.offset_datetime, EncodingPath.expected_offset_datetime)

		encode_str : Str, EncodeState -> Try(EncodeState, err)
		encode_str = |value, state| Ok(with_cell(state, value))

		encode_bool : Bool, EncodeState -> Try(EncodeState, err)
		encode_bool = |value, state| Ok(with_cell(state, CsvCell.boolean_text(value)))

		encode_u8 : U8, EncodeState -> Try(EncodeState, err)
		encode_u8 = |value, state| Ok(with_cell(state, value.to_str()))

		encode_i8 : I8, EncodeState -> Try(EncodeState, err)
		encode_i8 = |value, state| Ok(with_cell(state, value.to_str()))

		encode_u16 : U16, EncodeState -> Try(EncodeState, err)
		encode_u16 = |value, state| Ok(with_cell(state, value.to_str()))

		encode_i16 : I16, EncodeState -> Try(EncodeState, err)
		encode_i16 = |value, state| Ok(with_cell(state, value.to_str()))

		encode_u32 : U32, EncodeState -> Try(EncodeState, err)
		encode_u32 = |value, state| Ok(with_cell(state, value.to_str()))

		encode_i32 : I32, EncodeState -> Try(EncodeState, err)
		encode_i32 = |value, state| Ok(with_cell(state, value.to_str()))

		encode_u64 : U64, EncodeState -> Try(EncodeState, err)
		encode_u64 = |value, state| Ok(with_cell(state, value.to_str()))

		encode_i64 : I64, EncodeState -> Try(EncodeState, err)
		encode_i64 = |value, state| Ok(with_cell(state, value.to_str()))

		encode_u128 : U128, EncodeState -> Try(EncodeState, err)
		encode_u128 = |value, state| Ok(with_cell(state, value.to_str()))

		encode_i128 : I128, EncodeState -> Try(EncodeState, err)
		encode_i128 = |value, state| Ok(with_cell(state, value.to_str()))

		encode_f32 : F32, EncodeState -> Try(EncodeState, err)
		encode_f32 = |value, state| Ok(with_cell(state, CsvCell.f32_text(value)))

		encode_f64 : F64, EncodeState -> Try(EncodeState, err)
		encode_f64 = |value, state| Ok(with_cell(state, CsvCell.f64_text(value)))

		encode_dec : Dec, EncodeState -> Try(EncodeState, err)
		encode_dec = |value, state| Ok(with_cell(state, value.to_str()))

		encode_local_date : Format, EncodingDate.Date, EncodeState -> Try(EncodeState, err)
		encode_local_date = |_, date, state| Ok(with_date_cell(state, CsvDate.date_text(date, cell_path(state))))

		encode_local_time : Format, EncodingDate.Time, EncodeState -> Try(EncodeState, err)
		encode_local_time = |_, time, state| Ok(with_date_cell(state, CsvDate.time_text(time, cell_path(state))))

		encode_local_datetime : Format, CsvDate.LocalDatetime, EncodeState -> Try(EncodeState, err)
		encode_local_datetime = |_, moment, state| Ok(with_date_cell(state, CsvDate.local_datetime_text(moment, cell_path(state))))

		encode_offset_datetime : Format, CsvDate.OffsetDatetime, EncodeState -> Try(EncodeState, err)
		encode_offset_datetime = |_, moment, state| Ok(with_date_cell(state, CsvDate.offset_datetime_text(moment, cell_path(state))))

		## One record is one CSV record: each field's name and cell, in the
		## order the derive supplies them.
		encode_record : EncodeState, U64, (EncodeFields, (EncodeFields, Str, (EncodeState -> Try(EncodeState, err)) -> Try(EncodeFields, err)) -> Try(EncodeFields, err)) -> Try(EncodeState, err)
		encode_record = |state, _, write_fields| {
			start = { record: state.record, names: [], cells: [], problem: state.problem }
			written = write_fields(start, |fields, name, write_value| {
				value = write_value({ record: fields.record, column: name, names: [], cells: [], cell: "", problem: fields.problem })?
				Ok({ record: fields.record, names: fields.names.append(name), cells: fields.cells.append(value.cell), problem: value.problem })
			})?
			Ok({ record: state.record, column: state.column, names: written.names, cells: written.cells, cell: "", problem: written.problem })
		}
	}

	a.Parseable(errs) :
		where [
			a.parser_for : Format -> (DecodeState -> Try({ value : a, rest : DecodeState }, errs)),
		]

	a.Encodable(err) :
		where [
			a.encoder_for : Format -> (a, EncodeState -> Try(EncodeState, err)),
		]

	## Every record as fields, the header record (if any) included.
	parse : Str -> Try(List(List(Str)), Err)
	parse = |text| CsvParse.rows(text, parse_syntax(Dialect.csv))

	parse_with : Str, Dialect -> Try(List(List(Str)), Err)
	parse_with = |text, dialect| CsvParse.rows(text, parse_syntax(dialect))

	## Rows as text, quoting what needs it, with no final line break.
	to_str : List(List(Str)) -> Str
	to_str = |rows| CsvEmit.rows(rows, emit_syntax(Dialect.csv))

	to_str_with : List(List(Str)), Dialect -> Str
	to_str_with = |rows, dialect| CsvEmit.rows(rows, emit_syntax(dialect))

	## The first record as column names; `MissingHeader` for no records,
	## `DuplicateHeader` for a repeated name.
	table : Str -> Try(Table, Err)
	table = |text| Csv.table_with(text, Dialect.csv)

	table_with : Str, Dialect -> Try(Table, Err)
	table_with = |text, dialect| {
		read = CsvParse.with_header(text, parse_syntax(dialect))?
		Ok({ headers: read.header, rows: read.records.map(|fields| Row.new(read.header, fields)) })
	}

	## Each data record as an `a`, fields matched to columns by name. Columns
	## the type does not name are ignored; an empty cell is absent for an
	## optional (`?:`) or `Try` field. A cell's path is
	## `[Index(record), Key(column)]`, records counted from 0 after the header.
	decode : Str -> Try(List(a), [Parse(Err), Mismatch({ path : List(Segment), expected : Str }), MissingRequiredField(Str), ..errs])
		where [a.Parseable([Parse(Err), Mismatch({ path : List(Segment), expected : Str }), MissingRequiredField(Str), ..errs])]
	decode = |text| Csv.decode_with(text, Dialect.csv)

	decode_with : Str, Dialect -> Try(List(a), [Parse(Err), Mismatch({ path : List(Segment), expected : Str }), MissingRequiredField(Str), ..errs])
		where [a.Parseable([Parse(Err), Mismatch({ path : List(Segment), expected : Str }), MissingRequiredField(Str), ..errs])]
	decode_with = |text, dialect|
		match CsvParse.with_header(text, parse_syntax(dialect)) {
			Err(problem) => Err(Parse(problem))
			Ok(read) => decode_records(read.header, read.records, 0, [], [])
		}

	## A header of every field name any record has, in byte order, and a
	## record per value with absent fields as empty cells. No values write
	## nothing.
	encode : List(a) -> Try(Str, EncodeErr) where [a.Encodable(EncodeErr)]
	encode = |values| Csv.encode_with(values, Dialect.csv)

	encode_with : List(a), Dialect -> Try(Str, EncodeErr) where [a.Encodable(EncodeErr)]
	encode_with = |values, dialect| {
		records = encode_records(values)?
		first_problem(records)?
		headers = EncodingText.sorted_union(records.map(|record| record.names))
		Ok(write_records(headers, records, dialect))
	}

	## As `encode`, with the columns in the given order. Checked in order:
	## `DuplicateColumn` and `UnknownColumn` (a name no record has) by list
	## position, `MissingColumn` (a field some record has, including one that
	## cannot be written) by byte order, then `Encode`.
	encode_columns : List(a), List(Str) -> Try(Str, [DuplicateColumn(Str), UnknownColumn(Str), MissingColumn(Str), Encode(EncodeErr)]) where [a.Encodable(EncodeErr)]
	encode_columns = |values, columns| Csv.encode_columns_with(values, columns, Dialect.csv)

	encode_columns_with : List(a), List(Str), Dialect -> Try(Str, [DuplicateColumn(Str), UnknownColumn(Str), MissingColumn(Str), Encode(EncodeErr)]) where [a.Encodable(EncodeErr)]
	encode_columns_with = |values, columns, dialect| {
		records = encode_records(values).map_err(|problem| Encode(problem))?
		names = EncodingText.sorted_union(records.map(|record| record.names))
		check_columns(columns, names)?
		first_problem(records).map_err(|problem| Encode(problem))?
		Ok(write_records(columns, records, dialect))
	}

	## A one-line message: `line L, column C: …` where there is a position.
	err_to_str : Err -> Str
	err_to_str = |problem|
		match problem {
			Syntax(syntax) => "${EncodingText.position_prefix(syntax.line, syntax.column)}expected ${syntax.expected}"
			RaggedRow(ragged) => "line ${ragged.line.to_str()}: expected ${ragged.width.to_str()} fields, found ${ragged.found.to_str()}"
			MissingHeader => "expected a header record, but the input has no records"
			DuplicateHeader(duplicate) => "${EncodingText.position_prefix(duplicate.line, duplicate.column)}the column name \"${duplicate.name}\" is repeated"
		}
}

expected_cell : Str
expected_cell = "a single cell, not a record"

parse_syntax : Csv.Dialect -> CsvParse.Syntax
parse_syntax = |dialect| {
	delimiter: dialect.delimiter,
	quote: dialect.quote,
	comment: dialect.comment,
	trim: dialect.trim,
	skip_blank_lines: dialect.skip_blank_lines,
	ragged: dialect.ragged,
}

emit_syntax : Csv.Dialect -> CsvEmit.Syntax
emit_syntax = |dialect| {
	delimiter: dialect.delimiter,
	quote: dialect.quote,
	comment: dialect.comment,
	newline: dialect.newline,
}

cell_text : Csv.DecodeState -> Str
cell_text = |state| state.cells.get(state.column) ?? ""

next_cell : Csv.DecodeState -> Csv.DecodeState
next_cell = |state| { ..state, column: state.column + 1 }

read_cell : Csv.DecodeState, (Str -> Try(v, [Invalid])), Str -> Try({ value : v, rest : Csv.DecodeState }, [Mismatch({ path : List(Csv.Segment), expected : Str })])
read_cell = |state, read, expected|
	match read(cell_text(state)) {
		Ok(value) => Ok({ value, rest: next_cell(state) })
		Err(_) => Err(EncodingPath.mismatch_at(state, expected))
	}

## Each record in turn. A required field whose cell was empty comes back as
## `MissingRequiredField`; its column is then offered empty, for this record
## and every later one, and the record read again, so each column costs at
## most one retry.
decode_records : List(Str), List(List(Str)), U64, List(Str), List(a) -> Try(List(a), [Parse(Csv.Err), Mismatch({ path : List(Csv.Segment), expected : Str }), MissingRequiredField(Str), ..errs])
	where [a.Csv.Parseable([Parse(Csv.Err), Mismatch({ path : List(Csv.Segment), expected : Str }), MissingRequiredField(Str), ..errs])]
decode_records = |headers, records, index, offered_empty, decoded|
	match records.get(index) {
		Err(_) => Ok(decoded)
		Ok(cells) => {
			state = { headers, cells, column: 0, record: index, offered_empty, is_in_record: False }
			match EncodingPath.run(Csv.Format.Default, state) {
				Ok(value) => decode_records(headers, records, index + 1, offered_empty, decoded.append(value))
				Err(MissingRequiredField(name)) if headers.contains(name) and !offered_empty.contains(name) =>
					decode_records(headers, records, index, offered_empty.append(name), decoded)
				Err(problem) => Err(problem)
			}
		}
	}

with_cell : Csv.EncodeState, Str -> Csv.EncodeState
with_cell = |state, text| { ..state, cell: text }

cell_path : Csv.EncodeState -> List(Csv.Segment)
cell_path = |state| [Index(state.record), Key(state.column)]

## A date cell's text, or an empty cell and the problem, kept only if it is
## the record's first.
with_date_cell : Csv.EncodeState, Try(Str, Csv.EncodeErr) -> Csv.EncodeState
with_date_cell = |state, written|
	match (written, state.problem) {
		(Ok(text), _) => with_cell(state, text)
		(Err(problem), Clean) => { ..state, cell: "", problem: Failed(problem) }
		(Err(_), Failed(_)) => with_cell(state, "")
	}

EncodedRecord : { names : List(Str), cells : List(Str), problem : Csv.Problem }

encode_record : a, U64 -> Try(EncodedRecord, err) where [a.Csv.Encodable(err)]
encode_record = |value, index| {
	start = { record: index, column: "", names: [], cells: [], cell: "", problem: Clean }
	state = EncodingPath.encode_run(Csv.Format.Default, start, value)?
	Ok({ names: state.names, cells: state.cells, problem: state.problem })
}

encode_records : List(a) -> Try(List(EncodedRecord), err) where [a.Csv.Encodable(err)]
encode_records = |values| values.map_with_index(|value, index| encode_record(value, index)).map_try(|attempt| attempt)

## The first record's problem, in record order.
first_problem : List(EncodedRecord) -> Try({}, Csv.EncodeErr)
first_problem = |records|
	records.fold_until(Ok({}), |found, record|
		match record.problem {
			Clean => Continue(found)
			Failed(problem) => Break(Err(problem))
		})

## A header record and each record's cells placed under it; nothing for no
## records.
write_records : List(Str), List(EncodedRecord), Csv.Dialect -> Str
write_records = |headers, records, dialect|
	if records.is_empty() {
		""
	} else {
		CsvEmit.rows([headers].concat(records.map(|record| placed(headers, record))), emit_syntax(dialect))
	}

placed : List(Str), EncodedRecord -> List(Str)
placed = |headers, record| headers.map(|header| cell_under(header, record))

cell_under : Str, EncodedRecord -> Str
cell_under = |header, record|
	match record.names.find_first_index(|name| name == header) {
		Ok(index) => record.cells.get(index) ?? ""
		Err(_) => ""
	}

check_columns : List(Str), List(Str) -> Try({}, [DuplicateColumn(Str), UnknownColumn(Str), MissingColumn(Str)])
check_columns = |columns, names| {
	repeated = CsvParse.first_repeat(columns).map_ok(|index| columns.get(index) ?? "")
	unknown = columns.find_first(|column| !names.contains(column))
	missing = names.find_first(|name| !columns.contains(name))
	match (repeated, unknown, missing) {
		(Ok(name), _, _) => Err(DuplicateColumn(name))
		(_, Ok(name), _) => Err(UnknownColumn(name))
		(_, _, Ok(name)) => Err(MissingColumn(name))
		_ => Ok({})
	}
}
