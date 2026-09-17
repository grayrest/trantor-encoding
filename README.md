# trantor-encoding

Base64, hex, CSV and TOML for the
[trantor platform](https://github.com/grayrest/trantor), in Roc with no host
code.

- **`Base64`** and **`Hex`**: RFC 4648 byte encodings.
- **`Csv`**: RFC 4180 text as rows, as tables with a header record, and as
  typed records with cells in XML Schema 1.1's forms.
- **`Toml`**: TOML 1.1.0 read into a value tree or typed records, written as
  TOML 1.0 (or 1.1), and edited in place with comments and layout kept.

## Setup

This package does not provide a program entry point. Add it to a world beside
one that does, like `trantor-cli` or `trantor-http`; it needs nothing else.

```toml
# world.toml
[world]
name = "myapp"

[deps]
trantor-cli      = { path = "../trantor-cli" }
trantor-encoding = { path = "../trantor-encoding" }
```

## Stability

Encoded output is byte-identical for the same input within a major version:
`Csv.to_str`, `Csv.encode` and `Csv.encode_columns` (and their `_with` forms),
`Toml.to_str` and `Toml.encode` in both TOML versions, and the text a
`Toml.Document` edit inserts. A checked-in generated file does not change on a
minor upgrade. Base64 is fixed by RFC 4648; hex is RFC 4648 base16 written
lowercase.

Error tags and their payloads are covered. Message text is not: the `expected`
field of `Syntax` and `Mismatch` errors, and what `Csv.err_to_str` and
`Toml.err_to_str` return, may be reworded in any release.

## Base64 and Hex

```roc
import pf.Base64
import pf.Hex

Base64.encode("hello".to_utf8())   # "aGVsbG8="
Base64.encode_url([0xFB, 0xFF])    # "-_8"
Base64.decode("aGVsbG8")           # Ok([104, 101, 108, 108, 111]), padding is optional
Base64.decode("aGVsbG9=")          # Err(InvalidBase64(6)), 9 leaves unused bits set
Hex.encode([0xCA, 0xFE])           # "cafe"
Hex.decode("CAFE")                 # Ok([202, 254])
Hex.decode("caf")                  # Err(OddLength)
```

```roc
Base64.encode : List(U8) -> Str                                                 # standard alphabet, padded
Base64.encode_url : List(U8) -> Str                                             # URL-safe alphabet, unpadded
Base64.decode : Str -> Try(List(U8), [InvalidBase64(U64), InvalidLength])       # standard alphabet
Base64.decode_url : Str -> Try(List(U8), [InvalidBase64(U64), InvalidLength])   # URL-safe alphabet

Hex.encode : List(U8) -> Str                                                    # lowercase
Hex.decode : Str -> Try(List(U8), [InvalidHex(U64), OddLength])                 # either case
```

The decoders take padding or its absence and nothing else: no whitespace, line
breaks, separators or `0x` prefixes. Each byte list has one encoding, so a
final group whose unused bits are not zero is refused. Indices are 0-based
offsets into the string's UTF-8 bytes, and the first problem found scanning
left to right is the one reported:

- `InvalidBase64(index)`: a byte outside the alphabet; a misplaced `=` (`QQ=A`
  at the `=`); data after complete padding; excess padding; nonzero unused
  bits, at the last data character.
- `InvalidLength`: a final group of one character, checked only when the scan
  found none of the above.
- `InvalidHex(index)` is reported before `OddLength`.

Binary fields in typed records are `Str`, converted with these functions;
there is no bytes type that decodes itself.

## CSV

### Rows and tables

```roc
import pf.Csv

rows = Csv.parse("name,age\nada,36\n\"hopper, grace\",45")?
# [["name", "age"], ["ada", "36"], ["hopper, grace", "45"]]
Csv.to_str(rows)                          # "name,age\nada,36\n\"hopper, grace\",45"
Csv.to_str_with(rows, Csv.Dialect.tsv)    # "name\tage\nada\t36\nhopper, grace\t45"

table = Csv.table("name,age\nada,36\ngrace,45")?
table.column("name")                      # Ok(["ada", "grace"])
row = table.rows.get(1)?                  # the record a decode error calls Index(1)
row.get("age")                            # Ok("45")
```

`parse` reads every record, the header included; `table` takes the first
record as column names. Reading accepts LF, CRLF and lone CR line breaks and
skips one leading byte-order mark. `Dialect.newline` sets what writing uses,
and `to_str` writes no final line break.

### Typed records

```roc
Person : { name : Str, age : U32, email ?: Str }

people : Try(List(Person), _)
people = Csv.decode("name,age,email\nada,36,ada@example.com\ngrace,45,")
# ada 36 ada@example.com; grace 45 with no email, since an empty cell is absent

rows : List(Person)
rows = [{ name: "ada", age: 36, email: "ada@example.com" }, { name: "grace", age: 45 }]
Csv.encode(rows)                                     # Ok("age,email,name\n36,ada@example.com,ada\n45,,grace")
Csv.encode_columns(rows, ["name", "age", "email"])   # Ok("name,age,email\nada,36,ada@example.com\ngrace,45,")
Csv.encode_columns(rows, ["name", "age"])            # Err(MissingColumn("email"))

bad : Try(List(Person), _)
bad = Csv.decode("name,age\nada,36\ngrace,old")
# Err(Mismatch({ path: [Index(1), Key("age")], expected: … }))
```

- Fields match columns by name. Columns the type does not name are ignored; a
  field whose column the header lacks is `MissingRequiredField(name)`.
- An empty cell is absent for an optional (`?:`) or `Try` field. A required
  `Str` field reads it as `""`; any other required field is a `Mismatch`.
- A `Mismatch` path is `[Index(record), Key(column)]`, records counted from 0
  after the header. Skipped blank and comment lines are not counted and a
  quoted cell spanning lines is one record, so `Csv.table(text)` then
  `table.rows.get(record)` finds the record an error names.
- Decoding stops at the first error.
- `encode`'s header is every field name any record has, sorted by bytes. A
  record without an optional field gets an empty cell under that name.
- `encode_columns` writes the columns in the order given. It checks, in order:
  `DuplicateColumn` and `UnknownColumn` (a name no record has) by list
  position, `MissingColumn` (a field some record has that the list leaves out)
  by the field's byte order, then `Encode`. An optional field absent from
  every record cannot be told from a misspelled name, so it is
  `UnknownColumn`; leave that column out.
- Encoding no records writes `""`.

### Cell forms

Typed cells use XML Schema 1.1's lexical forms, which the W3C's CSV on the Web
recommendation reads typed cells with:

| Field | Reads | Writes |
|---|---|---|
| integers | `[+-]?[0-9]+`, range-checked | `-42` |
| `F32`, `F64` | `1`, `-2.5`, `.5`, `5.`, `1e10`, `1.5E-3`, `INF`, `+INF`, `-INF`, `NaN` | `1.0`, `-0.0`, `1e300`, `1e-7`, `INF`, `-INF`, `NaN` |
| `Dec` | as floats, without `INF` and `NaN` | `Dec.to_str`: `5.0`, `-0.25` |
| `Bool` | `true`, `false`, `1`, `0` | `true`, `false` |

- The specials are case-sensitive. Underscores, hex, digit grouping and other
  spellings are a `Mismatch`. Spaces around a cell are accepted only through
  `Dialect.trim`.
- Floats are written with the shortest spelling that reads back identically,
  with `.0` added to whole numbers; an `F32` with its own shortest spelling.
- Integer text into a float field rounds to the nearest float, as XML Schema
  has it: `9007199254740993` reads as `9007199254740992`. TOML refuses the
  same.
- A float past the field type's range reads as infinity. A `Dec` past its
  range is a `Mismatch`.
- `Dec` cells apply the exponent and keep 18 fractional digits, cutting the
  rest toward zero, as TOML does (see [Floats and `Dec`](#floats-and-dec)).

### Dates in cells

`Toml`'s date types (see [Dates and times](#dates-and-times)) work in CSV
records too:

```roc
import pf.Toml

Event : { name : Str, on : Toml.LocalDate, at : Toml.OffsetDatetime }

events : Try(List(Event), _)
events = Csv.decode("name,on,at\nlaunch,2026-03-08,2026-03-08T09:30:00-05:00")
Csv.encode(events?)                       # Ok("at,name,on\n2026-03-08T09:30:00-05:00,launch,2026-03-08")

late : Try(List({ at : Toml.LocalTime }), _)
late = Csv.decode("at\n24:00:00")
# Err(Mismatch({ path: [Index(0), Key("at")], expected: … }))
```

- The forms are XML Schema 1.1's: a date `-?YYYY-MM-DD`, a time
  `hh:mm:ss(.fraction)?`, a date-time date`T`time.
- A timezone (`Z` or `±hh:mm`, within ±14:00) separates an offset date-time
  from a local one: a local field refuses a cell with one, an offset field a
  cell without. `-00:00` is offset 0.
- `T` and `Z` are uppercase and seconds are required; a space for `T` or
  missing seconds are a `Mismatch`.
- Years have four or more digits and may be negative; `0000` is allowed. A year
  of more than four digits with a leading zero, or one outside `I32`, is
  refused. Years outside 0–9999 are written in that extended form
  (`10000-01-01`, `-0044-03-15`).
- Fraction digits past nanoseconds are cut off. Writing trims trailing zeros,
  leaves out a zero fraction, and writes offset 0 as `Z`.
- `24:00:00`, which XML Schema 1.1 allows as the end of a day, is refused, as
  is second 60. Both are deliberate departures from the standard.
- Writing refuses what a cell cannot hold, at the cell's path: `InvalidDate`
  for a day not on the ISO calendar, `InvalidTime` for hour 24, second 60 or a
  sub-second field over 999, `InvalidOffset` beyond ±14:00. The first problem
  in record order is returned.

### Excel

Encoders never write a byte-order mark. Excel needs one to read a CSV file as
UTF-8, so prepend it:

```roc
excel = "\u(FEFF)${Csv.encode(rows)?}"
```

Reading skips it again.

### Csv

```roc
Csv.Segment : [Key(Str), Index(U64)]

Csv.Dialect := {
    delimiter : U8,
    quote : U8,
    comment : [NoComment, CommentPrefix(U8)],   # lines starting with the byte are dropped
    trim : Bool,                                # spaces and tabs around fields and quotes
    skip_blank_lines : Bool,
    ragged : [Reject, Allow],                   # records wider or narrower than the first
    newline : [Lf, Crlf],                       # writing only
}
Csv.Dialect.csv : Csv.Dialect                   # ',' and '"', NoComment, no trim, blank lines skipped, Reject, Lf
Csv.Dialect.tsv : Csv.Dialect                   # csv with '\t'
# any other: { ..Csv.Dialect.csv, delimiter: ';' }

# rows
Csv.parse : Str -> Try(List(List(Str)), Csv.Err)
Csv.parse_with : Str, Csv.Dialect -> Try(List(List(Str)), Csv.Err)
Csv.to_str : List(List(Str)) -> Str
Csv.to_str_with : List(List(Str)), Csv.Dialect -> Str

# tables
Csv.table : Str -> Try(Csv.Table, Csv.Err)      # MissingHeader for no records
Csv.table_with : Str, Csv.Dialect -> Try(Csv.Table, Csv.Err)

Csv.Table := { headers : List(Str), rows : List(Csv.Row) }
column : Csv.Table, Str -> Try(List(Str), [NoSuchColumn])
len : Csv.Table -> U64                          # data records, not the header
is_empty : Csv.Table -> Bool
to_str : Csv.Table, Csv.Dialect -> Str          # header first
# `==` compares headers and rows

Csv.Row := { headers : List(Str), fields : List(Str) }
Csv.Row.new : List(Str), List(Str) -> Csv.Row   # headers, fields
get : Csv.Row, Str -> Try(Str, [NoSuchColumn])  # the first column of that name; "" past a short record
at : Csv.Row, U64 -> Try(Str, [OutOfBounds])
fields : Csv.Row -> List(Str)
len : Csv.Row -> U64
has_column : Csv.Row, Str -> Bool
# `==` compares headers and fields

# typed records
Csv.decode : Str -> Try(List(a), [Parse(Csv.Err), Mismatch({ path : List(Csv.Segment), expected : Str }), MissingRequiredField(Str), ..errs])
    where [a.Csv.Parseable([Parse(Csv.Err), Mismatch({ path : List(Csv.Segment), expected : Str }), MissingRequiredField(Str), ..errs])]
Csv.decode_with : Str, Csv.Dialect -> Try(List(a), …)                                    # as decode
Csv.encode : List(a) -> Try(Str, Csv.EncodeErr) where [a.Csv.Encodable(Csv.EncodeErr)]
Csv.encode_with : List(a), Csv.Dialect -> Try(Str, Csv.EncodeErr) where [a.Csv.Encodable(Csv.EncodeErr)]
Csv.encode_columns : List(a), List(Str) -> Try(Str, [DuplicateColumn(Str), UnknownColumn(Str), MissingColumn(Str), Encode(Csv.EncodeErr)])
    where [a.Csv.Encodable(Csv.EncodeErr)]
Csv.encode_columns_with : List(a), List(Str), Csv.Dialect -> Try(Str, …)                 # as encode_columns

a.Csv.Parseable(errs) : where [a.parser_for : Csv.Format -> (Csv.DecodeState -> Try({ value : a, rest : Csv.DecodeState }, errs))]
a.Csv.Encodable(err) : where [a.encoder_for : Csv.Format -> (a, Csv.EncodeState -> Try(Csv.EncodeState, err))]

# errors
Csv.Err : [
    Syntax({ line : U64, column : U64, expected : Str }),
    RaggedRow({ line : U64, width : U64, found : U64 }),         # line: the record's first; width: the first record's
    MissingHeader,
    DuplicateHeader({ line : U64, column : U64, name : Str }),   # at the second occurrence
]
Csv.EncodeErr : [
    InvalidDate({ path : List(Csv.Segment), date : { year : I32, month : U8, day : U8 } }),
    InvalidTime({ path : List(Csv.Segment), time : { hour : U8, minute : U8, second : U8, millisecond : U16, microsecond : U16, nanosecond : U16 } }),
    InvalidOffset({ path : List(Csv.Segment), offset : { minutes : I16 } }),
]
Csv.err_to_str : Csv.Err -> Str                 # "line L, column C: …"
```

Lines are 1-based and columns count code points. An unterminated quote is
reported at its opening quote; text after a closing quote, and a quote inside
an unquoted field, at the offending character. `DuplicateHeader` compares
names byte for byte; one empty name is allowed and a second is a duplicate.

## TOML

### Reading

```roc
import pf.Toml

config = Toml.parse("title = \"demo\"\n\n[server]\nport = 8080\n")?
# Table([("title", String("demo")), ("server", Table([("port", Integer(8080))]))])

Toml.parse("a = 1\na = 2")
# Err(DuplicateKey({ line: 2, column: 1, path: [Key("a")] }))
# and Toml.err_to_str of that error: "line 2, column 1: the key a is already defined"
```

`parse` reads TOML 1.1.0, and so every 1.0 document. It stops at the first
error; lines are 1-based and columns count code points. One leading
byte-order mark is skipped, CRLF is accepted, and a line break inside a
multi-line string reads as `\n` whichever the file has (an escaped `\r` stays).
Tables keep document order.

- `DuplicateKey` covers every conflicting definition, not only a key written
  twice: a header reopening a defined table, a dotted key into a header or
  inline table, `[[x]]` over a static array. It is at the second definition,
  with the full path (`Index` included inside arrays of tables).
- `OutOfRange` is an integer past `I64`, or a well-formed date, time or offset
  outside its fields' ranges (month 13, second 60, an offset past ±23:59).
  `text` is the literal.
- `TooDeep` is a table or array nested past 128 levels. The root table is the
  first level, and every kind of nesting counts: headers, dotted keys, inline
  tables, arrays, and arrays of tables (an `[[x]]` element is two levels).
- Years are 0–9999 and offsets within ±23:59; `Z` and `+00:00` both read as 0.
  Fraction digits past nanoseconds are cut off.
- Second 60 is refused when reading (`OutOfRange`) and writing
  (`InvalidTime`), a deliberate departure from RFC 3339's grammar, which allows
  a leap second.

### Typed values

```roc
Config : {
    name : Str,
    released : Toml.LocalDate,
    tags : List(Str),
    limits : Dict(Str, U32),
    server : { host : Str, port : U16 },
}

config : Try(Config, _)
config = Toml.decode("name = \"demo\"\nreleased = 2026-03-08\ntags = [\"a\", \"b\"]\n\n[server]\nhost = \"localhost\"\nport = 8080\n\n[limits]\nrequests = 100\n")
Toml.encode(config?)
# Ok("name = \"demo\"\nreleased = 2026-03-08\ntags = [\"a\", \"b\"]\n\n[limits]\nrequests = 100\n\n[server]\nhost = \"localhost\"\nport = 8080\n")

wrong : Try({ server : { port : U16 } }, _)
wrong = Toml.decode("[server]\nport = \"8080\"")
# Err(Mismatch({ path: [Key("server"), Key("port")], expected: … }))

missing : Try({ server : { host : Str, port : U16 } }, _)
missing = Toml.decode("[server]\nhost = \"localhost\"")
# Err(MissingRequiredField("port"))
```

- Tables decode into records and `Dict(Str, _)`, arrays into lists and tuples.
  Integers decode into any width they fit. Floats decode into `F32`, `F64` and
  `Dec`; a float into an integer field is a `Mismatch`. An integer decodes into
  a float field only when the float holds it exactly (`9007199254740993` into
  `F64` is a `Mismatch`).
- `MissingRequiredField` carries only the field's name, as the compiler
  produces it, not its path. A name used in two tables is ambiguous there: give
  the field a default, or decode that table on its own with `decode_value`.
- `Try` fields decode (absent is `Err`) but do not encode. The encoders cannot
  write a tag union, so encoding a record with a `Try` field, or any tag-union
  field, is a compile error. Optional (`?:`) fields decode and encode; an absent
  one is left out.
- `Dict` keys encode only as `Str`, sorted by bytes.
- A `Toml.Value` field holds whatever is at that key.
- Writing stops at the first problem: a date, time or offset TOML cannot hold,
  an integer past `I64`, a root that is not a table, or nesting past 128
  levels, which a typed value can reach too.

### Writing

```roc
value : Toml.Value
value = Table([
    ("name", String("demo")),
    ("ratio", Float(Toml.float_from_f64(1.5))),
    ("server", Table([("port", Integer(8080))])),
])
Toml.to_str(value)                           # Ok("name = \"demo\"\nratio = 1.5\n\n[server]\nport = 8080\n")

alarm : Toml.Value
alarm = Table([("at", LocalTime({ hour: 7, minute: 30, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 })), ("bell", String("\u(7)"))])
Toml.to_str(alarm)                           # Ok("at = 07:30:00\nbell = \"\\u0007\"\n")
Toml.to_str_with(alarm, { version: V1_1 })   # Ok("at = 07:30\nbell = \"\\x07\"\n")
```

A `Value` is built from its tags under a `Toml.Value` annotation. A float needs
a `Toml.Float`: `Toml.float_from_f64`, or `Toml.float_from_dec` for an exact
decimal.

Versions: reading accepts TOML 1.1.0. Writing produces what a 1.0 parser
accepts unless `V1_1` is asked for, which only adds the `\e` and `\xHH` escapes
and leaves out seconds that are zero with no fraction. Inline tables and arrays
are written on one line in both.

A document is written as:

- each table's keys first, then its sub-tables as `[section]`, then its arrays
  of tables as `[[section]]`, with a blank line between sections; every
  sub-table gets its own header and the root none;
- a non-empty array holding only tables as `[[x]]`; an empty or mixed array
  inline, with any tables in it inline;
- keys bare when they are non-empty `A-Za-z0-9_-`, quoted otherwise;
- basic strings escaping `"`, `\`, U+0000–001F and U+007F (`\b \t \n \f \r` by
  name, others as `\u00XX` with uppercase digits); `\r` is always escaped, and a
  string holding `\n` is written as a multi-line string;
- decimal integers without underscores; floats with their kept spelling (see
  below), which for a generated float is the shortest spelling that reads back
  identically (`1.0`, `-0.0`, `1e300`, `1e-7`, `inf`, `-inf`, `nan`);
- RFC 3339 dates and times, a fraction trimmed of trailing zeros and left out
  when zero, `Z` for offset 0;
- LF line endings and a final line break, and no byte-order mark.

### Floats and `Dec`

```roc
price : Try({ price : Dec }, _)
price = Toml.decode("price = 12345678.123456789012345678")   # Ok({ price: 12345678.123456789012345678 })

rough : Try({ price : F64 }, _)
rough = Toml.decode("price = 12345678.123456789012345678")   # Ok({ price: 12345678.12345679 })

Toml.encode({ price: 12345678.123456789012345678.Dec })      # Ok("price = 12345678.123456789012345678\n")

cut : Try({ x : Dec, y : Dec }, _)
cut = Toml.decode("x = 0.9999999999999999999\ny = 1e-30")   # Ok({ x: 0.999999999999999999, y: 0.0 })

huge : Try({ x : F64 }, _)
huge = Toml.decode("x = 1e400")                              # Ok({ x: inf })
```

- A `Toml.Float` keeps the float's spelling beside its `F64`, so a `Dec` stays
  exact through `parse`, a `Value`, `decode_value` and `encode_value`. Two
  floats are equal when their `F64`s are: `1.0 == 1.00`, and NaN equals NaN.
- A `Dec` keeps 18 fractional digits and cuts the rest toward zero, after the
  exponent is applied. `to_dec` answers `NotADec` for `inf`, `nan` and values
  past `Dec`'s range, and decoding those into a `Dec` field is a `Mismatch`.
- A float literal past `F32`'s or `F64`'s range reads as infinity.
- `float_from_f64` stores the `F64`'s shortest spelling, so digits past the
  `F64`'s precision are already gone; use `float_from_dec` for decimals.

### Editing

```roc
original = "# what this world builds on\n[deps]\ntrantor-cli = { path = \"../trantor-cli\" }  # the baseline\n"
doc = Toml.parse_document(original)?
doc.to_str() == original                  # True

added = doc.set(Toml.path(["deps", "trantor-encoding"]), Table([("path", String("../trantor-encoding"))]))?
added.to_str()
# "# what this world builds on\n[deps]\ntrantor-cli = { path = \"../trantor-cli\" }  # the baseline\ntrantor-encoding = { path = \"../trantor-encoding\" }\n"

header : Toml.Edit
header = { table: Header }
sectioned = doc.set_with(Toml.path(["deps", "trantor-encoding"]), Table([("path", String("../trantor-encoding"))]), header)?
sectioned.to_str()
# "# what this world builds on\n[deps]\ntrantor-cli = { path = \"../trantor-cli\" }  # the baseline\n\n[deps.trantor-encoding]\npath = \"../trantor-encoding\"\n"

removed = added.remove(Toml.path(["deps", "trantor-cli"]))?
removed.to_str()
# "# what this world builds on\n[deps]\ntrantor-encoding = { path = \"../trantor-encoding\" }\n"

added.get([Key("deps"), Key("trantor-cli"), Key("path")])   # Ok(String("../trantor-cli"))
doc.get(Toml.path(["deps", "nope"]))                         # Err(NotFound([Key("deps"), Key("nope")]))

tags = Toml.parse_document("tags = [\"a\", \"b\"]  # sorted\n")?
appended = tags.append(Toml.path(["tags"]), String("c"))?
appended.to_str()                         # "tags = [\"a\", \"b\", \"c\"]  # sorted\n"

dotted = Toml.parse_document("fruit.apple = 1\n")?
no_apple = dotted.remove(Toml.path(["fruit", "apple"]))?
no_apple.get(Toml.path(["fruit"]))        # Err(NotFound([Key("fruit")]))

headed = Toml.parse_document("[fruit]\napple = 1\n")?
empty_fruit = headed.remove(Toml.path(["fruit", "apple"]))?
empty_fruit.get(Toml.path(["fruit"]))     # Ok(Table([]))
```

A `Document` keeps comments, blank lines, line endings, a byte-order mark,
spellings and table styles; unedited, `to_str` gives the text back byte for
byte. `to_value` is the table `parse` reads, so a document decodes through
`Toml.decode_value(doc.to_value())`, and a typed value goes in through
`Toml.encode_value` and `set`.

- **Replacing** a value keeps its key, spacing and trailing comment. `set`
  always writes the new spelling, even of an equal value.
- **New keys** go after the table's last key, with the indentation and line
  ending of the line they follow. A table that only a deeper header names
  (`[p.x.y]` names `p.x`) gets a `[p.x]` section for its first key.
- **New sections** go after the last section of their family, the sections
  whose path starts with the new one's parent, else at the end of the file.
  One blank line goes before, unless the family has none between its sections.
- **Table style** comes from `Toml.Edit`'s `table`:
  - `Auto`, the default, writes a new table inline inside an inline table or
    array; dotted under a dotted table; inline under a `[header]` table (not
    the root) whose children are all inline tables, with no `[p.x]`
    sub-sections; otherwise as a `[header]`. Arrays of tables are `[[x]]`
    except under inline and dotted tables. So `trantor add`'s first dependency
    under `Auto` is `[deps.foo]`; pass `Inline` for `foo = { … }`.
  - `Inline`, `Header` and `Dotted` force the style of the table set. A table
    takes additions only in the form that created it: `Header` inside an inline
    or dotted table, and `Dotted` inside an inline table or under a table only
    named by headers, are `StyleNotPossible`.
- **Kind changes:** a scalar becoming a table is written inline in place under
  `Auto`. A table becoming a scalar has everything under it removed and the
  scalar added, except an inline table, which is replaced in place. Replacing
  a table with a table in its own style sets new and changed keys and removes
  missing ones; an empty table stays, a dotted one as `a = {}` in place of
  its first key and one only named by deeper headers as an `[a]` section.
- **Removing** a path removes every key/value and section whose path starts
  with it, wherever they are in the file, each with the comment lines directly
  above it. An entry in an inline table or array takes one adjacent comma, or
  its lines when the container spans lines.
- A header table or an inline table stays when its last key is removed. A
  table made only of dotted keys, or only named by deeper headers, goes away
  with its last key, as `fruit` does above.
- **Appending** to an array follows its layout: one line stays one line, and a
  multi-line array gains a line indented like its last element, keeping a
  trailing comma. Appending to an array of tables adds an `[[x]]` section after
  the last element and takes only tables (`NotATable` otherwise). Checking for
  a duplicate is the caller's, with `get`.
- **Versions:** an edit inside a construct already in the document follows its
  layout, 1.1 included. What an edit creates follows `version`: `V1_0` unless
  `set_with` says `V1_1`. `append` writes 1.0.
- **An empty path:** `get([])` is the root; `set([], table)` edits the root key
  by key, keeping comments, and anything but a table is
  `Encode(RootNotATable)`; `remove([])` and `append([], …)` are `NotFound([])`.
- A `Toml.Edit` kept in a variable needs the annotation `e : Toml.Edit`, as
  `header` has above; a literal passed directly does not.
- Errors come in this order: the value (`Encode`, depth included), the lookup
  (`NotFound`, `NotATable`, `NotAnArray`), then the style.
- Lookups are linear, sized for configuration files.
- There is no API for editing comments, and arrays are only appended to or
  replaced whole.

### Dates and times

`Toml.LocalDate`, `Toml.LocalTime`, `Toml.LocalDatetime` and
`Toml.OffsetDatetime` are record-backed nominal types with `==`. Their codecs
use date methods any format may define rather than TOML's format, so they work
in CSV records as well (see [Dates in cells](#dates-in-cells)), and
trantor-temporal's `PlainDate` and `PlainTime` decode and encode through both
formats the same way. The builtin JSON format has no date methods, so one of
these types in a JSON record is a compile error; keep the text in a `Str` field
there.

Build one with a full record literal under an annotation. A record update that
produces one (`{ ..midnight, second: 5 }` with `midnight : Toml.LocalTime`)
hangs the current compiler.

### Toml

```roc
Toml.Segment : [Key(Str), Index(U64)]
Toml.path : List(Str) -> List(Toml.Segment)     # keys only

# values
Toml.Value := [
    String(Str),
    Integer(I64),
    Float(Toml.Float),
    Boolean(Bool),
    OffsetDatetime({ date : Toml.Date, time : Toml.Time, offset : Toml.Offset }),
    LocalDatetime({ date : Toml.Date, time : Toml.Time }),
    LocalDate(Toml.Date),
    LocalTime(Toml.Time),
    Array(List(Toml.Value)),
    Table(List((Str, Toml.Value))),             # document order
]
# `==`: tables by their keys in any order, arrays in order, floats by Toml.Float's `==`

Toml.Float                                      # opaque: a spelling and its F64
to_f64 : Toml.Float -> F64                      # ±infinity past the range
to_dec : Toml.Float -> Try(Dec, [NotADec])      # 18 fractional digits, cut toward zero
Toml.float_from_f64 : F64 -> Toml.Float         # the shortest spelling: 1.0, 1e300, inf, nan
Toml.float_from_dec : Dec -> Toml.Float         # the exact decimal: 5.0, -0.25
# `==` compares the F64s, NaN equal to NaN

Toml.Date : { year : I32, month : U8, day : U8 }
Toml.Time : { hour : U8, minute : U8, second : U8, millisecond : U16, microsecond : U16, nanosecond : U16 }
Toml.Offset : { minutes : I16 }

# dates and times as typed fields, through any format with the date methods
Toml.LocalDate := { year : I32, month : U8, day : U8 }
Toml.LocalTime := { hour : U8, minute : U8, second : U8, millisecond : U16, microsecond : U16, nanosecond : U16 }
Toml.LocalDatetime := { date : Toml.Date, time : Toml.Time }
Toml.OffsetDatetime := { date : Toml.Date, time : Toml.Time, offset : Toml.Offset }

# reading
Toml.parse : Str -> Try(Toml.Value, Toml.Err)
Toml.err_to_str : Toml.Err -> Str               # "line L, column C: …"

# typed values
Toml.decode : Str -> Try(a, [Parse(Toml.Err), Mismatch({ path : List(Toml.Segment), expected : Str }), MissingRequiredField(Str), ..errs])
    where [a.Toml.Parseable([Parse(Toml.Err), Mismatch({ path : List(Toml.Segment), expected : Str }), MissingRequiredField(Str), ..errs])]
Toml.decode_value : Toml.Value -> Try(a, [Mismatch({ path : List(Toml.Segment), expected : Str }), MissingRequiredField(Str), ..errs])
    where [a.Toml.Parseable([Mismatch({ path : List(Toml.Segment), expected : Str }), MissingRequiredField(Str), ..errs])]
Toml.encode : a -> Try(Str, Toml.EncodeErr) where [a.Toml.Encodable(Toml.EncodeErr)]    # encode_value, then to_str
Toml.encode_with : a, Toml.Write -> Try(Str, Toml.EncodeErr) where [a.Toml.Encodable(Toml.EncodeErr)]
Toml.encode_value : a -> Try(Toml.Value, Toml.EncodeErr) where [a.Toml.Encodable(Toml.EncodeErr)]

a.Toml.Parseable(errs) : where [a.parser_for : TomlFormat.Format -> (TomlCursor.DecodeState -> Try({ value : a, rest : TomlCursor.DecodeState }, errs))]
a.Toml.Encodable(err) : where [a.encoder_for : TomlEncode.Encoder -> (a, TomlEncode.EncodeState -> Try(TomlEncode.EncodeState, err))]

# writing
Toml.Write : { version : [V1_0, V1_1] }
Toml.to_str : Toml.Value -> Try(Str, Toml.EncodeErr)                    # TOML 1.0
Toml.to_str_with : Toml.Value, Toml.Write -> Try(Str, Toml.EncodeErr)

# editing
Toml.parse_document : Str -> Try(Toml.Document, Toml.Err)               # refused as parse refuses
Toml.Document                                   # opaque
to_str : Toml.Document -> Str
to_value : Toml.Document -> Toml.Value
get : Toml.Document, List(Toml.Segment) -> Try(Toml.Value, Toml.EditErr)
set : Toml.Document, List(Toml.Segment), Toml.Value -> Try(Toml.Document, Toml.EditErr)
set_with : Toml.Document, List(Toml.Segment), Toml.Value, Toml.Edit -> Try(Toml.Document, Toml.EditErr)
remove : Toml.Document, List(Toml.Segment) -> Try(Toml.Document, Toml.EditErr)
append : Toml.Document, List(Toml.Segment), Toml.Value -> Try(Toml.Document, Toml.EditErr)

Toml.Edit : { version ?: [V1_0, V1_1], table ?: [Auto, Inline, Header, Dotted] }   # V1_0 and Auto when absent

# errors
Toml.Err : [
    Syntax({ line : U64, column : U64, expected : Str }),
    DuplicateKey({ line : U64, column : U64, path : List(Toml.Segment) }),
    OutOfRange({ line : U64, column : U64, text : Str }),
    TooDeep({ line : U64, column : U64 }),
]
Toml.EncodeErr : [
    InvalidDate({ path : List(Toml.Segment), date : Toml.Date }),         # a year outside 0–9999, or not an ISO day
    InvalidTime({ path : List(Toml.Segment), time : Toml.Time }),         # hour 24, second 60, a sub-second field over 999
    InvalidOffset({ path : List(Toml.Segment), offset : Toml.Offset }),   # past ±23:59
    IntegerOutOfRange({ path : List(Toml.Segment), value : Str }),
    RootNotATable,
    DuplicateKey(List(Toml.Segment)),                                     # a Value holding a key twice
    TooDeep(List(Toml.Segment)),                                          # the table or array at level 129
]
Toml.EditErr : [
    NotFound(List(Toml.Segment)),               # the path through the missing key or index
    NotATable(List(Toml.Segment)),              # the value a key was applied to
    NotAnArray(List(Toml.Segment)),             # the value an index was applied to
    StyleNotPossible({ path : List(Toml.Segment), style : [Header, Dotted] }),
    Encode(Toml.EncodeErr),
]
```

`Toml.Value` also has codecs, tied to TOML's format by method name: a
`Toml.Value` field in a CSV or JSON record is a compile error naming
`parse_toml_value` or `encode_toml_value`.

## Known Limits

- CSV has no nested values. A record-typed field is a `Mismatch` at its column
  when decoding and writes an empty cell when encoding; list and dict fields do
  not compile.
- Record field names cannot start with an uppercase letter or hold non-ASCII
  characters, so a CSV column named that way cannot be a field. Read it with
  `Csv.table`.
- A record literal with `True`/`False` fields needs a type annotation to encode
  them as `Bool`; unannotated they are a tag union and do not compile.
- `{ raw }` with a single name is a block, not a record, so
  `Toml.encode_value({ raw })` encodes `raw` itself. Write `{ raw: raw }`.
- A nominal type of your own has no derived codecs; it decodes and encodes only
  through a hand-written `parser_for` and `encoder_for`.
- Input is `Str`, so invalid UTF-8 is refused before a parser sees it.
- `24:00:00` and second 60 are refused in both formats.
- `MissingRequiredField` names no path, and `Try` and tag-union fields do not
  encode (see [Typed values](#typed-values)).
- Toml's date types, `Toml.Value` and trantor-temporal's date types do not work
  in JSON records.
