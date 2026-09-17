import EncodingPath
import TomlLocate
import TomlValue

## How a new value is written, decided from the top down before anything is
## written (D-S3-29, D-S3-41, D-S3-46, D-S3-54.7).
##
## A plan is either entries (key/values relative to the table they are added
## to: one for a scalar, array or inline table, several for dotted keys) or
## blocks (`[header]` and `[[x]]` sections with their key/values, in writing
## order: a table's own entries, its sub-tables, its arrays of tables).
##
## The table a value is added to gives the context: inside an inline table or
## array value, a dotted table, a table only named on the way to a header
## (implicit), a header table whose children are all inline tables, or any
## other header table and the root.
TomlPlan :: [].{

	Segment : EncodingPath.Segment

	Context : [InInline, InDotted, InImplicit, InHeader, InInlineChildren]

	Entry : (List(Str), TomlValue.Value)

	Block : { path : List(Segment), is_array : Bool, entries : List(Entry) }

	Plan : [Entries(List(Entry)), Blocks(List(Block))]

	## `value` set at the end of `names` below the table at `base`, in
	## `context`. Tables named on the way are created: a table's direct parent
	## as a `[header]` when the table is `Inline` or `Dotted` and a header
	## can be there, the others by `Auto`, so a header with nothing but
	## sub-tables stays implicit.
	chain : Context, List(Segment), List(Str), TomlValue.Value, TomlLocate.Style -> Try(Plan, [NotPossible([Header, Dotted])])
	chain = |context, base, names, value, style|
		match names.first() {
			Err(_) => Ok(Entries([]))
			Ok(name) => {
				path = base.append(Key(name))
				rest = names.drop_first(1)
				if rest.is_empty() {
					TomlPlan.target(context, path, value, style)
				} else {
					parent_style = if rest.len() == 1 and is_table(value) and (style == Inline or style == Dotted) and can_hold_header(context) { Header } else { Auto }
					match TomlPlan.table_choice(context, parent_style) ? |_| NotPossible(Header) {
						Inline => {
							_ = TomlPlan.chain(InInline, path, rest, value, style)?
							Ok(Entries([([name], nested(rest, value))]))
						}
						Dotted => TomlPlan.chain(InDotted, path, rest, value, style).map_ok(|inner| prefixed(name, inner))
						Header => TomlPlan.chain(InHeader, path, rest, value, style).map_ok(|inner| under_header(path, inner))
					}
				}
			}
		}

	## `value` set at `path` itself, in `context`.
	target : Context, List(Segment), TomlValue.Value, TomlLocate.Style -> Try(Plan, [NotPossible([Header, Dotted])])
	target = |context, path, value, style| {
		name = last_key(path)
		match value {
			Table(entries) =>
				match TomlPlan.table_choice(context, style)? {
					Inline => Ok(Entries([([name], value)]))
					Dotted => Ok(Entries(TomlPlan.flattened([], [(name, value)])))
					Header => Ok(Blocks(TomlPlan.table_blocks(path, entries)))
				}
			Array(items) if TomlPlan.is_table_array(items) =>
				match array_choice(context, style)? {
					Inline => Ok(Entries([([name], value)]))
					Header => Ok(Blocks(TomlPlan.element_blocks(path, items, 0)))
				}
			_ => Ok(Entries([([name], value)]))
		}
	}

	## A table's style: `Auto` by its context, a forced style where its context
	## allows it (no `Header` inside inline or dotted tables, no `Dotted`
	## inside inline or implicit tables).
	table_choice : Context, TomlLocate.Style -> Try([Inline, Dotted, Header], [NotPossible([Header, Dotted])])
	table_choice = |context, style|
		match (style, context) {
			(Auto, InInline) | (Auto, InInlineChildren) | (Inline, _) => Ok(Inline)
			(Auto, InDotted) => Ok(Dotted)
			(Auto, _) => Ok(Header)
			(Header, InInline) | (Header, InDotted) => Err(NotPossible(Header))
			(Header, _) => Ok(Header)
			(Dotted, InInline) | (Dotted, InImplicit) => Err(NotPossible(Dotted))
			(Dotted, _) => Ok(Dotted)
		}

	## A table's sections at `path`: its own `[header]` with its scalars,
	## arrays and inline values (left out when it has none but has sections
	## below), then its sub-tables' sections, then its arrays of tables'.
	table_blocks : List(Segment), List((Str, TomlValue.Value)) -> List(Block)
	table_blocks = |path, entries| {
		parts = section_parts(path, entries)
		if parts.own.is_empty() and !parts.below.is_empty() { parts.below } else { [{ path, is_array: False, entries: parts.own }].concat(parts.below) }
	}

	## `[[x]]` sections for `items` at `path`, numbered from `start`; each
	## element's header is written even when it holds no key/values.
	element_blocks : List(Segment), List(TomlValue.Value), U64 -> List(Block)
	element_blocks = |path, items, start|
		items
		.map_with_index(|item, position| {
			element = path.append(Index(start + position))
			parts = section_parts(element, table_entries(item))
			[{ path: element, is_array: True, entries: parts.own }].concat(parts.below)
		})
		.join()

	## Dotted keys for tables below `prefix`: every non-empty table's entries
	## spelled through it; an empty table stays `{}`.
	flattened : List(Str), List((Str, TomlValue.Value)) -> List(Entry)
	flattened = |prefix, entries|
		entries
		.map(|(name, value)|
			match value {
				Table(inner) if !inner.is_empty() => TomlPlan.flattened(prefix.append(name), inner)
				_ => [(prefix.append(name), value)]
			})
		.join()

	## A non-empty array holding only tables.
	is_table_array : List(TomlValue.Value) -> Bool
	is_table_array = |items| !items.is_empty() and items.all(is_table)
}

## A table's key/value entries, and the sections of its sub-tables and then
## its arrays of tables.
section_parts : List(EncodingPath.Segment), List((Str, TomlValue.Value)) -> { own : List(TomlPlan.Entry), below : List(TomlPlan.Block) }
section_parts = |path, entries| {
	own = entries.keep_if(|(_, value)| !is_section(value)).map(|(name, value)| ([name], value))
	tables = entries.map(|(name, value)| sub_table_blocks(path.append(Key(name)), value)).join()
	arrays = entries.map(|(name, value)| table_array_blocks(path.append(Key(name)), value)).join()
	{ own, below: tables.concat(arrays) }
}

sub_table_blocks : List(EncodingPath.Segment), TomlValue.Value -> List(TomlPlan.Block)
sub_table_blocks = |path, value|
	match value {
		Table(entries) => TomlPlan.table_blocks(path, entries)
		_ => []
	}

table_array_blocks : List(EncodingPath.Segment), TomlValue.Value -> List(TomlPlan.Block)
table_array_blocks = |path, value|
	match value {
		Array(items) if TomlPlan.is_table_array(items) => TomlPlan.element_blocks(path, items, 0)
		_ => []
	}

table_entries : TomlValue.Value -> List((Str, TomlValue.Value))
table_entries = |value|
	match value {
		Table(entries) => entries
		_ => []
	}

is_table : TomlValue.Value -> Bool
is_table = |value|
	match value {
		Table(_) => True
		_ => False
	}

is_section : TomlValue.Value -> Bool
is_section = |value|
	match value {
		Table(_) => True
		Array(items) => TomlPlan.is_table_array(items)
		_ => False
	}

can_hold_header : TomlPlan.Context -> Bool
can_hold_header = |context| context != InInline and context != InDotted

## An array of tables is `[[x]]` sections by `Auto` except inside inline and
## dotted tables; `Inline` and `Dotted` write it as an array value.
array_choice : TomlPlan.Context, TomlLocate.Style -> Try([Inline, Header], [NotPossible([Header, Dotted])])
array_choice = |context, style|
	match (style, context) {
		(Header, InInline) | (Header, InDotted) => Err(NotPossible(Header))
		(Header, _) => Ok(Header)
		(Auto, InInline) | (Auto, InDotted) => Ok(Inline)
		(Auto, _) => Ok(Header)
		_ => Ok(Inline)
	}

last_key : List(EncodingPath.Segment) -> Str
last_key = |path|
	match path.last() {
		Ok(Key(name)) => name
		_ => ""
	}

## `names` wrapped around `value` as nested tables.
nested : List(Str), TomlValue.Value -> TomlValue.Value
nested = |names, value| names.fold_rev(value, |name, inner| Table([(name, inner)]))

prefixed : Str, TomlPlan.Plan -> TomlPlan.Plan
prefixed = |name, plan|
	match plan {
		Entries(entries) => Entries(entries.map(|(keys, value)| ([name].concat(keys), value)))
		Blocks(blocks) => Blocks(blocks)
	}

## A table created under a header: its entries in a section of its own, or
## only the sections below it.
under_header : List(EncodingPath.Segment), TomlPlan.Plan -> TomlPlan.Plan
under_header = |path, plan|
	match plan {
		Entries(entries) => Blocks([{ path, is_array: False, entries }])
		Blocks(blocks) => Blocks(blocks)
	}
