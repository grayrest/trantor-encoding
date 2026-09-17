import EncodingPath
import TomlLex
import TomlProblem
import TomlValue

## Tables as a document builds them, before they become a `Value`.
##
## Tables and arrays of tables live in a flat list and refer to each other by
## index, so defining a key rewrites one slot rather than every table above
## it. Each table remembers how it was created, which decides what may extend
## it later:
##
## - `Implicit`: named on the way to a header (`a` in `[a.b]`); a header may
##   define it once.
## - `Header`: defined by `[a]` or as an `[[a]]` element; only deeper headers
##   reach into it again.
## - `Dotted`: created by a dotted key; more dotted keys and deeper headers
##   extend it, a header never defines it.
##
## Inline tables and arrays written as values are finished `Value`s and take
## nothing more.
TomlTree :: [].{

	Origin : [Implicit, Header, Dotted]

	Child : [Leaf(TomlValue.Value), Node(U64)]

	Table : {
		names : List(Str),
		children : List(Child),
		index : Dict(Str, U64),
		origin : Origin,
		level : U64,
		path : List(EncodingPath.Segment),
	}

	Slot : [
		TableSlot(Table),
		ArraySlot({ elements : List(U64), level : U64, path : List(EncodingPath.Segment) }),
		Taken,
	]

	## Where a key/value goes: the table, the key's last segment, and the
	## level and path a value under it starts from.
	Prepared : { tree : List(Slot), table : U64, name : Str, level : U64, path : List(EncodingPath.Segment) }

	## A tree holding one empty table at `level` and `path`.
	new : U64, List(EncodingPath.Segment) -> List(Slot)
	new = |level, path| [TableSlot(empty_table(Header, level, path))]

	root : U64
	root = 0

	## The table `[a.b.c]` names, defined now.
	open_table : List(Slot), List(TomlLex.KeyPart) -> Try({ tree : List(Slot), table : U64 }, TomlProblem.Problem)
	open_table = |tree, parts| {
		parent = header_parent(tree, parts)?
		last = parts.last() ?? { name: "", at: 0, next: 0 }
		match child_of(parent.tree, parent.table, last.name) {
			Missing => add_table(parent.tree, parent.table, last, Header)
			Found(Node(id)) if is_implicit(parent.tree, id) => Ok({ tree: set_origin(parent.tree, id, Header), table: id })
			_ => Err(duplicate(parent.tree, parent.table, last))
		}
	}

	## A new element of the array of tables `[[a.b.c]]` names.
	open_array_table : List(Slot), List(TomlLex.KeyPart) -> Try({ tree : List(Slot), table : U64 }, TomlProblem.Problem)
	open_array_table = |tree, parts| {
		parent = header_parent(tree, parts)?
		last = parts.last() ?? { name: "", at: 0, next: 0 }
		match child_of(parent.tree, parent.table, last.name) {
			Missing => add_array(parent.tree, parent.table, last)
			Found(Node(id)) if is_array(parent.tree, id) => add_element(parent.tree, id, last.at)
			_ => Err(duplicate(parent.tree, parent.table, last))
		}
	}

	## Walks a dotted key from `table`, creating dotted tables, and checks its
	## last segment is not yet defined.
	prepare_key : List(Slot), U64, List(TomlLex.KeyPart) -> Try(Prepared, TomlProblem.Problem)
	prepare_key = |tree, table, parts| {
		parent = parts.drop_last(1).fold_try({ tree, table }, dotted_step)?
		last = parts.last() ?? { name: "", at: 0, next: 0 }
		match child_of(parent.tree, parent.table, last.name) {
			Missing => {
				owner = table_at(parent.tree, parent.table)
				Ok({ tree: parent.tree, table: parent.table, name: last.name, level: owner.level, path: owner.path.append(Key(last.name)) })
			}
			Found(_) => Err(duplicate(parent.tree, parent.table, last))
		}
	}

	## `name = value` in `table`, which `prepare_key` found free.
	insert : List(Slot), U64, Str, TomlValue.Value -> List(Slot)
	insert = |tree, table, name, value| with_child(tree, table, name, Leaf(value))

	## The path of the table or array of tables at `id`.
	path_of : List(Slot), U64 -> List(EncodingPath.Segment)
	path_of = |tree, id|
		match tree.get(id) {
			Ok(TableSlot(table)) => table.path
			Ok(ArraySlot(array)) => array.path
			_ => []
		}

	## The table at `root` as a `Value`.
	to_value : List(Slot) -> TomlValue.Value
	to_value = |tree| node_value(tree, TomlTree.root)
}

empty_table : TomlTree.Origin, U64, List(EncodingPath.Segment) -> TomlTree.Table
empty_table = |origin, level, path| { names: [], children: [], index: Dict.empty(), origin, level, path }

table_at : List(TomlTree.Slot), U64 -> TomlTree.Table
table_at = |tree, id|
	match tree.get(id) {
		Ok(TableSlot(table)) => table
		_ => empty_table(Header, 0, [])
	}

child_of : List(TomlTree.Slot), U64, Str -> [Found(TomlTree.Child), Missing]
child_of = |tree, id, name| {
	table = table_at(tree, id)
	match table.index.get(name) {
		Ok(position) =>
			match table.children.get(position) {
				Ok(child) => Found(child)
				Err(_) => Missing
			}
		Err(_) => Missing
	}
}

is_implicit : List(TomlTree.Slot), U64 -> Bool
is_implicit = |tree, id|
	match tree.get(id) {
		Ok(TableSlot(table)) => table.origin == Implicit
		_ => False
	}

is_dotted : List(TomlTree.Slot), U64 -> Bool
is_dotted = |tree, id|
	match tree.get(id) {
		Ok(TableSlot(table)) => table.origin == Dotted
		_ => False
	}

is_array : List(TomlTree.Slot), U64 -> Bool
is_array = |tree, id|
	match tree.get(id) {
		Ok(ArraySlot(_)) => True
		_ => False
	}

duplicate : List(TomlTree.Slot), U64, TomlLex.KeyPart -> TomlProblem.Problem
duplicate = |tree, id, part| Duplicate({ at: part.at, path: table_at(tree, id).path.append(Key(part.name)) })

## Every header segment but the last: into any table, and into the last
## element of an array of tables, creating implicit tables.
header_parent : List(TomlTree.Slot), List(TomlLex.KeyPart) -> Try({ tree : List(TomlTree.Slot), table : U64 }, TomlProblem.Problem)
header_parent = |tree, parts| parts.drop_last(1).fold_try({ tree, table: TomlTree.root }, header_step)

header_step : { tree : List(TomlTree.Slot), table : U64 }, TomlLex.KeyPart -> Try({ tree : List(TomlTree.Slot), table : U64 }, TomlProblem.Problem)
header_step = |at, part|
	match child_of(at.tree, at.table, part.name) {
		Missing => add_table(at.tree, at.table, part, Implicit)
		Found(Node(id)) =>
			match at.tree.get(id) {
				Ok(ArraySlot(array)) => Ok({ tree: at.tree, table: array.elements.last() ?? id })
				_ => Ok({ tree: at.tree, table: id })
			}
		Found(Leaf(_)) => Err(duplicate(at.tree, at.table, part))
	}

## One segment of a dotted key: only into tables dotted keys created.
dotted_step : { tree : List(TomlTree.Slot), table : U64 }, TomlLex.KeyPart -> Try({ tree : List(TomlTree.Slot), table : U64 }, TomlProblem.Problem)
dotted_step = |at, part|
	match child_of(at.tree, at.table, part.name) {
		Missing => add_table(at.tree, at.table, part, Dotted)
		Found(Node(id)) if is_dotted(at.tree, id) => Ok({ tree: at.tree, table: id })
		Found(_) => Err(duplicate(at.tree, at.table, part))
	}

## A new table under `parent` named by `part`.
add_table : List(TomlTree.Slot), U64, TomlLex.KeyPart, TomlTree.Origin -> Try({ tree : List(TomlTree.Slot), table : U64 }, TomlProblem.Problem)
add_table = |tree, parent, part, origin| {
	owner = table_at(tree, parent)
	level = TomlProblem.deeper(owner.level, part.at)?
	id = tree.len()
	added = tree.append(TableSlot(empty_table(origin, level, owner.path.append(Key(part.name)))))
	Ok({ tree: with_child(added, parent, part.name, Node(id)), table: id })
}

## A new array of tables under `parent`, with its first element.
add_array : List(TomlTree.Slot), U64, TomlLex.KeyPart -> Try({ tree : List(TomlTree.Slot), table : U64 }, TomlProblem.Problem)
add_array = |tree, parent, part| {
	owner = table_at(tree, parent)
	level = TomlProblem.deeper(owner.level, part.at)?
	id = tree.len()
	added = tree.append(ArraySlot({ elements: [], level, path: owner.path.append(Key(part.name)) }))
	add_element(with_child(added, parent, part.name, Node(id)), id, part.at)
}

add_element : List(TomlTree.Slot), U64, U64 -> Try({ tree : List(TomlTree.Slot), table : U64 }, TomlProblem.Problem)
add_element = |tree, array_id, at| {
	taken = take(tree, array_id)
	match taken.slot {
		ArraySlot(array) => {
			level = TomlProblem.deeper(array.level, at)?
			id = taken.tree.len()
			element = TableSlot(empty_table(Header, level, array.path.append(Index(array.elements.len()))))
			grown = ArraySlot({ ..array, elements: array.elements.append(id) })
			Ok({ tree: put(taken.tree, array_id, grown).append(element), table: id })
		}
		other => Ok({ tree: put(taken.tree, array_id, other), table: array_id })
	}
}

set_origin : List(TomlTree.Slot), U64, TomlTree.Origin -> List(TomlTree.Slot)
set_origin = |tree, id, origin| {
	taken = take(tree, id)
	match taken.slot {
		TableSlot(table) => put(taken.tree, id, TableSlot({ ..table, origin }))
		other => put(taken.tree, id, other)
	}
}

## `table` with `name` added. Slots are taken out while they change so their
## lists stay uniquely owned and grow in place.
with_child : List(TomlTree.Slot), U64, Str, TomlTree.Child -> List(TomlTree.Slot)
with_child = |tree, id, name, child| {
	taken = take(tree, id)
	match taken.slot {
		TableSlot(table) => {
			grown = {
				..table,
				names: table.names.append(name),
				children: table.children.append(child),
				index: table.index.insert(name, table.children.len()),
			}
			put(taken.tree, id, TableSlot(grown))
		}
		other => put(taken.tree, id, other)
	}
}

## `slot` back at `id`, where `take` left `Taken`.
put : List(TomlTree.Slot), U64, TomlTree.Slot -> List(TomlTree.Slot)
put = |tree, id, slot| take_replacing(tree, id, slot).tree

## The slot at `id` and the tree with `Taken` in its place.
take : List(TomlTree.Slot), U64 -> { tree : List(TomlTree.Slot), slot : TomlTree.Slot }
take = |tree, id| take_replacing(tree, id, Taken)

take_replacing : List(TomlTree.Slot), U64, TomlTree.Slot -> { tree : List(TomlTree.Slot), slot : TomlTree.Slot }
take_replacing = |tree, id, replacement|
	match tree.replace(id, replacement) {
		Ok({ list, prev }) => { tree: list, slot: prev }
		Err(_) => crash "TomlTree: slot ${id.to_str()} does not exist"
	}

node_value : List(TomlTree.Slot), U64 -> TomlValue.Value
node_value = |tree, id|
	match tree.get(id) {
		Ok(TableSlot(table)) => Table(table.names.map2(table.children, |name, child| (name, child_value(tree, child))))
		Ok(ArraySlot(array)) => Array(array.elements.map(|element| node_value(tree, element)))
		_ => Table([])
	}

child_value : List(TomlTree.Slot), TomlTree.Child -> TomlValue.Value
child_value = |tree, child|
	match child {
		Leaf(value) => value
		Node(id) => node_value(tree, id)
	}
