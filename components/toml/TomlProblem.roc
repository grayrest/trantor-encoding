import EncodingPath

## What reading a document fails with while scanning, located by byte index;
## `TomlParse` turns the first one into a `Toml.Err` with a line and column.
TomlProblem :: [].{

	Problem : [
		## Something other than what the grammar allows at `at`.
		Syntax({ at : U64, expected : Str }),
		## A well-formed literal from `at` to `next` whose value TOML cannot hold.
		OutOfRange({ at : U64, next : U64 }),
		## A key or table defined again, or reused as another kind.
		Duplicate({ at : U64, path : List(EncodingPath.Segment) }),
		## A table or array opened past the nesting limit.
		Deep(U64),
	]

	## Nesting levels a document may reach, the root table being the first.
	max_depth : U64
	max_depth = 128

	## The level below `level`, or `Deep` at `at` past `max_depth`.
	deeper : U64, U64 -> Try(U64, Problem)
	deeper = |level, at|
		if level >= TomlProblem.max_depth {
			Err(Deep(at))
		} else {
			Ok(level + 1)
		}

	syntax : U64, Str -> Problem
	syntax = |at, expected| Syntax({ at, expected })
}
