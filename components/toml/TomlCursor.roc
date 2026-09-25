import EncodingPath
import TomlValue

## Where typed reading is in a value: the value under the cursor, a frame per
## table or array being walked, and the path to the cursor, which `Mismatch`
## reports.
TomlCursor :: [].{

	## An open table's entries or an open array's items, and the next one.
	Frame : { entries : List((Str, TomlValue.Value)), items : List(TomlValue.Value), next : U64 }

	DecodeState := { current : TomlValue.Value, frames : List(Frame), path : List(EncodingPath.Segment) }.{
		key_path : DecodeState -> List(EncodingPath.Segment)
		key_path = |state| state.path
	}

	## `value` under the cursor at the root.
	start : TomlValue.Value -> DecodeState
	start = |value| { current: value, frames: [], path: [] }

	expected_table : Str
	expected_table = "a table"

	expected_array : Str
	expected_array = "an array"

	## Walking the table under the cursor.
	open_table : DecodeState -> Try([Counted({ len : U64, rest : DecodeState }), Uncounted(DecodeState)], [Mismatch({ path : List(EncodingPath.Segment), expected : Str })])
	open_table = |state|
		match state.current {
			Table(entries) => Ok(Uncounted(push(state, { entries, items: [], next: 0 })))
			_ => Err(EncodingPath.mismatch_at(state, TomlCursor.expected_table))
		}

	## Walking `items`, the array under the cursor.
	open_items : DecodeState, List(TomlValue.Value) -> DecodeState
	open_items = |state, items| push(state, { entries: [], items, next: 0 })

	## Done walking the innermost table or array.
	close : DecodeState -> DecodeState
	close = |state| { ..state, frames: state.frames.drop_last(1) }

	## Back out of the value just read.
	leave : DecodeState -> DecodeState
	leave = |state| { ..state, path: state.path.drop_last(1) }

	## The next table entry's name, its value under the cursor and its key on
	## the path; the table closed when there is none.
	next_entry : DecodeState -> [Entry({ name : Str, rest : DecodeState }), Finished(DecodeState)]
	next_entry = |state| {
		frame = top(state)
		match frame.entries.get(frame.next) {
			Ok((name, value)) => {
				moved = advanced(state)
				Entry({ name, rest: { ..moved, current: value, path: state.path.append(Key(name)) } })
			}
			Err(_) => Finished(TomlCursor.close(state))
		}
	}

	## After a dict key: the entry just taken, its value under the cursor.
	entry_value : DecodeState -> DecodeState
	entry_value = |state| {
		frame = top(state)
		match frame.entries.get(frame.next - 1) {
			Ok((name, value)) => { ..state, current: value, path: state.path.append(Key(name)) }
			Err(_) => state
		}
	}

	## The next array item under the cursor, its index on the path; the array
	## closed when there is none.
	next_item : DecodeState -> Try(DecodeState, [Finished(DecodeState)])
	next_item = |state| {
		frame = top(state)
		match frame.items.get(frame.next) {
			Ok(value) => Ok({ ..advanced(state), current: value, path: state.path.append(Index(frame.next)) })
			Err(_) => Err(Finished(TomlCursor.close(state)))
		}
	}

	## Item `index` of the innermost array under the cursor.
	tuple_item : DecodeState, U64 -> DecodeState
	tuple_item = |state, index| {
		frame = top(state)
		current = frame.items.get(index) ?? state.current
		{ ..state, current, path: state.path.append(Index(index)) }
	}
}

push : TomlCursor.DecodeState, TomlCursor.Frame -> TomlCursor.DecodeState
push = |state, frame| { ..state, frames: state.frames.append(frame) }

top : TomlCursor.DecodeState -> TomlCursor.Frame
top = |state| state.frames.last() ?? { entries: [], items: [], next: 0 }

## The innermost frame moved past its next entry or item.
## `take_first(len - 1)`, not `drop_last(1)`: `drop_last(1).append(…)` corrupts
## memory in a compiled app (a segfault in `TomlRemove.merged`).
advanced : TomlCursor.DecodeState -> TomlCursor.DecodeState
advanced = |state| {
	frame = top(state)
	kept = if state.frames.is_empty() { [] } else { state.frames.take_first(state.frames.len() - 1) }
	{ ..state, frames: kept.append({ ..frame, next: frame.next + 1 }) }
}
