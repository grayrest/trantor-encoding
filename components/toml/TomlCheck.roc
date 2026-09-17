import EncodingDate
import EncodingPath
import TomlDate
import TomlProblem
import TomlValue

## What writing refuses, found before a byte is written: a root that is not a
## table, a key twice in one table, nesting past 128 levels, and dates, times
## and offsets TOML cannot hold (years 0–9999, ISO calendar days, no second 60,
## sub-second fields up to 999, offsets within ±23:59).
##
## Problems are reported in document order, the first one found.
TomlCheck :: [].{

	## `Toml.EncodeErr`.
	EncodeErr : [
		InvalidDate({ path : List(EncodingPath.Segment), date : EncodingDate.Date }),
		InvalidTime({ path : List(EncodingPath.Segment), time : EncodingDate.Time }),
		InvalidOffset({ path : List(EncodingPath.Segment), offset : EncodingDate.Offset }),
		IntegerOutOfRange({ path : List(EncodingPath.Segment), value : Str }),
		RootNotATable,
		DuplicateKey(List(EncodingPath.Segment)),
		TooDeep(List(EncodingPath.Segment)),
	]

	## `value` found at `path`, a table or array there opening level
	## `path.len() + 1`.
	value_at : TomlValue.Value, List(EncodingPath.Segment) -> Try({}, EncodeErr)
	value_at = |value, path|
		match value {
			Table(entries) => {
				TomlCheck.container_at(path)?
				entries_at(entries, path)
			}
			Array(items) => {
				TomlCheck.container_at(path)?
				items_at(items, path)
			}
			LocalDate(date) => TomlCheck.date_at(date, path)
			LocalTime(time) => TomlCheck.time_at(time, path)
			LocalDatetime(moment) => {
				TomlCheck.date_at(moment.date, path)?
				TomlCheck.time_at(moment.time, path)
			}
			OffsetDatetime(moment) => {
				TomlCheck.date_at(moment.date, path)?
				TomlCheck.time_at(moment.time, path)?
				TomlCheck.offset_at(moment.offset, path)
			}
			_ => Ok({})
		}

	## A table or array at `path` within the nesting limit.
	container_at : List(EncodingPath.Segment) -> Try({}, EncodeErr)
	container_at = |path|
		if path.len() >= TomlProblem.max_depth {
			Err(TooDeep(path))
		} else {
			Ok({})
		}

	date_at : EncodingDate.Date, List(EncodingPath.Segment) -> Try({}, EncodeErr)
	date_at = |date, path|
		if EncodingDate.is_valid_date(date, TomlDate.limits) {
			Ok({})
		} else {
			Err(InvalidDate({ path, date }))
		}

	time_at : EncodingDate.Time, List(EncodingPath.Segment) -> Try({}, EncodeErr)
	time_at = |time, path|
		if EncodingDate.is_valid_time(time) {
			Ok({})
		} else {
			Err(InvalidTime({ path, time }))
		}

	offset_at : EncodingDate.Offset, List(EncodingPath.Segment) -> Try({}, EncodeErr)
	offset_at = |offset, path|
		if EncodingDate.is_valid_offset(offset, TomlDate.limits) {
			Ok({})
		} else {
			Err(InvalidOffset({ path, offset }))
		}
}

## Each entry in order: its key new to the table, then its value.
entries_at : List((Str, TomlValue.Value)), List(EncodingPath.Segment) -> Try({}, TomlCheck.EncodeErr)
entries_at = |entries, path|
	entries
	.fold_try(Set.empty(), |seen, (name, value)| {
		entry_path = path.append(Key(name))
		if seen.contains(name) {
			Err(DuplicateKey(entry_path))
		} else {
			TomlCheck.value_at(value, entry_path)?
			Ok(seen.insert(name))
		}
	})
	.map_ok(|_| {})

items_at : List(TomlValue.Value), List(EncodingPath.Segment) -> Try({}, TomlCheck.EncodeErr)
items_at = |items, path|
	items
	.map_with_index(|item, index| (item, index))
	.fold_try({}, |_, (item, index)| TomlCheck.value_at(item, path.append(Index(index))))
