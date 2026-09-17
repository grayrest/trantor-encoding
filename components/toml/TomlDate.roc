import EncodingDate

## TOML's RFC 3339 grammar: `YYYY-MM-DD`, `hh:mm[:ss[.fraction]]`, the two
## joined by `T`, `t` or a space, and an offset `Z`, `z` or `±hh:mm`.
##
## Seconds may be omitted (TOML 1.1). Fraction digits past nanoseconds are
## cut off. A well-formed literal whose fields are out of range (month 13,
## February 30, hour 24, second 60, an offset past ±23:59) is `OutOfRange`;
## second 60 is refused deliberately (D-S3-37.5).
TomlDate :: [].{

	Moment : [
		OffsetDatetime({ date : EncodingDate.Date, time : EncodingDate.Time, offset : EncodingDate.Offset }),
		LocalDatetime({ date : EncodingDate.Date, time : EncodingDate.Time }),
		LocalDate(EncodingDate.Date),
		LocalTime(EncodingDate.Time),
	]

	## `OutOfRange` carries the index just past the literal.
	Problem : [Invalid({ at : U64, expected : Str }), OutOfRange(U64)]

	## TOML's years and offsets.
	limits : EncodingDate.Limits
	limits = { min_year: 0, max_year: 9999, max_offset_minutes: 23 * 60 + 59 }

	## Whether a date or a time starts at `index` (four digits and `-`, or two
	## digits and `:`), as opposed to a number.
	starts_at : List(U8), U64 -> Bool
	starts_at = |bytes, index|
		digits_then(bytes, index, year_digits, '-') or digits_then(bytes, index, field_digits, ':')

	## The date or time starting at `index`, and the index just past it.
	read : List(U8), U64 -> Try({ moment : Moment, next : U64 }, Problem)
	read = |bytes, index| {
		cursor = { bytes, index }
		reading =
			if digits_then(bytes, index, year_digits, '-') {
				date_and_more(cursor)?
			} else {
				time = time_at(cursor)?
				{ moment: LocalTime(time.value), next: time.rest.index }
			}
		if is_in_range(reading.moment) {
			Ok(reading)
		} else {
			Err(OutOfRange(reading.next))
		}
	}
}

year_digits : U64
year_digits = 4

field_digits : U64
field_digits = 2

expected_digits : Str
expected_digits = "two digits"

digits_then : List(U8), U64, U64, U8 -> Bool
digits_then = |bytes, index, count, separator|
	bytes.sublist({ start: index, len: count }).all(|byte| byte >= '0' and byte <= '9')
	and bytes.len() >= index + count
	and bytes.get(index + count) == Ok(separator)

## A date, then a time when `T`/`t` follows, or a space and a time.
date_and_more : EncodingDate.Cursor -> Try({ moment : TomlDate.Moment, next : U64 }, TomlDate.Problem)
date_and_more = |cursor| {
	date = date_at(cursor)?
	after = date.rest
	delimiter = after.bytes.get(after.index)
	is_joined =
		delimiter == Ok('T')
		or delimiter == Ok('t')
		or (delimiter == Ok(' ') and digits_then(after.bytes, after.index + 1, field_digits, ':'))
	if is_joined {
		time = time_at({ ..after, index: after.index + 1 })?
		offset_and_end(date.value, time.value, time.rest)
	} else {
		Ok({ moment: LocalDate(date.value), next: after.index })
	}
}

offset_and_end : EncodingDate.Date, EncodingDate.Time, EncodingDate.Cursor -> Try({ moment : TomlDate.Moment, next : U64 }, TomlDate.Problem)
offset_and_end = |date, time, cursor|
	match cursor.bytes.get(cursor.index) {
		Ok('Z') | Ok('z') => Ok({ moment: OffsetDatetime({ date, time, offset: { minutes: 0 } }), next: cursor.index + 1 })
		Ok('+') | Ok('-') => {
			offset = offset_at(cursor)?
			Ok({ moment: OffsetDatetime({ date, time, offset: offset.value }), next: offset.rest.index })
		}
		_ => Ok({ moment: LocalDatetime({ date, time }), next: cursor.index })
	}

date_at : EncodingDate.Cursor -> Try({ value : EncodingDate.Date, rest : EncodingDate.Cursor }, TomlDate.Problem)
date_at = |cursor| {
	year = digits(cursor, year_digits, "four digits")?
	month = digits(separator(year.rest, '-')?, field_digits, expected_digits)?
	day = digits(separator(month.rest, '-')?, field_digits, expected_digits)?
	Ok({
		value: { year: U64.to_i32_wrap(year.value), month: U64.to_u8_wrap(month.value), day: U64.to_u8_wrap(day.value) },
		rest: day.rest,
	})
}

## `hh:mm`, then `:ss` and a fraction if present.
time_at : EncodingDate.Cursor -> Try({ value : EncodingDate.Time, rest : EncodingDate.Cursor }, TomlDate.Problem)
time_at = |cursor| {
	hour = digits(cursor, field_digits, expected_digits)?
	minute = digits(separator(hour.rest, ':')?, field_digits, expected_digits)?
	seconds =
		if minute.rest.bytes.get(minute.rest.index) == Ok(':') {
			seconds_at({ ..minute.rest, index: minute.rest.index + 1 })?
		} else {
			{ second: 0, fraction: [], rest: minute.rest }
		}
	fraction = EncodingDate.fraction_from_digits(seconds.fraction)
	Ok({
		value: {
			hour: U64.to_u8_wrap(hour.value),
			minute: U64.to_u8_wrap(minute.value),
			second: seconds.second,
			millisecond: fraction.millisecond,
			microsecond: fraction.microsecond,
			nanosecond: fraction.nanosecond,
		},
		rest: seconds.rest,
	})
}

## `ss` and `.digits` if a `.` follows.
seconds_at : EncodingDate.Cursor -> Try({ second : U8, fraction : List(U8), rest : EncodingDate.Cursor }, TomlDate.Problem)
seconds_at = |cursor| {
	second = digits(cursor, field_digits, expected_digits)?
	if second.rest.bytes.get(second.rest.index) == Ok('.') {
		run = EncodingDate.digit_run({ ..second.rest, index: second.rest.index + 1 })
		if run.digits.is_empty() {
			Err(Invalid({ at: run.rest.index, expected: "fraction digits" }))
		} else {
			Ok({ second: U64.to_u8_wrap(second.value), fraction: run.digits, rest: run.rest })
		}
	} else {
		Ok({ second: U64.to_u8_wrap(second.value), fraction: [], rest: second.rest })
	}
}

## `+hh:mm` or `-hh:mm` at a sign.
offset_at : EncodingDate.Cursor -> Try({ value : EncodingDate.Offset, rest : EncodingDate.Cursor }, TomlDate.Problem)
offset_at = |cursor| {
	is_negative = cursor.bytes.get(cursor.index) == Ok('-')
	hours = digits({ ..cursor, index: cursor.index + 1 }, field_digits, expected_digits)?
	minutes = digits(separator(hours.rest, ':')?, field_digits, expected_digits)?
	match EncodingDate.offset_from_clock(is_negative, hours.value, minutes.value, TomlDate.limits) {
		Ok(offset) => Ok({ value: offset, rest: minutes.rest })
		Err(_) => Err(OutOfRange(minutes.rest.index))
	}
}

is_in_range : TomlDate.Moment -> Bool
is_in_range = |moment|
	match moment {
		OffsetDatetime(parts) => is_valid_date(parts.date) and EncodingDate.is_valid_time(parts.time)
		LocalDatetime(parts) => is_valid_date(parts.date) and EncodingDate.is_valid_time(parts.time)
		LocalDate(day) => is_valid_date(day)
		LocalTime(clock) => EncodingDate.is_valid_time(clock)
	}

is_valid_date : EncodingDate.Date -> Bool
is_valid_date = |day| EncodingDate.is_valid_date(day, TomlDate.limits)

digits : EncodingDate.Cursor, U64, Str -> Try({ value : U64, rest : EncodingDate.Cursor }, TomlDate.Problem)
digits = |cursor, count, expected|
	EncodingDate.fixed_digits(cursor, count).map_err(|Unexpected(at)| Invalid({ at, expected }))

separator : EncodingDate.Cursor, U8 -> Try(EncodingDate.Cursor, TomlDate.Problem)
separator = |cursor, byte|
	EncodingDate.expect_byte(cursor, byte).map_err(|Unexpected(at)| Invalid({ at, expected: Str.from_utf8_lossy([byte]) }))

reads : Str -> Try(TomlDate.Moment, TomlDate.Problem)
reads = |text| {
	bytes = text.to_utf8()
	read = TomlDate.read(bytes, 0)?
	if read.next == bytes.len() { Ok(read.moment) } else { Err(Invalid({ at: read.next, expected: "the end" })) }
}

day_of : I32, U8, U8 -> EncodingDate.Date
day_of = |year, month, day| { year, month, day }

clock_of : U8, U8, U8, U16, U16, U16 -> EncodingDate.Time
clock_of = |hour, minute, second, millisecond, microsecond, nanosecond| { hour, minute, second, millisecond, microsecond, nanosecond }

# Each kind, with `T`, `t`, a space, `Z`, `z` and numeric offsets.
expect reads("1979-05-27") == Ok(LocalDate(day_of(1979, 5, 27)))
expect reads("07:32:00") == Ok(LocalTime(clock_of(7, 32, 0, 0, 0, 0)))
expect reads("1979-05-27T07:32:00") == Ok(LocalDatetime({ date: day_of(1979, 5, 27), time: clock_of(7, 32, 0, 0, 0, 0) }))
expect reads("1979-05-27 07:32:00Z") == Ok(OffsetDatetime({ date: day_of(1979, 5, 27), time: clock_of(7, 32, 0, 0, 0, 0), offset: { minutes: 0 } }))
expect reads("1979-05-27t07:32:00z") == Ok(OffsetDatetime({ date: day_of(1979, 5, 27), time: clock_of(7, 32, 0, 0, 0, 0), offset: { minutes: 0 } }))
expect reads("1979-05-27T00:32:00-07:00") == Ok(OffsetDatetime({ date: day_of(1979, 5, 27), time: clock_of(0, 32, 0, 0, 0, 0), offset: { minutes: -420 } }))
expect reads("1979-05-27T00:32:00+23:59") == Ok(OffsetDatetime({ date: day_of(1979, 5, 27), time: clock_of(0, 32, 0, 0, 0, 0), offset: { minutes: 1439 } }))

# A space not followed by a time ends a local date.
expect TomlDate.read("1979-05-27 # c".to_utf8(), 0) == Ok({ moment: LocalDate(day_of(1979, 5, 27)), next: 10 })

# Seconds omitted; fractions cut after nanoseconds.
expect reads("07:32") == Ok(LocalTime(clock_of(7, 32, 0, 0, 0, 0)))
expect reads("1979-05-27 07:32-07:00") == Ok(OffsetDatetime({ date: day_of(1979, 5, 27), time: clock_of(7, 32, 0, 0, 0, 0), offset: { minutes: -420 } }))
expect reads("00:32:00.5") == Ok(LocalTime(clock_of(0, 32, 0, 500, 0, 0)))
expect reads("00:32:00.1234567899") == Ok(LocalTime(clock_of(0, 32, 0, 123, 456, 789)))

# Out of range: calendar, clock, second 60, offsets.
expect reads("2021-02-29") == Err(OutOfRange(10))
expect reads("2000-02-29") == Ok(LocalDate(day_of(2000, 2, 29)))
expect reads("0000-01-01") == Ok(LocalDate(day_of(0, 1, 1)))
expect reads("2021-13-01") == Err(OutOfRange(10))
expect reads("2021-01-00") == Err(OutOfRange(10))
expect reads("24:00:00") == Err(OutOfRange(8))
expect reads("23:60:00") == Err(OutOfRange(8))
expect reads("23:59:60") == Err(OutOfRange(8))
expect reads("1979-05-27T00:32:00+24:00") == Err(OutOfRange(25))
expect reads("1979-05-27T00:32:00-00:60") == Err(OutOfRange(25))

# Malformed at the offending byte.
expect reads("1979-5-27") == Err(Invalid({ at: 6, expected: expected_digits }))
expect reads("1979-05-27T7:32") == Err(Invalid({ at: 12, expected: expected_digits }))
expect reads("07:32:00.") == Err(Invalid({ at: 9, expected: "fraction digits" }))
expect reads("07:32.5") == Err(Invalid({ at: 5, expected: "the end" }))
expect reads("1979-05-27T07:32:00+07") == Err(Invalid({ at: 22, expected: ":" }))
expect TomlDate.starts_at("1979-".to_utf8(), 0)
expect TomlDate.starts_at("07:".to_utf8(), 0)
expect !TomlDate.starts_at("1979".to_utf8(), 0)
expect !TomlDate.starts_at("1e5".to_utf8(), 0)
