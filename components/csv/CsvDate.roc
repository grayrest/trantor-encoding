import EncodingDate
import EncodingPath

## XML Schema 1.1's date and time forms for CSV cells (D-S3-51):
##
## - `date` `-?YYYY-MM-DD`, a year of four digits or more (more than four with
##   no leading zero), negative and `0000` allowed, any `I32`;
## - `time` `hh:mm:ss` and an optional fraction, cut after nanoseconds;
## - `dateTime` a date, `T` and a time;
## - a timezone `Z` or `±hh:mm` within ±14:00 (`-00:00` is zero), which
##   separates offset date-times from local ones.
##
## `T` and `Z` are uppercase and seconds are required. Second 60 and `24:00:00`
## (XML Schema's end-of-day form) are refused. Writing takes the extended form
## for years outside 0–9999, trims the fraction and writes `Z` for offset zero.
CsvDate :: [].{

	## Writing failures, each at `[Index(record), Key(column)]`.
	EncodeErr : [
		InvalidDate({ path : List(EncodingPath.Segment), date : EncodingDate.Date }),
		InvalidTime({ path : List(EncodingPath.Segment), time : EncodingDate.Time }),
		InvalidOffset({ path : List(EncodingPath.Segment), offset : EncodingDate.Offset }),
	]

	LocalDatetime : { date : EncodingDate.Date, time : EncodingDate.Time }

	OffsetDatetime : { date : EncodingDate.Date, time : EncodingDate.Time, offset : EncodingDate.Offset }

	## CSV's years and offsets.
	limits : EncodingDate.Limits
	limits = { min_year: I32.lowest, max_year: I32.highest, max_offset_minutes: max_offset_hours * minutes_per_hour }

	local_date : Str -> Try(EncodingDate.Date, [Invalid])
	local_date = |text| whole(text, date_at)

	local_time : Str -> Try(EncodingDate.Time, [Invalid])
	local_time = |text| whole(text, time_at)

	local_datetime : Str -> Try(LocalDatetime, [Invalid])
	local_datetime = |text| whole(text, local_datetime_at)

	offset_datetime : Str -> Try(OffsetDatetime, [Invalid])
	offset_datetime = |text| whole(text, offset_datetime_at)

	date_text : EncodingDate.Date, List(EncodingPath.Segment) -> Try(Str, EncodeErr)
	date_text = |date, path|
		if EncodingDate.is_valid_date(date, CsvDate.limits) {
			Ok(EncodingDate.date_text(date))
		} else {
			Err(InvalidDate({ path, date }))
		}

	time_text : EncodingDate.Time, List(EncodingPath.Segment) -> Try(Str, EncodeErr)
	time_text = |time, path|
		if EncodingDate.is_valid_time(time) {
			Ok(EncodingDate.time_text(time))
		} else {
			Err(InvalidTime({ path, time }))
		}

	## The date is checked before the time.
	local_datetime_text : LocalDatetime, List(EncodingPath.Segment) -> Try(Str, EncodeErr)
	local_datetime_text = |moment, path| {
		date = CsvDate.date_text(moment.date, path)?
		time = CsvDate.time_text(moment.time, path)?
		Ok("${date}T${time}")
	}

	## The date is checked before the time, the time before the offset.
	offset_datetime_text : OffsetDatetime, List(EncodingPath.Segment) -> Try(Str, EncodeErr)
	offset_datetime_text = |moment, path| {
		local = CsvDate.local_datetime_text({ date: moment.date, time: moment.time }, path)?
		if EncodingDate.is_valid_offset(moment.offset, CsvDate.limits) {
			Ok("${local}${EncodingDate.offset_text(moment.offset)}")
		} else {
			Err(InvalidOffset({ path, offset: moment.offset }))
		}
	}
}

max_offset_hours : I16
max_offset_hours = 14

minutes_per_hour : I16
minutes_per_hour = 60

year_digits : U64
year_digits = 4

## An `I32` has at most ten digits.
max_year_digits : U64
max_year_digits = 10

field_digits : U64
field_digits = 2

decimal_base : I64
decimal_base = 10

Read(v) : Try({ value : v, rest : EncodingDate.Cursor }, [Invalid])

## `read` over all of `text`.
whole : Str, (EncodingDate.Cursor -> Read(v)) -> Try(v, [Invalid])
whole = |text, read| {
	parsed = read({ bytes: text.to_utf8(), index: 0 })?
	if EncodingDate.is_at_end(parsed.rest) { Ok(parsed.value) } else { Err(Invalid) }
}

date_at : EncodingDate.Cursor -> Read(EncodingDate.Date)
date_at = |cursor| {
	year = year_at(cursor)?
	month = two_digits(byte_after(year.rest, '-')?)?
	day = two_digits(byte_after(month.rest, '-')?)?
	date = { year: year.value, month: I64.to_u8_wrap(month.value), day: I64.to_u8_wrap(day.value) }
	if EncodingDate.is_valid_date(date, CsvDate.limits) {
		Ok({ value: date, rest: day.rest })
	} else {
		Err(Invalid)
	}
}

## `-?` and four digits, or more than four without a leading zero.
year_at : EncodingDate.Cursor -> Read(I32)
year_at = |cursor| {
	is_negative = cursor.bytes.get(cursor.index) == Ok('-')
	run = EncodingDate.digit_run({ ..cursor, index: if is_negative { cursor.index + 1 } else { cursor.index } })
	count = run.digits.len()
	has_padding = count > year_digits and run.digits.first() == Ok(0)
	if count < year_digits or count > max_year_digits or has_padding {
		Err(Invalid)
	} else {
		size = run.digits.fold(0, |acc, digit| acc * decimal_base + U8.to_i64(digit))
		year = if is_negative { -size } else { size }
		I64.to_i32_try(year).map_ok(|value| { value, rest: run.rest }).map_err(|_| Invalid)
	}
}

## `hh:mm:ss` and `.digits` if a `.` follows.
time_at : EncodingDate.Cursor -> Read(EncodingDate.Time)
time_at = |cursor| {
	hour = two_digits(cursor)?
	minute = two_digits(byte_after(hour.rest, ':')?)?
	second = two_digits(byte_after(minute.rest, ':')?)?
	fraction = fraction_at(second.rest)?
	time = {
		hour: I64.to_u8_wrap(hour.value),
		minute: I64.to_u8_wrap(minute.value),
		second: I64.to_u8_wrap(second.value),
		millisecond: fraction.value.millisecond,
		microsecond: fraction.value.microsecond,
		nanosecond: fraction.value.nanosecond,
	}
	if EncodingDate.is_valid_time(time) {
		Ok({ value: time, rest: fraction.rest })
	} else {
		Err(Invalid)
	}
}

fraction_at : EncodingDate.Cursor -> Read(EncodingDate.Fraction)
fraction_at = |cursor|
	if cursor.bytes.get(cursor.index) == Ok('.') {
		run = EncodingDate.digit_run({ ..cursor, index: cursor.index + 1 })
		if run.digits.is_empty() {
			Err(Invalid)
		} else {
			Ok({ value: EncodingDate.fraction_from_digits(run.digits), rest: run.rest })
		}
	} else {
		Ok({ value: EncodingDate.fraction_from_digits([]), rest: cursor })
	}

local_datetime_at : EncodingDate.Cursor -> Read(CsvDate.LocalDatetime)
local_datetime_at = |cursor| {
	date = date_at(cursor)?
	time = time_at(byte_after(date.rest, 'T')?)?
	Ok({ value: { date: date.value, time: time.value }, rest: time.rest })
}

offset_datetime_at : EncodingDate.Cursor -> Read(CsvDate.OffsetDatetime)
offset_datetime_at = |cursor| {
	local = local_datetime_at(cursor)?
	offset = timezone_at(local.rest)?
	Ok({ value: { date: local.value.date, time: local.value.time, offset: offset.value }, rest: offset.rest })
}

## `Z`, or a sign and `hh:mm` within ±14:00.
timezone_at : EncodingDate.Cursor -> Read(EncodingDate.Offset)
timezone_at = |cursor|
	match cursor.bytes.get(cursor.index) {
		Ok('Z') => Ok({ value: { minutes: 0 }, rest: { ..cursor, index: cursor.index + 1 } })
		Ok('+') | Ok('-') => {
			is_negative = cursor.bytes.get(cursor.index) == Ok('-')
			hours = two_digits({ ..cursor, index: cursor.index + 1 })?
			minutes = two_digits(byte_after(hours.rest, ':')?)?
			match EncodingDate.offset_from_clock(is_negative, I64.to_u64_wrap(hours.value), I64.to_u64_wrap(minutes.value), CsvDate.limits) {
				Ok(offset) => Ok({ value: offset, rest: minutes.rest })
				Err(_) => Err(Invalid)
			}
		}
		_ => Err(Invalid)
	}

two_digits : EncodingDate.Cursor -> Read(I64)
two_digits = |cursor|
	match EncodingDate.fixed_digits(cursor, field_digits) {
		Ok(read) => Ok({ value: U64.to_i64_wrap(read.value), rest: read.rest })
		Err(_) => Err(Invalid)
	}

byte_after : EncodingDate.Cursor, U8 -> Try(EncodingDate.Cursor, [Invalid])
byte_after = |cursor, byte| EncodingDate.expect_byte(cursor, byte).map_err(|_| Invalid)

day_of : I32, U8, U8 -> EncodingDate.Date
day_of = |year, month, day| { year, month, day }

clock_of : U8, U8, U8, U16, U16, U16 -> EncodingDate.Time
clock_of = |hour, minute, second, millisecond, microsecond, nanosecond| { hour, minute, second, millisecond, microsecond, nanosecond }

noon : EncodingDate.Time
noon = clock_of(12, 0, 0, 0, 0, 0)

# Each form.
expect CsvDate.local_date("2026-03-08") == Ok(day_of(2026, 3, 8))
expect CsvDate.local_time("07:32:05") == Ok(clock_of(7, 32, 5, 0, 0, 0))
expect CsvDate.local_datetime("2026-03-08T12:00:00") == Ok({ date: day_of(2026, 3, 8), time: noon })
expect CsvDate.offset_datetime("2026-03-08T12:00:00Z") == Ok({ date: day_of(2026, 3, 8), time: noon, offset: { minutes: 0 } })
expect CsvDate.offset_datetime("2026-03-08T12:00:00-08:00") == Ok({ date: day_of(2026, 3, 8), time: noon, offset: { minutes: -480 } })
expect CsvDate.offset_datetime("2026-03-08T12:00:00+14:00") == Ok({ date: day_of(2026, 3, 8), time: noon, offset: { minutes: 840 } })
expect CsvDate.offset_datetime("2026-03-08T12:00:00-00:00") == Ok({ date: day_of(2026, 3, 8), time: noon, offset: { minutes: 0 } })

# Extended, negative and zero years; I32's bounds.
expect CsvDate.local_date("10000-01-01") == Ok(day_of(10000, 1, 1))
expect CsvDate.local_date("-0044-03-15") == Ok(day_of(-44, 3, 15))
expect CsvDate.local_date("0000-02-29") == Ok(day_of(0, 2, 29))
expect CsvDate.local_date("-2147483648-01-01") == Ok(day_of(I32.lowest, 1, 1))
expect CsvDate.local_date("2147483647-12-31") == Ok(day_of(I32.highest, 12, 31))

# Fractions: cut after nanoseconds.
expect CsvDate.local_time("00:00:00.5") == Ok(clock_of(0, 0, 0, 500, 0, 0))
expect CsvDate.local_time("00:00:00.1234567899") == Ok(clock_of(0, 0, 0, 123, 456, 789))

# Refused: timezones on local forms, none on offset ones, the lowercase and
# RFC 3339 variants, `24:00:00`, second 60, calendar and range errors.
expect ["2026-03-08Z", "2026-03-08+01:00", "2026-3-08", "26-03-08", "02026-03-08", "2147483648-01-01", "-2147483649-01-01", "12345678901-01-01", "2023-02-29", "2026-13-01", "2026-03-08 "].all(|text| CsvDate.local_date(text) == Err(Invalid))
expect ["07:32", "07:32:00Z", "07:32:00-05:00", "24:00:00", "23:59:60", "23:60:00", "07:32:00.", "7:32:00"].all(|text| CsvDate.local_time(text) == Err(Invalid))
expect ["2026-03-08t12:00:00", "2026-03-08 12:00:00", "2026-03-08T12:00", "2026-03-08T12:00:00Z", "2026-03-08"].all(|text| CsvDate.local_datetime(text) == Err(Invalid))
expect ["2026-03-08T12:00:00", "2026-03-08T12:00:00z", "2026-03-08T12:00:00+14:01", "2026-03-08T12:00:00-15:00", "2026-03-08T12:00:00+01:60", "2026-03-08T12:00:00+0100", "2026-03-08T24:00:00Z"].all(|text| CsvDate.offset_datetime(text) == Err(Invalid))

# Writing: extended years, trimmed fractions, `Z` for zero.
expect CsvDate.date_text(day_of(10000, 1, 1), []) == Ok("10000-01-01")
expect CsvDate.date_text(day_of(-44, 3, 15), []) == Ok("-0044-03-15")
expect CsvDate.time_text(clock_of(7, 32, 0, 120, 0, 0), []) == Ok("07:32:00.12")
expect CsvDate.local_datetime_text({ date: day_of(2026, 3, 8), time: noon }, []) == Ok("2026-03-08T12:00:00")
expect CsvDate.offset_datetime_text({ date: day_of(2026, 3, 8), time: noon, offset: { minutes: 0 } }, []) == Ok("2026-03-08T12:00:00Z")
expect CsvDate.offset_datetime_text({ date: day_of(2026, 3, 8), time: noon, offset: { minutes: -840 } }, []) == Ok("2026-03-08T12:00:00-14:00")

# Writing failures, the date before the time before the offset.
expect CsvDate.date_text(day_of(2023, 2, 29), [Key("d")]) == Err(InvalidDate({ path: [Key("d")], date: day_of(2023, 2, 29) }))
expect CsvDate.time_text(clock_of(24, 0, 0, 0, 0, 0), []) == Err(InvalidTime({ path: [], time: clock_of(24, 0, 0, 0, 0, 0) }))
expect CsvDate.offset_datetime_text({ date: day_of(2026, 3, 8), time: noon, offset: { minutes: 841 } }, []) == Err(InvalidOffset({ path: [], offset: { minutes: 841 } }))
expect CsvDate.offset_datetime_text({ date: day_of(2026, 2, 30), time: clock_of(0, 0, 60, 0, 0, 0), offset: { minutes: 841 } }, []) == Err(InvalidDate({ path: [], date: day_of(2026, 2, 30) }))
expect CsvDate.offset_datetime_text({ date: day_of(2026, 2, 28), time: clock_of(0, 0, 60, 0, 0, 0), offset: { minutes: 841 } }, []) == Err(InvalidTime({ path: [], time: clock_of(0, 0, 60, 0, 0, 0) }))
