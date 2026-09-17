## Dates and times the formats share, below their grammars.
##
## The records are the date contract's shapes. Validity is the ISO (proleptic
## Gregorian) calendar; the year range and the largest offset are the format's
## and come in as `Limits`. Reading goes through a `Cursor` over bytes; writing
## zero-pads fields, trims fraction digits and writes `Z` for offset zero. What
## separates and surrounds the fields (RFC 3339 for TOML, XML Schema 1.1 for
## CSV) stays in each format.
EncodingDate :: [].{

	Date : { year : I32, month : U8, day : U8 }

	Time : { hour : U8, minute : U8, second : U8, millisecond : U16, microsecond : U16, nanosecond : U16 }

	Offset : { minutes : I16 }

	## A format's range: the years it reads and writes, and the largest offset
	## either side of UTC in minutes.
	Limits : { min_year : I32, max_year : I32, max_offset_minutes : I16 }

	## The sub-second fields of a `Time`.
	Fraction : { millisecond : U16, microsecond : U16, nanosecond : U16 }

	## A read position in `bytes`.
	Cursor : { bytes : List(U8), index : U64 }

	is_leap_year : I32 -> Bool
	is_leap_year = |year| year % 4 == 0 and (year % 100 != 0 or year % 400 == 0)

	## The days in `month` of `year`; 0 for a month outside 1–12.
	days_in_month : I32, U8 -> U8
	days_in_month = |year, month|
		if month == february {
			if is_leap_year(year) { 29 } else { 28 }
		} else {
			month_lengths.get(U8.to_u64(month)) ?? 0
		}

	## A real calendar day with its year inside `limits`.
	is_valid_date : Date, Limits -> Bool
	is_valid_date = |date, limits|
		date.year >= limits.min_year
		and date.year <= limits.max_year
		and date.day >= 1
		and date.day <= days_in_month(date.year, date.month)

	## Hours 0–23, minutes and seconds 0–59 (no leap second, no `24:00`), each
	## sub-second field 0–999.
	is_valid_time : Time -> Bool
	is_valid_time = |time|
		time.hour <= last_hour
		and time.minute <= last_minute
		and time.second <= last_second
		and [time.millisecond, time.microsecond, time.nanosecond].all(|part| part <= last_sub_second)

	is_valid_offset : Offset, Limits -> Bool
	is_valid_offset = |offset, limits|
		offset.minutes >= -limits.max_offset_minutes and offset.minutes <= limits.max_offset_minutes

	## An offset read as a sign, hours and minutes; minutes past 59 or a total
	## past the limit are `OutOfRange`. `-00:00` is offset zero.
	offset_from_clock : Bool, U64, U64, Limits -> Try(Offset, [OutOfRange])
	offset_from_clock = |is_negative, hours, minutes, limits| {
		limit = I16.to_u64_wrap(limits.max_offset_minutes)
		if minutes > U8.to_u64(last_minute) or hours > limit // minutes_per_hour or hours * minutes_per_hour + minutes > limit {
			Err(OutOfRange)
		} else {
			size = U64.to_i16_wrap(hours * minutes_per_hour + minutes)
			Ok({ minutes: if is_negative { -size } else { size } })
		}
	}

	## Fraction digit values of any length: the first nine, missing ones zero,
	## the rest cut off.
	fraction_from_digits : List(U8) -> Fraction
	fraction_from_digits = |digits| {
		nine = digits.take_first(fraction_digits).concat(List.repeat(0, fraction_digits - digits.len().min(fraction_digits)))
		{
			millisecond: group_value(nine, 0),
			microsecond: group_value(nine, group_digits),
			nanosecond: group_value(nine, 2 * group_digits),
		}
	}

	## `value` in decimal, zero-padded to `width` digits.
	pad : U64, U64 -> Str
	pad = |value, width| {
		text = value.to_str()
		length = text.count_utf8_bytes()
		if length >= width { text } else { "${Str.repeat("0", width - length)}${text}" }
	}

	## `YYYY-MM-DD`; a negative year leads with `-`, and a year needing more
	## than four digits takes them.
	date_text : Date -> Str
	date_text = |date| {
		year = I32.to_i64(date.year)
		sign = if year < 0 { "-" } else { "" }
		"${sign}${pad(I64.to_u64_wrap(year.abs()), year_digits)}-${two_digits(U8.to_u64(date.month))}-${two_digits(U8.to_u64(date.day))}"
	}

	## `hh:mm:ss` and the fraction, if any.
	time_text : Time -> Str
	time_text = |time|
		"${two_digits(U8.to_u64(time.hour))}:${two_digits(U8.to_u64(time.minute))}:${two_digits(U8.to_u64(time.second))}${fraction_text(time)}"

	## `.` and the fraction's digits with trailing zeros removed; empty when
	## the fraction is zero.
	fraction_text : Time -> Str
	fraction_text = |time| {
		nanoseconds = (U16.to_u64(time.millisecond) * group_size + U16.to_u64(time.microsecond)) * group_size + U16.to_u64(time.nanosecond)
		if nanoseconds == 0 {
			""
		} else {
			trimmed = trim_zeros({ value: nanoseconds, width: fraction_digits })
			".${pad(trimmed.value, trimmed.width)}"
		}
	}

	## `Z` for zero, else `+hh:mm` or `-hh:mm`.
	offset_text : Offset -> Str
	offset_text = |offset| {
		minutes = I16.to_i64(offset.minutes)
		if minutes == 0 {
			"Z"
		} else {
			size = I64.to_u64_wrap(minutes.abs())
			sign = if minutes < 0 { "-" } else { "+" }
			"${sign}${two_digits(size // minutes_per_hour)}:${two_digits(size % minutes_per_hour)}"
		}
	}

	## Exactly `width` ASCII digits as a number (`width` at most 19).
	fixed_digits : Cursor, U64 -> Try({ value : U64, rest : Cursor }, [Unexpected(U64)])
	fixed_digits = |cursor, width|
		List.repeat(0, width).fold_until(Ok({ value: 0, rest: cursor }), |acc, _|
			match acc {
				Err(_) => Break(acc)
				Ok(read) =>
					match digit_at(read.rest) {
						Ok(digit) => Continue(Ok({ value: read.value * decimal_base + U8.to_u64(digit), rest: advance(read.rest) }))
						Err(problem) => Break(Err(problem))
					}
			})

	## The digit values of the ASCII digits from the cursor on, possibly none.
	digit_run : Cursor -> { digits : List(U8), rest : Cursor }
	digit_run = |cursor| {
		after = cursor.bytes.drop_first(cursor.index)
		count = after.find_first_index(|byte| !is_digit(byte)) ?? after.len()
		{
			digits: after.take_first(count).map(|byte| byte - '0'),
			rest: { ..cursor, index: cursor.index + count },
		}
	}

	## Past `byte`, or `Unexpected` at the cursor.
	expect_byte : Cursor, U8 -> Try(Cursor, [Unexpected(U64)])
	expect_byte = |cursor, byte|
		if cursor.bytes.get(cursor.index) == Ok(byte) {
			Ok(advance(cursor))
		} else {
			Err(Unexpected(cursor.index))
		}

	is_at_end : Cursor -> Bool
	is_at_end = |cursor| cursor.index >= cursor.bytes.len()
}

february : U8
february = 2

## Days per month for a common year, indexed by month number.
month_lengths : List(U8)
month_lengths = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

last_hour : U8
last_hour = 23

last_minute : U8
last_minute = 59

last_second : U8
last_second = 59

last_sub_second : U16
last_sub_second = 999

minutes_per_hour : U64
minutes_per_hour = 60

## Nanosecond digits, in groups of three per sub-second field.
fraction_digits : U64
fraction_digits = 9

group_digits : U64
group_digits = 3

group_size : U64
group_size = 1000

year_digits : U64
year_digits = 4

## Every field but the year is two digits wide.
field_digits : U64
field_digits = 2

decimal_base : U64
decimal_base = 10

two_digits : U64 -> Str
two_digits = |value| EncodingDate.pad(value, field_digits)

group_value : List(U8), U64 -> U16
group_value = |digits, start|
	digits.sublist({ start, len: group_digits }).fold(0, |acc, digit| acc * U64.to_u16_wrap(decimal_base) + U8.to_u16(digit))

trim_zeros : { value : U64, width : U64 } -> { value : U64, width : U64 }
trim_zeros = |number|
	if number.value % decimal_base == 0 {
		trim_zeros({ value: number.value // decimal_base, width: number.width - 1 })
	} else {
		number
	}

is_digit : U8 -> Bool
is_digit = |byte| byte >= '0' and byte <= '9'

digit_at : EncodingDate.Cursor -> Try(U8, [Unexpected(U64)])
digit_at = |cursor|
	match cursor.bytes.get(cursor.index) {
		Ok(byte) if is_digit(byte) => Ok(byte - '0')
		_ => Err(Unexpected(cursor.index))
	}

advance : EncodingDate.Cursor -> EncodingDate.Cursor
advance = |cursor| { ..cursor, index: cursor.index + 1 }

# The two formats' limits: TOML's (D-S3-24) and CSV's (D-S3-51).
toml_limits : EncodingDate.Limits
toml_limits = { min_year: 0, max_year: 9999, max_offset_minutes: 1439 }

csv_limits : EncodingDate.Limits
csv_limits = { min_year: I32.lowest, max_year: I32.highest, max_offset_minutes: 840 }

date : I32, U8, U8 -> EncodingDate.Date
date = |year, month, day| { year, month, day }

time : U8, U8, U8, EncodingDate.Fraction -> EncodingDate.Time
time = |hour, minute, second, fraction| { hour, minute, second, millisecond: fraction.millisecond, microsecond: fraction.microsecond, nanosecond: fraction.nanosecond }

no_fraction : EncodingDate.Fraction
no_fraction = { millisecond: 0, microsecond: 0, nanosecond: 0 }

cursor_on : Str -> EncodingDate.Cursor
cursor_on = |text| { bytes: text.to_utf8(), index: 0 }

# February 29 across century years, including negative ones.
expect EncodingDate.is_valid_date(date(2024, 2, 29), toml_limits)
expect !EncodingDate.is_valid_date(date(2023, 2, 29), toml_limits)
expect !EncodingDate.is_valid_date(date(1900, 2, 29), toml_limits)
expect !EncodingDate.is_valid_date(date(2100, 2, 29), toml_limits)
expect EncodingDate.is_valid_date(date(2000, 2, 29), toml_limits)
expect EncodingDate.is_valid_date(date(1600, 2, 29), toml_limits)
expect EncodingDate.is_valid_date(date(0, 2, 29), toml_limits)
expect EncodingDate.is_valid_date(date(-400, 2, 29), csv_limits)
expect !EncodingDate.is_valid_date(date(-100, 2, 29), csv_limits)
expect EncodingDate.is_valid_date(date(-4, 2, 29), csv_limits)

# Month lengths, and months and days outside them.
expect [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12].map(|month| EncodingDate.days_in_month(2023, month)) == [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
expect EncodingDate.days_in_month(2024, 2) == 29
expect EncodingDate.days_in_month(2024, 13) == 0
expect EncodingDate.is_valid_date(date(2023, 4, 30), toml_limits)
expect !EncodingDate.is_valid_date(date(2023, 4, 31), toml_limits)
expect EncodingDate.is_valid_date(date(2023, 12, 31), toml_limits)
expect !EncodingDate.is_valid_date(date(2023, 13, 1), toml_limits)
expect !EncodingDate.is_valid_date(date(2023, 0, 1), toml_limits)
expect !EncodingDate.is_valid_date(date(2023, 1, 0), toml_limits)

# Both range-limit sets: years and offsets.
expect !EncodingDate.is_valid_date(date(10000, 1, 1), toml_limits)
expect !EncodingDate.is_valid_date(date(-1, 12, 31), toml_limits)
expect EncodingDate.is_valid_date(date(9999, 12, 31), toml_limits)
expect EncodingDate.is_valid_date(date(10000, 1, 1), csv_limits)
expect EncodingDate.is_valid_date(date(I32.lowest, 1, 1), csv_limits)
expect EncodingDate.is_valid_offset({ minutes: 1439 }, toml_limits)
expect !EncodingDate.is_valid_offset({ minutes: -1440 }, toml_limits)
expect EncodingDate.is_valid_offset({ minutes: -840 }, csv_limits)
expect !EncodingDate.is_valid_offset({ minutes: 841 }, csv_limits)
expect EncodingDate.offset_from_clock(False, 23, 59, toml_limits) == Ok({ minutes: 1439 })
expect EncodingDate.offset_from_clock(True, 24, 0, toml_limits) == Err(OutOfRange)
expect EncodingDate.offset_from_clock(True, 14, 0, csv_limits) == Ok({ minutes: -840 })
expect EncodingDate.offset_from_clock(False, 14, 1, csv_limits) == Err(OutOfRange)
expect EncodingDate.offset_from_clock(False, 1, 60, toml_limits) == Err(OutOfRange)
expect EncodingDate.offset_from_clock(True, 0, 0, csv_limits) == Ok({ minutes: 0 })
expect EncodingDate.offset_from_clock(False, U64.highest, 0, csv_limits) == Err(OutOfRange)

# Times: no second 60, no `24:00`, sub-second fields up to 999.
expect EncodingDate.is_valid_time(time(23, 59, 59, { millisecond: 999, microsecond: 999, nanosecond: 999 }))
expect !EncodingDate.is_valid_time(time(23, 59, 60, no_fraction))
expect !EncodingDate.is_valid_time(time(24, 0, 0, no_fraction))
expect !EncodingDate.is_valid_time(time(12, 60, 0, no_fraction))
expect !EncodingDate.is_valid_time(time(0, 0, 0, { millisecond: 1000, microsecond: 0, nanosecond: 0 }))
expect !EncodingDate.is_valid_time(time(0, 0, 0, { millisecond: 0, microsecond: 1000, nanosecond: 0 }))
expect !EncodingDate.is_valid_time(time(0, 0, 0, { millisecond: 0, microsecond: 0, nanosecond: 1000 }))

# Fraction digits: cut past nine, padded below, trimmed on writing, omitted when zero.
expect EncodingDate.fraction_from_digits([1, 2, 3, 4, 5, 6, 7, 8, 9]) == { millisecond: 123, microsecond: 456, nanosecond: 789 }
expect EncodingDate.fraction_from_digits([1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 9]) == { millisecond: 123, microsecond: 456, nanosecond: 789 }
expect EncodingDate.fraction_from_digits([5]) == { millisecond: 500, microsecond: 0, nanosecond: 0 }
expect EncodingDate.fraction_from_digits([0, 0, 0, 0, 0, 0, 0, 0, 0, 1]) == no_fraction
expect EncodingDate.fraction_from_digits([]) == no_fraction
expect EncodingDate.fraction_text(time(0, 0, 0, no_fraction)) == ""
expect EncodingDate.fraction_text(time(0, 0, 0, { millisecond: 500, microsecond: 0, nanosecond: 0 })) == ".5"
expect EncodingDate.fraction_text(time(0, 0, 0, { millisecond: 0, microsecond: 0, nanosecond: 1 })) == ".000000001"
expect EncodingDate.fraction_text(time(0, 0, 0, { millisecond: 120, microsecond: 30, nanosecond: 0 })) == ".12003"
expect EncodingDate.fraction_text(time(0, 0, 0, { millisecond: 999, microsecond: 999, nanosecond: 999 })) == ".999999999"

# Zero-padded writing, extended years, `Z` for offset zero.
expect EncodingDate.pad(7, 3) == "007"
expect EncodingDate.pad(12345, 3) == "12345"
expect EncodingDate.date_text(date(2026, 3, 8)) == "2026-03-08"
expect EncodingDate.date_text(date(0, 1, 1)) == "0000-01-01"
expect EncodingDate.date_text(date(10000, 1, 1)) == "10000-01-01"
expect EncodingDate.date_text(date(-44, 3, 15)) == "-0044-03-15"
expect EncodingDate.date_text(date(I32.lowest, 1, 1)) == "-2147483648-01-01"
expect EncodingDate.time_text(time(7, 32, 0, no_fraction)) == "07:32:00"
expect EncodingDate.time_text(time(23, 5, 9, { millisecond: 250, microsecond: 0, nanosecond: 0 })) == "23:05:09.25"
expect EncodingDate.offset_text({ minutes: 0 }) == "Z"
expect EncodingDate.offset_text({ minutes: -480 }) == "-08:00"
expect EncodingDate.offset_text({ minutes: -30 }) == "-00:30"
expect EncodingDate.offset_text({ minutes: 1439 }) == "+23:59"
expect EncodingDate.offset_text({ minutes: I16.lowest }) == "-546:08"

# The digit cursor.
expect EncodingDate.fixed_digits(cursor_on("2026-03"), 4) == Ok({ value: 2026, rest: { bytes: "2026-03".to_utf8(), index: 4 } })
expect EncodingDate.fixed_digits(cursor_on("20x6"), 4) == Err(Unexpected(2))
expect EncodingDate.fixed_digits(cursor_on("20"), 4) == Err(Unexpected(2))
expect EncodingDate.digit_run(cursor_on("123456789012Z")) == { digits: [1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2], rest: { bytes: "123456789012Z".to_utf8(), index: 12 } }
expect EncodingDate.digit_run(cursor_on("Z")).digits == []
expect EncodingDate.expect_byte(cursor_on("-0"), '-') == Ok({ bytes: "-0".to_utf8(), index: 1 })
expect EncodingDate.expect_byte(cursor_on("T"), '-') == Err(Unexpected(0))
expect EncodingDate.expect_byte({ bytes: [], index: 0 }, '-') == Err(Unexpected(0))
expect EncodingDate.is_at_end({ bytes: "07".to_utf8(), index: 2 })
expect !EncodingDate.is_at_end(cursor_on("07"))

# A whole reading through the cursor: `hh:mm` into an offset.
expect {
	start = cursor_on("-05:30")
	after_sign = EncodingDate.expect_byte(start, '-') ?? start
	hours = EncodingDate.fixed_digits(after_sign, 2) ?? { value: 0, rest: after_sign }
	colon = EncodingDate.expect_byte(hours.rest, ':') ?? hours.rest
	minutes = EncodingDate.fixed_digits(colon, 2) ?? { value: 0, rest: colon }
	EncodingDate.is_at_end(minutes.rest) and EncodingDate.offset_from_clock(True, hours.value, minutes.value, csv_limits) == Ok({ minutes: -330 })
}
