## Number conversions the formats share, below their grammars.
##
## A format's grammar reads the text (CSV's XML Schema forms, TOML's
## underscores and prefixes) and hands this module digit values, never
## characters: `0`–`9`, or `0`–`15` for hex. What comes back is the value, or
## `OutOfRange` for the format to report its own way.
##
## - Integers accumulate into a sign and a `U128` magnitude, then narrow into a
##   width through the builtin conversion passed in (`U128.to_u8_try`,
##   `I128.to_i64_try`, …).
## - Floats go to `F64`/`F32` rounded to nearest, past the type's range to
##   ±infinity; to `Dec` with the exponent applied and digits past the 18th
##   fractional place cut toward zero, a value past `Dec`'s range refused.
## - Finite floats are spelled shortest-round-trip with a `.0` added where the
##   spelling would read as an integer (`1.0`, `-0.0`, `1e300`, `1e-7`); the
##   format passes its special spellings.
EncodingNumber :: [].{

	## An integer as read: a sign and its magnitude, before narrowing.
	Integer : { is_negative : Bool, magnitude : U128 }

	## A sign and digit values, most significant first.
	SignedDigits : { is_negative : Bool, digits : List(U8) }

	## A decimal float as its grammar split it: `whole.fraction` times ten to
	## the `exponent`. Any list may be empty; an empty exponent is zero.
	FloatParts : { is_negative : Bool, whole : List(U8), fraction : List(U8), exponent : SignedDigits }

	## How a format spells the values a finite spelling cannot.
	Specials : { infinity : Str, negative_infinity : Str, nan : Str }

	## `magnitude` with one more digit in `radix` appended.
	accumulate : U128, U8, U8 -> Try(U128, [OutOfRange])
	accumulate = |magnitude, radix, digit| {
		shifted = magnitude.times_try(U8.to_u128(radix)) ? |_| OutOfRange
		shifted.plus_try(U8.to_u128(digit)).map_err(|_| OutOfRange)
	}

	## The magnitude of `digits` in `radix`.
	magnitude : List(U8), U8 -> Try(U128, [OutOfRange])
	magnitude = |digits, radix|
		digits.fold_until(Ok(0), |acc, digit|
			match acc {
				Err(_) => Break(acc)
				Ok(so_far) =>
					match accumulate(so_far, radix, digit) {
						Ok(next) => Continue(Ok(next))
						Err(problem) => Break(Err(problem))
					}
			})

	## `integer` into an unsigned width through `convert` (`U128.to_u16_try`).
	## Minus zero is zero.
	to_unsigned : Integer, (U128 -> Try(n, [OutOfRange, ..])) -> Try(n, [OutOfRange])
	to_unsigned = |integer, convert|
		if integer.is_negative and integer.magnitude != 0 {
			Err(OutOfRange)
		} else {
			convert(integer.magnitude).map_err(|_| OutOfRange)
		}

	## `integer` into a signed width through `convert` (`I128.to_i32_try`).
	to_signed : Integer, (I128 -> Try(n, [OutOfRange, ..])) -> Try(n, [OutOfRange])
	to_signed = |integer, convert| {
		value = signed_value(integer)?
		convert(value).map_err(|_| OutOfRange)
	}

	## The nearest `F64`; ±infinity past its range.
	to_f64 : FloatParts -> F64
	to_f64 = |parts| {
		size =
			match float_magnitude(parts) {
				Zero => 0.0
				Infinite => F64.infinity
				Text(text) => F64.from_str(text) ?? F64.infinity
			}
		if parts.is_negative { -size } else { size }
	}

	## The nearest `F32`; ±infinity past its range.
	to_f32 : FloatParts -> F32
	to_f32 = |parts| {
		size =
			match float_magnitude(parts) {
				Zero => 0.0
				Infinite => F32.infinity
				Text(text) => F32.from_str(text) ?? F32.infinity
			}
		if parts.is_negative { -size } else { size }
	}

	## `parts` as a `Dec`, digits past the 18th fractional place cut toward
	## zero; `OutOfRange` past `Dec`'s range.
	to_dec : FloatParts -> Try(Dec, [OutOfRange])
	to_dec = |parts| {
		normal = normalize(parts)
		if normal.digits.is_empty() {
			Ok(0)
		} else if normal.point > dec_whole_digits {
			Err(OutOfRange)
		} else if normal.point + dec_fraction_digits <= 0 {
			Ok(0)
		} else {
			atto_digits = I128.to_u64_wrap(normal.point + dec_fraction_digits)
			kept = normal.digits.take_first(atto_digits).concat(List.repeat(0, atto_digits - normal.digits.len().min(atto_digits)))
			attos_magnitude = magnitude(kept, decimal)?
			attos = to_signed({ is_negative: parts.is_negative, magnitude: attos_magnitude }, |value| Ok(value))?
			Ok(Dec.from_attos(attos))
		}
	}

	## The shortest spelling that reads back as `value`, recognisably a float.
	f64_spelling : F64, Specials -> Str
	f64_spelling = |value, specials|
		if value.is_nan() {
			specials.nan
		} else if value.is_infinite() {
			if value < 0 { specials.negative_infinity } else { specials.infinity }
		} else {
			with_point(value.to_str())
		}

	## `f64_spelling` for `F32`, by `F32`'s own shortest digits (`0.1`).
	f32_spelling : F32, Specials -> Str
	f32_spelling = |value, specials|
		if value.is_nan() {
			specials.nan
		} else if value.is_infinite() {
			if value < 0 { specials.negative_infinity } else { specials.infinity }
		} else {
			with_point(value.to_str())
		}
}

decimal : U8
decimal = 10

## `Dec` holds 18 fractional digits and at most 21 whole digits.
dec_fraction_digits : I128
dec_fraction_digits = 18

dec_whole_digits : I128
dec_whole_digits = 21

## Past these decimal-point positions every `F64` and `F32` is infinite or
## zero, so the builtin parser never sees an extreme exponent.
float_point_limit : I128
float_point_limit = 400

## An exponent past `U64`'s range counts as `U64`'s largest: no digit list is
## long enough for the difference to change a result.
exponent_cap : U128
exponent_cap = U64.to_u128(U64.highest)

## The magnitude of `I128.lowest`, one past `I128.highest`.
i128_lowest_magnitude : U128
i128_lowest_magnitude = I128.to_u128_wrap(I128.highest) + 1

signed_value : Integer -> Try(I128, [OutOfRange])
signed_value = |integer|
	if integer.is_negative and integer.magnitude == i128_lowest_magnitude {
		Ok(I128.lowest)
	} else {
		value = integer.magnitude.to_i128_try() ? |_| OutOfRange
		Ok(if integer.is_negative { -value } else { value })
	}

## Significant digits without leading or trailing zeros, and the decimal
## point's position: the value is `0.digits` times ten to `point`.
Normalized : { digits : List(U8), point : I128 }

normalize : EncodingNumber.FloatParts -> Normalized
normalize = |parts| {
	all = parts.whole.concat(parts.fraction)
	leading = all.find_first_index(|digit| digit != 0) ?? all.len()
	significant = all.drop_first(leading)
	digits =
		match significant.find_last_index(|digit| digit != 0) {
			Ok(last) => significant.take_first(last + 1)
			Err(_) => []
		}
	point = U64.to_i128(parts.whole.len()) - U64.to_i128(leading) + exponent_value(parts.exponent)
	{ digits, point }
}

exponent_value : EncodingNumber.SignedDigits -> I128
exponent_value = |exponent| {
	size = U128.to_i128_wrap((EncodingNumber.magnitude(exponent.digits, decimal) ?? exponent_cap).min(exponent_cap))
	if exponent.is_negative { -size } else { size }
}

## A float's unsigned size: zero, past every float's range, or text in a
## canonical form (`0.123e-5`) the builtin parsers read. They refuse such text
## only past the type's range, which is why a refusal reads as infinity.
float_magnitude : EncodingNumber.FloatParts -> [Zero, Infinite, Text(Str)]
float_magnitude = |parts| {
	normal = normalize(parts)
	if normal.digits.is_empty() or normal.point < -float_point_limit {
		Zero
	} else if normal.point > float_point_limit {
		Infinite
	} else {
		Text("0.${digit_text(normal.digits)}e${normal.point.to_str()}")
	}
}

digit_text : List(U8) -> Str
digit_text = |digits| Str.from_utf8_lossy(digits.map(|digit| digit + '0'))

with_point : Str -> Str
with_point = |spelling|
	if spelling.contains(".") or spelling.contains("e") {
		spelling
	} else {
		"${spelling}.0"
	}

# Test input: `[-]whole[.fraction][e[-]exponent]` as float parts.
parts_of : Str -> EncodingNumber.FloatParts
parts_of = |text| {
	unsigned = text.drop_prefix("-")
	pieces = unsigned.split_on("e")
	mantissa = pieces.first() ?? ""
	exponent = pieces.get(1) ?? ""
	halves = mantissa.split_on(".")
	{
		is_negative: text.starts_with("-"),
		whole: digit_values(halves.first() ?? ""),
		fraction: digit_values(halves.get(1) ?? ""),
		exponent: { is_negative: exponent.starts_with("-"), digits: digit_values(exponent.drop_prefix("-")) },
	}
}

digit_values : Str -> List(U8)
digit_values = |text| text.to_utf8().map(|byte| byte - '0')

positive : U128 -> EncodingNumber.Integer
positive = |size| { is_negative: False, magnitude: size }

negative : U128 -> EncodingNumber.Integer
negative = |size| { is_negative: True, magnitude: size }

lowercase : EncodingNumber.Specials
lowercase = { infinity: "inf", negative_infinity: "-inf", nan: "nan" }

schema : EncodingNumber.Specials
schema = { infinity: "INF", negative_infinity: "-INF", nan: "NaN" }

# Accumulating, in decimal and hex, to `U128`'s largest and past it.
expect EncodingNumber.magnitude([], 10) == Ok(0)
expect EncodingNumber.magnitude([4, 2], 10) == Ok(42)
expect EncodingNumber.magnitude([15, 15], 16) == Ok(255)
expect EncodingNumber.magnitude(digit_values("340282366920938463463374607431768211455"), 10) == Ok(U128.highest)
expect EncodingNumber.magnitude(digit_values("340282366920938463463374607431768211456"), 10) == Err(OutOfRange)
expect EncodingNumber.magnitude(digit_values("3402823669209384634633746074317682114550"), 10) == Err(OutOfRange)
expect EncodingNumber.accumulate(U128.highest // 16, 16, 15) == Ok(U128.highest)

# Narrowing at every unsigned width's bounds.
expect EncodingNumber.to_unsigned(positive(255), U128.to_u8_try) == Ok(255.U8)
expect EncodingNumber.to_unsigned(positive(256), U128.to_u8_try) == Err(OutOfRange)
expect EncodingNumber.to_unsigned(positive(65535), U128.to_u16_try) == Ok(65535.U16)
expect EncodingNumber.to_unsigned(positive(65536), U128.to_u16_try) == Err(OutOfRange)
expect EncodingNumber.to_unsigned(positive(4294967295), U128.to_u32_try) == Ok(4294967295.U32)
expect EncodingNumber.to_unsigned(positive(4294967296), U128.to_u32_try) == Err(OutOfRange)
expect EncodingNumber.to_unsigned(positive(18446744073709551615), U128.to_u64_try) == Ok(U64.highest)
expect EncodingNumber.to_unsigned(positive(18446744073709551616), U128.to_u64_try) == Err(OutOfRange)
expect EncodingNumber.to_unsigned(positive(U128.highest), |size| Ok(size)) == Ok(U128.highest)
expect EncodingNumber.to_unsigned(negative(0), U128.to_u8_try) == Ok(0.U8)
expect EncodingNumber.to_unsigned(negative(1), U128.to_u128_try) == Err(OutOfRange)

# Narrowing at every signed width's bounds.
expect EncodingNumber.to_signed(positive(127), I128.to_i8_try) == Ok(127.I8)
expect EncodingNumber.to_signed(positive(128), I128.to_i8_try) == Err(OutOfRange)
expect EncodingNumber.to_signed(negative(128), I128.to_i8_try) == Ok(-128.I8)
expect EncodingNumber.to_signed(negative(129), I128.to_i8_try) == Err(OutOfRange)
expect EncodingNumber.to_signed(positive(32767), I128.to_i16_try) == Ok(32767.I16)
expect EncodingNumber.to_signed(negative(32769), I128.to_i16_try) == Err(OutOfRange)
expect EncodingNumber.to_signed(negative(2147483648), I128.to_i32_try) == Ok(I32.lowest)
expect EncodingNumber.to_signed(positive(2147483648), I128.to_i32_try) == Err(OutOfRange)
expect EncodingNumber.to_signed(negative(9223372036854775808), I128.to_i64_try) == Ok(I64.lowest)
expect EncodingNumber.to_signed(positive(9223372036854775808), I128.to_i64_try) == Err(OutOfRange)
expect EncodingNumber.to_signed(negative(170141183460469231731687303715884105728), |value| Ok(value)) == Ok(I128.lowest)
expect EncodingNumber.to_signed(negative(170141183460469231731687303715884105729), |value| Ok(value)) == Err(OutOfRange)
expect EncodingNumber.to_signed(positive(170141183460469231731687303715884105727), |value| Ok(value)) == Ok(I128.highest)
expect EncodingNumber.to_signed(positive(170141183460469231731687303715884105728), |value| Ok(value)) == Err(OutOfRange)
expect EncodingNumber.to_signed(negative(0), I128.to_i8_try) == Ok(0.I8)

# Floats: rounding, exponents, zeros, and ±infinity past the range.
expect EncodingNumber.to_f64(parts_of("1.5")) == 1.5
expect EncodingNumber.to_f64(parts_of("0.1")) == 0.1
expect EncodingNumber.to_f64(parts_of("15e-1")) == 1.5
expect EncodingNumber.to_f64(parts_of("000.00125e3")) == 1.25
expect EncodingNumber.to_f64(parts_of(".5")) == 0.5
expect EncodingNumber.to_f64(parts_of("5.")) == 5.0
expect EncodingNumber.to_f64(parts_of("9007199254740993")) == 9007199254740992.0
expect EncodingNumber.to_f64(parts_of("1.7976931348623157e308")) == F64.highest
expect EncodingNumber.to_f64(parts_of("5e-324")).to_str() == "5e-324"
expect EncodingNumber.to_f64(parts_of("1e-400")) == 0.0
expect EncodingNumber.to_f64(parts_of("-0.0")).to_str() == "-0"
expect EncodingNumber.to_f64(parts_of("-1e-400")).to_str() == "-0"
expect EncodingNumber.to_f64(parts_of("1e400")) == F64.infinity
expect EncodingNumber.to_f64(parts_of("-1e400")) == -F64.infinity
expect EncodingNumber.to_f64(parts_of("1.7976931348623159e308")) == F64.infinity
expect EncodingNumber.to_f64(parts_of("1e99999999999999999999999999999999999999999")) == F64.infinity
expect EncodingNumber.to_f64(parts_of("1e-99999999999999999999999999999999999999999")) == 0.0
expect EncodingNumber.to_f32(parts_of("0.1")) == 0.1
expect EncodingNumber.to_f32(parts_of("3.4028235e38")) == F32.highest
expect EncodingNumber.to_f32(parts_of("1e39")) == F32.infinity
expect EncodingNumber.to_f32(parts_of("-1e39")) == -F32.infinity
expect EncodingNumber.to_f32(parts_of("-1e-50")).to_str() == "-0"

# Dec: the exponent applied, the 19th fractional digit and beyond cut toward zero.
expect EncodingNumber.to_dec(parts_of("1.5")) == Ok(1.5)
expect EncodingNumber.to_dec(parts_of("-0.25")) == Ok(-0.25)
expect EncodingNumber.to_dec(parts_of("125e-2")) == Ok(1.25)
expect EncodingNumber.to_dec(parts_of("0.9999999999999999999")) == Ok(0.999999999999999999)
expect EncodingNumber.to_dec(parts_of("0.99999999999999999999")) == Ok(0.999999999999999999)
expect EncodingNumber.to_dec(parts_of("-0.12345678901234567891")) == Ok(-0.123456789012345678)
expect EncodingNumber.to_dec(parts_of("1e-30")) == Ok(0)
expect EncodingNumber.to_dec(parts_of("-1e-30")) == Ok(0)
expect EncodingNumber.to_dec(parts_of("1.5e-20")) == Ok(0)
expect EncodingNumber.to_dec(parts_of("1.5e-18")) == Ok(0.000000000000000001)
expect EncodingNumber.to_dec(parts_of("0")) == Ok(0)
expect EncodingNumber.to_dec(parts_of("170141183460469231731")) == Ok(170141183460469231731)
expect EncodingNumber.to_dec(parts_of("170141183460469231731.687303715884105727")) == Ok(Dec.highest)
expect EncodingNumber.to_dec(parts_of("-170141183460469231731.687303715884105728")) == Ok(Dec.lowest)
expect EncodingNumber.to_dec(parts_of("170141183460469231732")) == Err(OutOfRange)
expect EncodingNumber.to_dec(parts_of("1e21")) == Err(OutOfRange)
expect EncodingNumber.to_dec(parts_of("1e400")) == Err(OutOfRange)

# Spellings under both special tables: `.0` where needed, `F32` by its own digits.
expect EncodingNumber.f64_spelling(1.0, lowercase) == "1.0"
expect EncodingNumber.f64_spelling(-0.0, lowercase) == "-0.0"
expect EncodingNumber.f64_spelling(1e300, lowercase) == "1e300"
expect EncodingNumber.f64_spelling(1e-7, lowercase) == "1e-7"
expect EncodingNumber.f64_spelling(0.1, lowercase) == "0.1"
expect EncodingNumber.f64_spelling(1e15, lowercase) == "1000000000000000.0"
expect EncodingNumber.f64_spelling(1e16, lowercase) == "1e16"
expect EncodingNumber.f64_spelling(-2.5, lowercase) == "-2.5"
expect EncodingNumber.f64_spelling(F64.highest, lowercase) == "1.7976931348623157e308"
expect EncodingNumber.f64_spelling(F64.infinity, lowercase) == "inf"
expect EncodingNumber.f64_spelling(-F64.infinity, lowercase) == "-inf"
expect EncodingNumber.f64_spelling(F64.nan, lowercase) == "nan"
expect EncodingNumber.f64_spelling(1.0, schema) == "1.0"
expect EncodingNumber.f64_spelling(F64.infinity, schema) == "INF"
expect EncodingNumber.f64_spelling(-F64.infinity, schema) == "-INF"
expect EncodingNumber.f64_spelling(F64.nan, schema) == "NaN"
expect EncodingNumber.f32_spelling(0.1, lowercase) == "0.1"
expect EncodingNumber.f32_spelling(16777216.0, lowercase) == "16777216.0"
expect EncodingNumber.f32_spelling(-0.0, schema) == "-0.0"
expect EncodingNumber.f32_spelling(F32.highest, schema) == "3.4028235e38"
expect EncodingNumber.f32_spelling(-F32.infinity, lowercase) == "-inf"
expect EncodingNumber.f32_spelling(F32.nan, schema) == "NaN"

# Spellings read back to the same value.
expect [0.1, 1e300, 5e-324, 123456.789, -2.2250738585072014e-308].all(|value|
	EncodingNumber.to_f64(parts_of(EncodingNumber.f64_spelling(value, lowercase))) == value)
