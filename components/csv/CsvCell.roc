import EncodingNumber

## Cell text to values and back, by XML Schema 1.1's lexical forms (the W3C
## CSV on the Web default):
##
## - integers `[+-]?[0-9]+`, leading zeros allowed;
## - `F32`/`F64`/`Dec`: a sign, digits with an optional fraction (`.5` and `5.`
##   too) and an optional `e`/`E` exponent; `F32`/`F64` also `INF`, `+INF`,
##   `-INF` and `NaN`, case-sensitive;
## - booleans `true`, `false`, `1`, `0`.
##
## Nothing else is accepted: no underscores, hex, grouping or surrounding
## whitespace. Writing uses the shortest float spelling with `INF`, `-INF` and
## `NaN`, and `true`/`false`.
CsvCell :: [].{

	## `text` in an unsigned width through `convert` (`U128.to_u8_try`).
	unsigned : Str, (U128 -> Try(n, [OutOfRange, ..])) -> Try(n, [Invalid])
	unsigned = |text, convert| {
		integer = read_integer(text)?
		EncodingNumber.to_unsigned(integer, convert).map_err(|_| Invalid)
	}

	## `text` in a signed width through `convert` (`I128.to_i8_try`).
	signed : Str, (I128 -> Try(n, [OutOfRange, ..])) -> Try(n, [Invalid])
	signed = |text, convert| {
		integer = read_integer(text)?
		EncodingNumber.to_signed(integer, convert).map_err(|_| Invalid)
	}

	## The nearest `F64`; ±infinity past its range.
	f64 : Str -> Try(F64, [Invalid])
	f64 = |text|
		match read_float(text)? {
			Finite(parts) => Ok(EncodingNumber.to_f64(parts))
			Infinity => Ok(F64.infinity)
			NegativeInfinity => Ok(-F64.infinity)
			NaN => Ok(F64.nan)
		}

	## The nearest `F32`; ±infinity past its range.
	f32 : Str -> Try(F32, [Invalid])
	f32 = |text|
		match read_float(text)? {
			Finite(parts) => Ok(EncodingNumber.to_f32(parts))
			Infinity => Ok(F32.infinity)
			NegativeInfinity => Ok(-F32.infinity)
			NaN => Ok(F32.nan)
		}

	## A `Dec`, digits past the 18th fractional place cut toward zero.
	dec : Str -> Try(Dec, [Invalid])
	dec = |text| {
		parts = read_decimal(text.to_utf8())?
		EncodingNumber.to_dec(parts).map_err(|_| Invalid)
	}

	boolean : Str -> Try(Bool, [Invalid])
	boolean = |text|
		match text {
			"true" | "1" => Ok(True)
			"false" | "0" => Ok(False)
			_ => Err(Invalid)
		}

	f64_text : F64 -> Str
	f64_text = |value| EncodingNumber.f64_spelling(value, specials)

	f32_text : F32 -> Str
	f32_text = |value| EncodingNumber.f32_spelling(value, specials)

	boolean_text : Bool -> Str
	boolean_text = |value| if value { "true" } else { "false" }
}

specials : EncodingNumber.Specials
specials = { infinity: "INF", negative_infinity: "-INF", nan: "NaN" }

## Splits an optional leading sign from the rest.
Signed : { is_negative : Bool, rest : List(U8) }

signed_bytes : List(U8) -> Signed
signed_bytes = |bytes|
	match bytes {
		['-', .. as rest] => { is_negative: True, rest }
		['+', .. as rest] => { is_negative: False, rest }
		_ => { is_negative: False, rest: bytes }
	}

is_digit : U8 -> Bool
is_digit = |byte| byte >= '0' and byte <= '9'

## The digit values of `bytes` when every one is a digit and there is at least
## one.
digit_values : List(U8) -> Try(List(U8), [Invalid])
digit_values = |bytes|
	if !bytes.is_empty() and bytes.all(is_digit) {
		Ok(bytes.map(|byte| byte - '0'))
	} else {
		Err(Invalid)
	}

## Digit values of a possibly empty run.
optional_digits : List(U8) -> Try(List(U8), [Invalid])
optional_digits = |bytes| if bytes.is_empty() { Ok([]) } else { digit_values(bytes) }

read_integer : Str -> Try(EncodingNumber.Integer, [Invalid])
read_integer = |text| {
	sign = signed_bytes(text.to_utf8())
	digits = digit_values(sign.rest)?
	magnitude = EncodingNumber.magnitude(digits, decimal).map_err(|_| Invalid)?
	Ok({ is_negative: sign.is_negative, magnitude })
}

decimal : U8
decimal = 10

read_float : Str -> Try([Finite(EncodingNumber.FloatParts), Infinity, NegativeInfinity, NaN], [Invalid])
read_float = |text|
	match text {
		"INF" | "+INF" => Ok(Infinity)
		"-INF" => Ok(NegativeInfinity)
		"NaN" => Ok(NaN)
		_ => read_decimal(text.to_utf8()).map_ok(|parts| Finite(parts))
	}

## `[+-]? (digits (. digits?)? | . digits) ([eE] [+-]? digits)?`
read_decimal : List(U8) -> Try(EncodingNumber.FloatParts, [Invalid])
read_decimal = |bytes| {
	sign = signed_bytes(bytes)
	exponent_at = sign.rest.find_first_index(|byte| byte == 'e' or byte == 'E') ?? sign.rest.len()
	mantissa = sign.rest.take_first(exponent_at)
	point_at = mantissa.find_first_index(|byte| byte == '.') ?? mantissa.len()
	whole = optional_digits(mantissa.take_first(point_at))?
	fraction = optional_digits(mantissa.drop_first(point_at + 1))?
	if whole.is_empty() and fraction.is_empty() {
		Err(Invalid)
	} else {
		exponent = read_exponent(sign.rest.drop_first(exponent_at))?
		Ok({ is_negative: sign.is_negative, whole, fraction, exponent })
	}
}

## Empty, or `e`/`E` then a signed digit run.
read_exponent : List(U8) -> Try(EncodingNumber.SignedDigits, [Invalid])
read_exponent = |bytes|
	match bytes {
		[] => Ok({ is_negative: False, digits: [] })
		[_, .. as rest] => {
			sign = signed_bytes(rest)
			digits = digit_values(sign.rest)?
			Ok({ is_negative: sign.is_negative, digits })
		}
	}

# Integers: signs, leading zeros, bounds; nothing but digits.
expect CsvCell.unsigned("42", U128.to_u8_try) == Ok(42.U8)
expect CsvCell.unsigned("+5", U128.to_u8_try) == Ok(5.U8)
expect CsvCell.unsigned("007", U128.to_u16_try) == Ok(7.U16)
expect CsvCell.unsigned("-0", U128.to_u32_try) == Ok(0.U32)
expect CsvCell.unsigned("256", U128.to_u8_try) == Err(Invalid)
expect CsvCell.unsigned("-1", U128.to_u64_try) == Err(Invalid)
expect CsvCell.signed("-128", I128.to_i8_try) == Ok(-128.I8)
expect CsvCell.signed("-129", I128.to_i8_try) == Err(Invalid)
expect CsvCell.unsigned("340282366920938463463374607431768211455", |size| Ok(size)) == Ok(U128.highest)
expect CsvCell.unsigned("340282366920938463463374607431768211456", |size| Ok(size)) == Err(Invalid)
expect ["", "+", "-", "1_000", "0x10", "1,000", " 1", "1 ", "1.0", "1e3", "٣"].all(|text| CsvCell.signed(text, I128.to_i64_try) == Err(Invalid))

# Floats: every lexical form, the specials case-sensitively, and ±infinity past the range.
expect CsvCell.f64("1.5") == Ok(1.5)
expect CsvCell.f64(".5") == Ok(0.5)
expect CsvCell.f64("5.") == Ok(5.0)
expect CsvCell.f64("-2") == Ok(-2.0)
expect CsvCell.f64("+1.25E2") == Ok(125.0)
expect CsvCell.f64("15e-1") == Ok(1.5)
expect CsvCell.f64("1e+2") == Ok(100.0)
expect CsvCell.f64("9007199254740993") == Ok(9007199254740992.0)
expect CsvCell.f64("INF") == Ok(F64.infinity)
expect CsvCell.f64("+INF") == Ok(F64.infinity)
expect CsvCell.f64("-INF") == Ok(-F64.infinity)
expect CsvCell.f64("NaN").map_ok(|value| value.is_nan()) == Ok(True)
expect CsvCell.f64("1e400") == Ok(F64.infinity)
expect CsvCell.f32("1e39") == Ok(F32.infinity)
expect CsvCell.f32("0.1") == Ok(0.1)
expect ["", ".", "e5", "1e", "1e+", "1.2.3", "1_000.0", "0x10", "1,000.5", "nan", "inf", "Infinity", "-NaN", "+NaN", " 1.0", "1.0 "].all(|text| CsvCell.f64(text) == Err(Invalid))

# Dec: exponents applied, past 18 fractional digits cut, no specials.
expect CsvCell.dec("-0.25") == Ok(-0.25)
expect CsvCell.dec("125e-2") == Ok(1.25)
expect CsvCell.dec("0.1234567890123456789") == Ok(0.123456789012345678)
expect CsvCell.dec("1e-30") == Ok(0)
expect CsvCell.dec("1e21") == Err(Invalid)
expect ["INF", "-INF", "NaN", "", "1_0"].all(|text| CsvCell.dec(text) == Err(Invalid))

# Booleans: exactly four spellings.
expect ["true", "1"].all(|text| CsvCell.boolean(text) == Ok(True))
expect ["false", "0"].all(|text| CsvCell.boolean(text) == Ok(False))
expect ["TRUE", "False", "yes", "", " true", "01"].all(|text| CsvCell.boolean(text) == Err(Invalid))

# Spellings: `.0` added, XML Schema specials.
expect CsvCell.f64_text(1.0) == "1.0"
expect CsvCell.f64_text(-F64.infinity) == "-INF"
expect CsvCell.f32_text(F32.nan) == "NaN"
expect CsvCell.boolean_text(False) == "false"
