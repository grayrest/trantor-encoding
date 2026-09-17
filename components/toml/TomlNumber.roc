import EncodingNumber

## TOML's integer and float grammar over bytes: decimal integers without
## leading zeros, `0x`/`0o`/`0b` prefixes, underscores between digits, floats
## with a fraction and/or an exponent, and `inf`/`nan` with an optional sign.
##
## Values leave through `EncodingNumber`: integers as `I64` (anything past it
## is `OutOfRange`), floats as their parts, uninterpreted, so the caller keeps
## the spelling.
TomlNumber :: [].{

	## A float literal's meaning.
	FloatReading : [Finite(EncodingNumber.FloatParts), Infinite(Bool), NotANumber]

	Number : [Integer(I64), Float(FloatReading)]

	## Where reading stopped short: the offending byte and what would fit
	## there, or a well-formed integer outside `I64` (with the index just past
	## it).
	Problem : [Invalid({ at : U64, expected : Str }), OutOfRange(U64)]

	## The number starting at `index`, and the index just past it.
	read : List(U8), U64 -> Try({ number : Number, next : U64 }, Problem)
	read = |bytes, index| {
		sign_end = if is_sign(bytes.get(index)) { index + 1 } else { index }
		is_negative = bytes.get(index) == Ok('-')
		match bytes.get(sign_end) {
			Ok('i') => special(bytes, sign_end, "inf", Infinite(is_negative))
			Ok('n') => special(bytes, sign_end, "nan", NotANumber)
			Ok('0') if sign_end == index and is_radix_prefix(bytes.get(index + 1)) => prefixed(bytes, index)
			_ => decimal(bytes, sign_end, is_negative)
		}
	}

	## A hexadecimal digit's value, in either case.
	hex_digit : U8 -> Try(U8, [NotADigit])
	hex_digit = |byte|
		if byte >= 'a' and byte <= 'f' {
			Ok(byte - 'a' + decimal_radix)
		} else if byte >= 'A' and byte <= 'F' {
			Ok(byte - 'A' + decimal_radix)
		} else {
			radix_digit(byte, decimal_radix)
		}

	## A whole spelling as one float; `Invalid` when anything is left over or
	## it is an integer.
	float_of : Str -> Try(FloatReading, [Invalid])
	float_of = |spelling| {
		bytes = spelling.to_utf8()
		match TomlNumber.read(bytes, 0) {
			Ok({ number: Float(reading), next }) if next == bytes.len() => Ok(reading)
			_ => Err(Invalid)
		}
	}
}

decimal_radix : U8
decimal_radix = 10

expected_digit : Str
expected_digit = "a digit"

expected_no_leading_zero : Str
expected_no_leading_zero = "a number without leading zeros"

## `dec_digit` and friends: a byte's digit value in a radix, if it is one.
DigitOf : U8 -> Try(U8, [NotADigit])

is_sign : Try(U8, _) -> Bool
is_sign = |byte| byte == Ok('+') or byte == Ok('-')

is_radix_prefix : Try(U8, _) -> Bool
is_radix_prefix = |byte| byte == Ok('x') or byte == Ok('o') or byte == Ok('b')

special : List(U8), U64, Str, TomlNumber.FloatReading -> Try({ number : TomlNumber.Number, next : U64 }, TomlNumber.Problem)
special = |bytes, index, word, reading| {
	letters = word.to_utf8()
	if bytes.sublist({ start: index, len: letters.len() }) == letters {
		Ok({ number: Float(reading), next: index + letters.len() })
	} else {
		Err(Invalid({ at: index, expected: "a number, `inf` or `nan`" }))
	}
}

## `0x`, `0o` or `0b` and at least one digit of that radix.
prefixed : List(U8), U64 -> Try({ number : TomlNumber.Number, next : U64 }, TomlNumber.Problem)
prefixed = |bytes, index| {
	digit_of =
		match bytes.get(index + 1) {
			Ok('x') => TomlNumber.hex_digit
			Ok('o') => |byte| radix_digit(byte, 8)
			_ => |byte| radix_digit(byte, 2)
		}
	radix =
		match bytes.get(index + 1) {
			Ok('x') => 16
			Ok('o') => 8
			_ => 2
		}
	run = digit_run(bytes, index + 2, digit_of)?
	value = integer({ is_negative: False, digits: run.digits }, radix, run.next)?
	Ok({ number: Integer(value), next: run.next })
}

## A decimal integer part, then a fraction and/or an exponent for a float.
decimal : List(U8), U64, Bool -> Try({ number : TomlNumber.Number, next : U64 }, TomlNumber.Problem)
decimal = |bytes, index, is_negative| {
	whole = digit_run(bytes, index, dec_digit)?
	if whole.digits.len() > 1 and whole.digits.first() == Ok(0) {
		Err(Invalid({ at: index, expected: expected_no_leading_zero }))
	} else {
		fraction =
			if bytes.get(whole.next) == Ok('.') {
				digit_run(bytes, whole.next + 1, dec_digit)?
			} else {
				{ digits: [], next: whole.next }
			}
		has_fraction = fraction.next != whole.next
		exponent = exponent_at(bytes, fraction.next)?
		if !has_fraction and exponent.next == fraction.next {
			value = integer({ is_negative, digits: whole.digits }, decimal_radix, whole.next)?
			Ok({ number: Integer(value), next: whole.next })
		} else {
			parts = { is_negative, whole: whole.digits, fraction: fraction.digits, exponent: exponent.value }
			Ok({ number: Float(Finite(parts)), next: exponent.next })
		}
	}
}

## `e` or `E`, an optional sign and digits (leading zeros allowed); nothing
## when no `e` is there.
exponent_at : List(U8), U64 -> Try({ value : EncodingNumber.SignedDigits, next : U64 }, TomlNumber.Problem)
exponent_at = |bytes, index|
	if bytes.get(index) == Ok('e') or bytes.get(index) == Ok('E') {
		signed = is_sign(bytes.get(index + 1))
		start = if signed { index + 2 } else { index + 1 }
		run = digit_run(bytes, start, dec_digit)?
		Ok({ value: { is_negative: bytes.get(index + 1) == Ok('-'), digits: run.digits }, next: run.next })
	} else {
		Ok({ value: { is_negative: False, digits: [] }, next: index })
	}

integer : EncodingNumber.SignedDigits, U8, U64 -> Try(I64, TomlNumber.Problem)
integer = |signed, radix, next| {
	size = EncodingNumber.magnitude(signed.digits, radix) ? |_| OutOfRange(next)
	EncodingNumber.to_signed({ is_negative: signed.is_negative, magnitude: size }, I128.to_i64_try).map_err(|_| OutOfRange(next))
}

## One or more digits with single underscores between them.
digit_run : List(U8), U64, DigitOf -> Try({ digits : List(U8), next : U64 }, TomlNumber.Problem)
digit_run = |bytes, index, digit_of| {
	first = digit_at(bytes, index, digit_of)?
	more_digits(bytes, index + 1, digit_of, [first])
}

more_digits : List(U8), U64, DigitOf, List(U8) -> Try({ digits : List(U8), next : U64 }, TomlNumber.Problem)
more_digits = |bytes, index, digit_of, digits|
	match bytes.get(index) {
		Ok('_') => {
			digit = digit_at(bytes, index + 1, digit_of)?
			more_digits(bytes, index + 2, digit_of, digits.append(digit))
		}
		Ok(byte) =>
			match digit_of(byte) {
				Ok(digit) => more_digits(bytes, index + 1, digit_of, digits.append(digit))
				Err(_) => Ok({ digits, next: index })
			}
		Err(_) => Ok({ digits, next: index })
	}

digit_at : List(U8), U64, DigitOf -> Try(U8, TomlNumber.Problem)
digit_at = |bytes, index, digit_of|
	match bytes.get(index) {
		Ok(byte) => digit_of(byte).map_err(|_| Invalid({ at: index, expected: expected_digit }))
		Err(_) => Err(Invalid({ at: index, expected: expected_digit }))
	}

dec_digit : U8 -> Try(U8, [NotADigit])
dec_digit = |byte| radix_digit(byte, decimal_radix)

## `0`-`9` below `radix` (at most ten).
radix_digit : U8, U8 -> Try(U8, [NotADigit])
radix_digit = |byte, radix|
	if byte >= '0' and byte < '0' + radix {
		Ok(byte - '0')
	} else {
		Err(NotADigit)
	}

reads : Str -> Try(TomlNumber.Number, TomlNumber.Problem)
reads = |text| {
	bytes = text.to_utf8()
	read = TomlNumber.read(bytes, 0)?
	if read.next == bytes.len() { Ok(read.number) } else { Err(Invalid({ at: read.next, expected: "the end" })) }
}

float_parts : Bool, List(U8), List(U8), Bool, List(U8) -> TomlNumber.Number
float_parts = |is_negative, whole, fraction, exponent_negative, exponent| Float(Finite({ is_negative, whole, fraction, exponent: { is_negative: exponent_negative, digits: exponent } }))

# Decimal integers, signs, underscores, and `I64`'s bounds.
expect reads("0") == Ok(Integer(0))
expect reads("+99") == Ok(Integer(99))
expect reads("-17") == Ok(Integer(-17))
expect reads("-0") == Ok(Integer(0))
expect reads("5_349_221") == Ok(Integer(5349221))
expect reads("9223372036854775807") == Ok(Integer(I64.highest))
expect reads("-9223372036854775808") == Ok(Integer(I64.lowest))
expect reads("9223372036854775808") == Err(OutOfRange(19))
expect reads("-9223372036854775809") == Err(OutOfRange(20))
expect reads("99999999999999999999999999999999999999999") == Err(OutOfRange(41))

# Prefixed integers: either hex case, leading zeros, no sign.
expect reads("0xDEAD_beef") == Ok(Integer(0xDEADBEEF))
expect reads("0o755") == Ok(Integer(493))
expect reads("0b0011") == Ok(Integer(3))
expect reads("0x7FFFFFFFFFFFFFFF") == Ok(Integer(I64.highest))
expect reads("0x8000000000000000") == Err(OutOfRange(18))
expect reads("0o8") == Err(Invalid({ at: 2, expected: expected_digit }))
expect reads("0x") == Err(Invalid({ at: 2, expected: expected_digit }))
expect reads("0x_1") == Err(Invalid({ at: 2, expected: expected_digit }))

# Refused integers stop at the offending byte.
expect reads("01") == Err(Invalid({ at: 0, expected: expected_no_leading_zero }))
expect reads("1__2") == Err(Invalid({ at: 2, expected: expected_digit }))
expect reads("1_") == Err(Invalid({ at: 2, expected: expected_digit }))
expect reads("+0x1") == Err(Invalid({ at: 2, expected: "the end" }))

# Floats keep their parts uninterpreted.
expect reads("3.1415") == Ok(float_parts(False, [3], [1, 4, 1, 5], False, []))
expect reads("-0.0") == Ok(float_parts(True, [0], [0], False, []))
expect reads("5e+22") == Ok(float_parts(False, [5], [], False, [2, 2]))
expect reads("-2E-02") == Ok(float_parts(True, [2], [], True, [0, 2]))
expect reads("6.626e-34") == Ok(float_parts(False, [6], [6, 2, 6], True, [3, 4]))
expect reads("224_617.445_991") == Ok(float_parts(False, [2, 2, 4, 6, 1, 7], [4, 4, 5, 9, 9, 1], False, []))
expect reads("inf") == Ok(Float(Infinite(False)))
expect reads("-inf") == Ok(Float(Infinite(True)))
expect reads("+nan") == Ok(Float(NotANumber))
expect reads("7.") == Err(Invalid({ at: 2, expected: expected_digit }))
expect reads("3.e+20") == Err(Invalid({ at: 2, expected: expected_digit }))
expect reads("1e") == Err(Invalid({ at: 2, expected: expected_digit }))
expect reads("1e_2") == Err(Invalid({ at: 2, expected: expected_digit }))
expect reads("00.5") == Err(Invalid({ at: 0, expected: expected_no_leading_zero }))
expect reads("infinity") == Err(Invalid({ at: 3, expected: "the end" }))
expect reads("Inf") == Err(Invalid({ at: 0, expected: expected_digit }))
expect TomlNumber.float_of("1.5") == Ok(Finite({ is_negative: False, whole: [1], fraction: [5], exponent: { is_negative: False, digits: [] } }))
expect TomlNumber.float_of("15") == Err(Invalid)
