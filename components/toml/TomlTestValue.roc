import Toml

## `Toml.Value` equality and `Toml.Float`'s conversions.
TomlTestValue :: [].{}

float : F64 -> Toml.Value
float = |value| Float(Toml.float_from_f64(value))

## The float a one-key document holds.
parsed_float : Str -> Try(Toml.Float, [NotAFloat])
parsed_float = |literal|
	match Toml.parse("x = ${literal}") {
		Ok(Table([(_, Float(value))])) => Ok(value)
		_ => Err(NotAFloat)
	}

dec_of : Str -> Try(Dec, [NotADec, NotAFloat])
dec_of = |literal| {
	value = parsed_float(literal) ? |_| NotAFloat
	value.to_dec().map_err(|_| NotADec)
}

f64_of : Str -> F64
f64_of = |literal| parsed_float(literal).map_ok(|value| value.to_f64()) ?? 0.0

key_count : U64
key_count = 10000

numbered : List((Str, Toml.Value))
numbered = List.repeat({}, key_count).map_with_index(|_, index| ("key${index.to_str()}", Integer(U64.to_i64_wrap(index))))

# Tables compare by key in any order; arrays in order; kinds never mix.
expect {
	left : Toml.Value
	left = Table([("a", Integer(1)), ("b", String("x"))])
	right : Toml.Value
	right = Table([("b", String("x")), ("a", Integer(1))])
	left == right
}
expect {
	left : Toml.Value
	left = Array([Integer(1), Integer(2)])
	left != Array([Integer(2), Integer(1)])
}
expect {
	left : Toml.Value
	left = Integer(1)
	left != float(1.0)
}
expect {
	left : Toml.Value
	left = Table([("a", Integer(1))])
	left != Table([("a", Integer(1)), ("b", Integer(2))])
}

# User-built duplicate keys: equal to themselves, and symmetric.
expect {
	doubled : Toml.Value
	doubled = Table([("a", Integer(1)), ("a", Integer(2))])
	doubled == Table([("a", Integer(1)), ("a", Integer(2))])
}
expect {
	doubled : Toml.Value
	doubled = Table([("a", Integer(1)), ("a", Integer(2))])
	doubled == Table([("a", Integer(2)), ("a", Integer(1))])
}
expect {
	doubled : Toml.Value
	doubled = Table([("b", Integer(0)), ("a", float(F64.nan)), ("a", Integer(2)), ("a", Integer(2))])
	doubled == Table([("a", Integer(2)), ("a", Integer(2)), ("b", Integer(0)), ("a", float(F64.nan))])
}
expect {
	doubled : Toml.Value
	doubled = Table([("a", Integer(1)), ("a", Integer(1)), ("a", Integer(2))])
	doubled != Table([("a", Integer(1)), ("a", Integer(2)), ("a", Integer(2))])
}
expect {
	doubled : Toml.Value
	doubled = Table([("a", Integer(1)), ("a", Integer(2)), ("b", Integer(3))])
	doubled != Table([("a", Integer(1)), ("b", Integer(2)), ("b", Integer(3))])
}
expect {
	doubled : Toml.Value
	doubled = Table([("a", Integer(1)), ("a", Integer(2))])
	single : Toml.Value
	single = Table([("a", Integer(1)), ("b", Integer(2))])
	doubled != single and single != doubled
}

# NaN equals NaN, nested too; `-0.0` equals `0.0`; `1.0` equals `1.00`.
expect {
	nested : Toml.Value
	nested = Array([Table([("x", float(F64.nan))])])
	nested == Array([Table([("x", float(F64.nan))])])
}
expect {
	value : Toml.Value
	value = float(-0.0)
	value == float(0.0)
}
expect parsed_float("1.0") == parsed_float("1.00")
expect parsed_float("nan") == parsed_float("-nan")
expect parsed_float("1.0") != parsed_float("1.5")

# A reversed 10,000-key compare (the conformance suite times it).
expect {
	forward : Toml.Value
	forward = Table(numbered)
	forward == Table(numbered.rev())
}
expect {
	forward : Toml.Value
	forward = Table(numbered)
	forward != Table(numbered.take_first(numbered.len() - 1).append(("key0", Integer(1))))
}

# `to_dec`: digits past the 18th cut toward zero, exponents applied, `Dec`'s
# range at its largest whole part.
expect dec_of("0.1234567890123456789") == Ok(0.123456789012345678)
expect dec_of("0.12345678901234567891") == Ok(0.123456789012345678)
expect dec_of("-0.9999999999999999999") == Ok(-0.999999999999999999)
expect dec_of("1e-30") == Ok(0.0)
expect dec_of("1.5e-20") == Ok(0.0)
expect dec_of("1.5e-18") == Ok(0.000000000000000001)
expect dec_of("12_345.5e2") == Ok(1234550.0)
expect dec_of("170141183460469231731.687303715884105727") == Ok(Dec.highest)
expect dec_of("170141183460469231732.0") == Err(NotADec)
expect dec_of("inf") == Err(NotADec)
expect dec_of("nan") == Err(NotADec)

# `to_f64`: nearest, ±infinity past the range.
expect f64_of("1e400") == F64.infinity
expect f64_of("-1e400") == -F64.infinity
expect f64_of("6.626e-34") == 6.626e-34
expect f64_of("224_617.445_991_228") == 224617.445991228

# Built floats: `float_from_dec` keeps the exact decimal, `float_from_f64` the
# value.
expect Toml.float_from_dec(12345678.123456789012345678).to_dec() == Ok(12345678.123456789012345678)
expect Toml.float_from_dec(-0.25).to_f64() == -0.25
expect Toml.float_from_f64(0.1).to_f64() == 0.1
expect Toml.float_from_f64(1e300).to_dec() == Err(NotADec)
expect Toml.float_from_f64(F64.infinity).to_f64() == F64.infinity
expect Toml.float_from_f64(1.5) == Toml.float_from_dec(1.5)
