import Base64Lookup

## Hex: RFC 4648 base16, written lowercase and read in either case.
##
## ```roc
## Hex.encode([0xCA, 0xFE]) == "cafe"
## Hex.decode("CAFE") == Ok([0xCA, 0xFE])
## ```
##
## Decoding takes no separators, prefixes or whitespace. Error indices are
## 0-based offsets in the string's UTF-8 bytes. An invalid character is reported
## before an odd length.
Hex :: [].{

	## `bytes` as two lowercase hex digits each.
	encode : List(U8) -> Str
	encode = |bytes| {
		digits = bytes.fold(List.with_capacity(bytes.len() * digits_per_byte), |out, byte|
			out
				.append(lower_digit(byte.shr_wrap(nibble_bits)))
				.append(lower_digit(byte.bitwise_and(nibble_mask))))
		Str.from_utf8_lossy(digits)
	}

	## The bytes `text` spells in hex digits of either case.
	decode : Str -> Try(List(U8), [InvalidHex(U64), OddLength])
	decode = |text| {
		bytes = text.to_utf8()
		start = { out: List.with_capacity(bytes.len() // digits_per_byte), high: 0 }
		decoded = Base64Lookup.scan(bytes, start, read_digit)?
		if bytes.len() % digits_per_byte == 0 {
			Ok(decoded.out)
		} else {
			Err(OddLength)
		}
	}
}

digits_per_byte : U64
digits_per_byte = 2

nibble_bits : U8
nibble_bits = 4

nibble_mask : U8
nibble_mask = 0x0F

lower_digits : List(U8)
lower_digits = "0123456789abcdef".to_utf8()

lookup : List(U8)
lookup = Base64Lookup.table([lower_digits, "0123456789ABCDEF".to_utf8()])

lower_digit : U8 -> U8
lower_digit = |nibble| lower_digits.get(U8.to_u64(nibble)) ?? 0

## `high` is the pending high nibble while a byte is half read.
Decoding : { out : List(U8), high : U8 }

DecodeErr : [InvalidHex(U64), OddLength]

## One digit into the pending high nibble (even index) or a finished byte (odd index).
read_digit : Decoding, U8, U64 -> Try(Decoding, DecodeErr)
read_digit = |state, byte, index| {
	nibble = Base64Lookup.value_of(lookup, byte)
	if nibble == Base64Lookup.absent {
		Err(InvalidHex(index))
	} else if index % digits_per_byte == 0 {
		Ok({ ..state, high: nibble })
	} else {
		Ok({ ..state, out: state.out.append(state.high.shl_wrap(nibble_bits).bitwise_or(nibble)) })
	}
}

# RFC 4648 §10 base16 vectors: lowercased for encode, decoded as written.
expect Hex.encode([]) == ""
expect Hex.encode("f".to_utf8()) == "66"
expect Hex.encode("fo".to_utf8()) == "666f"
expect Hex.encode("foo".to_utf8()) == "666f6f"
expect Hex.encode("foob".to_utf8()) == "666f6f62"
expect Hex.encode("fooba".to_utf8()) == "666f6f6261"
expect Hex.encode("foobar".to_utf8()) == "666f6f626172"
expect Hex.decode("") == Ok([])
expect Hex.decode("66") == Ok("f".to_utf8())
expect Hex.decode("666F") == Ok("fo".to_utf8())
expect Hex.decode("666F6F") == Ok("foo".to_utf8())
expect Hex.decode("666F6F62") == Ok("foob".to_utf8())
expect Hex.decode("666F6F6261") == Ok("fooba".to_utf8())
expect Hex.decode("666F6F626172") == Ok("foobar".to_utf8())

# Either case, mixed, and every byte value round trip.
expect Hex.decode("666f6F") == Ok("foo".to_utf8())
expect Hex.encode([0x00, 0x0F, 0xA5, 0xFF]) == "000fa5ff"
expect {
	every_byte = List.repeat(0, 256).map_with_index(|_, i| U64.to_u8_wrap(i))
	Hex.decode(Hex.encode(every_byte)) == Ok(every_byte)
}

# Rejections, indices in UTF-8 bytes.
expect Hex.decode("6g") == Err(InvalidHex(1))
expect Hex.decode("0x10") == Err(InvalidHex(1))
expect Hex.decode("66 6f") == Err(InvalidHex(2))
expect Hex.decode("\u(e9)6") == Err(InvalidHex(0))
expect Hex.decode("6\u(e9)") == Err(InvalidHex(1))
expect Hex.decode("abc") == Err(OddLength)
expect Hex.decode("a") == Err(OddLength)

# An invalid character wins over an odd length.
expect Hex.decode("abcg1") == Err(InvalidHex(3))
expect Hex.decode("abc\u(e9)") == Err(InvalidHex(3))
