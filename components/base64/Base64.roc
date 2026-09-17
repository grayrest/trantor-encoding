import Base64Lookup

## Base64: RFC 4648's standard alphabet (§4, written padded) and URL-safe
## alphabet (§5, written unpadded).
##
## ```roc
## Base64.encode("fo".to_utf8()) == "Zm8="
## Base64.encode_url([0xFB, 0xFF]) == "-_8"
## Base64.decode("Zm8") == Ok("fo".to_utf8())
## ```
##
## Decoding accepts padding or its absence and nothing else: no whitespace, no
## line breaks, and the payload bits must be canonical, so unused bits in a
## final group are zero. Scanning left to right, the first problem found is
## `InvalidBase64(index)`, a 0-based offset in the string's UTF-8 bytes:
##
## - a byte outside the alphabet;
## - a misplaced `=`: in a group of fewer than two characters, or padding left
##   incomplete (`QQ=` at the `=`);
## - data after complete padding, at its first character;
## - excess padding, at the first `=` that cannot be there;
## - nonzero unused bits, at the last data character of a two- or
##   three-character final group.
##
## Only a scan without those problems checks the length: a final group of one
## character is `InvalidLength`.
Base64 :: [].{

	## `bytes` in the standard alphabet, padded.
	encode : List(U8) -> Str
	encode = |bytes| encode_with(bytes, standard)

	## `bytes` in the URL-safe alphabet, unpadded.
	encode_url : List(U8) -> Str
	encode_url = |bytes| encode_with(bytes, url_safe)

	## The bytes `text` spells in the standard alphabet, padded or not.
	decode : Str -> Try(List(U8), [InvalidBase64(U64), InvalidLength])
	decode = |text| decode_with(text, standard)

	## The bytes `text` spells in the URL-safe alphabet, padded or not.
	decode_url : Str -> Try(List(U8), [InvalidBase64(U64), InvalidLength])
	decode_url = |text| decode_with(text, url_safe)
}

## An alphabet and its padding policy on output. Input may be padded or not
## under either policy (RFC 4648 §3.2 lets a specification drop padding).
Alphabet : { digits : List(U8), lookup : List(U8), is_padded : Bool }

standard : Alphabet
standard = alphabet("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/", True)

url_safe : Alphabet
url_safe = alphabet("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_", False)

alphabet : Str, Bool -> Alphabet
alphabet = |text, is_padded| {
	digits = text.to_utf8()
	{ digits, lookup: Base64Lookup.table([digits]), is_padded }
}

pad : U8
pad = '='

group_bytes : U64
group_bytes = 3

group_chars : U64
group_chars = 4

digit_bits : U8
digit_bits = 6

byte_bits : U8
byte_bits = 8

digit_mask : U32
digit_mask = 0x3F

byte_mask : U32
byte_mask = 0xFF

## The fewest characters that carry a byte: a shorter final group is invalid.
min_final_group : U64
min_final_group = 2

# Encoding

encode_with : List(U8), Alphabet -> Str
encode_with = |bytes, alpha| {
	capacity = (bytes.len() + group_bytes - 1) // group_bytes * group_chars
	chars = bytes.chunks_of(group_bytes).fold(List.with_capacity(capacity), |out, group| encode_group(out, group, alpha))
	Str.from_utf8_lossy(chars)
}

## One group of one to three bytes as two to four digits, then its padding.
encode_group : List(U8), List(U8), Alphabet -> List(U8)
encode_group = |out, group, alpha| {
	missing = group_bytes - group.len()
	word = group.fold(0.U32, |acc, byte| acc.shl_wrap(byte_bits).bitwise_or(U8.to_u32(byte)))
		.shl_wrap(U64.to_u8_wrap(missing) * byte_bits)
	written = List.repeat(0, group_chars - missing).fold_with_index(out, |acc, _, position| {
		shift = U64.to_u8_wrap(group_chars - 1 - position) * digit_bits
		acc.append(digit(alpha, word.shr_wrap(shift).bitwise_and(digit_mask)))
	})
	if alpha.is_padded {
		written.concat(List.repeat(pad, missing))
	} else {
		written
	}
}

digit : Alphabet, U32 -> U8
digit = |alpha, value| alpha.digits.get(U32.to_u64(value)) ?? pad

# Decoding

## `word` holds the data bits of the group in progress; `chars` counts every data
## character so far; `pads` counts `=` seen and `pad_at` is where the first was.
Decoding : { out : List(U8), word : U32, chars : U64, pads : U64, pad_at : U64 }

DecodeErr : [InvalidBase64(U64), InvalidLength]

decode_with : Str, Alphabet -> Try(List(U8), DecodeErr)
decode_with = |text, alpha| {
	bytes = text.to_utf8()
	start = { out: List.with_capacity(bytes.len() // group_chars * group_bytes), word: 0, chars: 0, pads: 0, pad_at: 0 }
	scanned = Base64Lookup.scan(bytes, start, |state, byte, index| read_byte(alpha, state, byte, index))?
	finish(scanned, bytes.len())
}

read_byte : Alphabet, Decoding, U8, U64 -> Try(Decoding, DecodeErr)
read_byte = |alpha, state, byte, index|
	if state.pads > 0 {
		read_padding(state, byte, index)
	} else if byte == pad {
		start_padding(state, index)
	} else {
		value = Base64Lookup.value_of(alpha.lookup, byte)
		if value == Base64Lookup.absent {
			Err(InvalidBase64(index))
		} else {
			Ok(read_digit(state, value))
		}
	}

read_digit : Decoding, U8 -> Decoding
read_digit = |state, value| {
	word = state.word.shl_wrap(digit_bits).bitwise_or(U8.to_u32(value))
	chars = state.chars + 1
	if chars % group_chars == 0 {
		{ ..state, out: append_word(state.out, word, group_bytes), word: 0, chars }
	} else {
		{ ..state, word, chars }
	}
}

## The first `=`: allowed only after a group of two or three characters, whose
## unused bits are then checked, the last data character being just before it.
start_padding : Decoding, U64 -> Try(Decoding, DecodeErr)
start_padding = |state, index| {
	group_len = state.chars % group_chars
	if group_len < min_final_group {
		Err(InvalidBase64(index))
	} else {
		closed = close_group(state, group_len, index - 1)?
		Ok({ ..closed, pads: 1, pad_at: index })
	}
}

## After the first `=`: more `=` until the group is complete, then nothing.
read_padding : Decoding, U8, U64 -> Try(Decoding, DecodeErr)
read_padding = |state, byte, index| {
	if state.pads == pads_needed(state) {
		Err(InvalidBase64(index))
	} else if byte == pad {
		Ok({ ..state, pads: state.pads + 1 })
	} else {
		Err(InvalidBase64(state.pad_at))
	}
}

finish : Decoding, U64 -> Try(List(U8), DecodeErr)
finish = |state, len| {
	group_len = state.chars % group_chars
	if state.pads > 0 {
		if state.pads == pads_needed(state) {
			Ok(state.out)
		} else {
			Err(InvalidBase64(state.pad_at))
		}
	} else if group_len == 0 {
		Ok(state.out)
	} else if group_len < min_final_group {
		Err(InvalidLength)
	} else {
		closed = close_group(state, group_len, len - 1)?
		Ok(closed.out)
	}
}

pads_needed : Decoding -> U64
pads_needed = |state| group_chars - state.chars % group_chars

## Emit a final group's bytes, refusing nonzero unused bits at `last_index`.
close_group : Decoding, U64, U64 -> Try(Decoding, DecodeErr)
close_group = |state, group_len, last_index| {
	unused_bits = U64.to_u8_wrap(group_len) * digit_bits - U64.to_u8_wrap(group_len - 1) * byte_bits
	unused_mask = 1.U32.shl_wrap(unused_bits) - 1
	if state.word.bitwise_and(unused_mask) != 0 {
		Err(InvalidBase64(last_index))
	} else {
		Ok({ ..state, out: append_word(state.out, state.word.shr_wrap(unused_bits), group_len - 1), word: 0 })
	}
}

## The low `count` bytes of `word`, most significant first.
append_word : List(U8), U32, U64 -> List(U8)
append_word = |out, word, count|
	List.repeat(0, count).fold_with_index(out, |acc, _, position| {
		shift = U64.to_u8_wrap(count - 1 - position) * byte_bits
		acc.append(U32.to_u8_wrap(word.shr_wrap(shift).bitwise_and(byte_mask)))
	})

# RFC 4648 §10 vectors, both ways.
expect Base64.encode([]) == ""
expect Base64.encode("f".to_utf8()) == "Zg=="
expect Base64.encode("fo".to_utf8()) == "Zm8="
expect Base64.encode("foo".to_utf8()) == "Zm9v"
expect Base64.encode("foob".to_utf8()) == "Zm9vYg=="
expect Base64.encode("fooba".to_utf8()) == "Zm9vYmE="
expect Base64.encode("foobar".to_utf8()) == "Zm9vYmFy"
expect Base64.decode("") == Ok([])
expect Base64.decode("Zg==") == Ok("f".to_utf8())
expect Base64.decode("Zm8=") == Ok("fo".to_utf8())
expect Base64.decode("Zm9v") == Ok("foo".to_utf8())
expect Base64.decode("Zm9vYg==") == Ok("foob".to_utf8())
expect Base64.decode("Zm9vYmE=") == Ok("fooba".to_utf8())
expect Base64.decode("Zm9vYmFy") == Ok("foobar".to_utf8())

# Padding is optional on decode.
expect Base64.decode("Zg") == Ok("f".to_utf8())
expect Base64.decode("Zm9vYmE") == Ok("fooba".to_utf8())

# The URL-safe alphabet: `-` and `_`, unpadded out, either in.
expect Base64.encode([0xFB, 0xFF]) == "+/8="
expect Base64.encode_url([0xFB, 0xFF]) == "-_8"
expect Base64.encode_url("f".to_utf8()) == "Zg"
expect Base64.decode_url("-_8") == Ok([0xFB, 0xFF])
expect Base64.decode_url("-_8=") == Ok([0xFB, 0xFF])
expect Base64.decode_url("+/8=") == Err(InvalidBase64(0))
expect Base64.decode("-_8=") == Err(InvalidBase64(0))

# Every byte value round trips through both alphabets.
expect {
	every_byte = List.repeat(0, 256).map_with_index(|_, i| U64.to_u8_wrap(i))
	Base64.decode(Base64.encode(every_byte)) == Ok(every_byte)
	and Base64.decode_url(Base64.encode_url(every_byte)) == Ok(every_byte)
}

# Out-of-alphabet bytes, indices in UTF-8 bytes.
expect Base64.decode("Zm9v!") == Err(InvalidBase64(4))
expect Base64.decode("Zm9v Zg==") == Err(InvalidBase64(4))
expect Base64.decode("Zm9v\n") == Err(InvalidBase64(4))
expect Base64.decode("Zm\u(e9)v") == Err(InvalidBase64(2))

# A misplaced `=`.
expect Base64.decode("=") == Err(InvalidBase64(0))
expect Base64.decode("Z=") == Err(InvalidBase64(1))
expect Base64.decode("Zm9v=") == Err(InvalidBase64(4))
expect Base64.decode("QQ=") == Err(InvalidBase64(2))
expect Base64.decode("QQ=A") == Err(InvalidBase64(2))
expect Base64.decode("QQ=!") == Err(InvalidBase64(2))

# Data after complete padding, at its first character.
expect Base64.decode("Zg==Zg==") == Err(InvalidBase64(4))
expect Base64.decode("Zm8=Zg") == Err(InvalidBase64(4))
expect Base64.decode("Zm8=!") == Err(InvalidBase64(4))

# Excess padding, at the first `=` that cannot be there.
expect Base64.decode("Zg===") == Err(InvalidBase64(4))
expect Base64.decode("Zm8==") == Err(InvalidBase64(4))

# Nonzero unused bits, at the last data character of the final group.
expect Base64.decode("Zh==") == Err(InvalidBase64(1))
expect Base64.decode("Zh") == Err(InvalidBase64(1))
expect Base64.decode("Zm9=") == Err(InvalidBase64(2))
expect Base64.decode("Zm9") == Err(InvalidBase64(2))
expect Base64.decode("Zm9vZh") == Err(InvalidBase64(5))

# A one-character final group is `InvalidLength`, whatever its bits.
expect Base64.decode("Q") == Err(InvalidLength)
expect Base64.decode("QUJDR") == Err(InvalidLength)
expect Base64.decode("QUJDA") == Err(InvalidLength)

# Precedence: the scan's first problem, and the length only after a clean scan.
expect Base64.decode("QUJDR!") == Err(InvalidBase64(5))
expect Base64.decode("QUJDR=") == Err(InvalidBase64(5))
expect Base64.decode("Zh=") == Err(InvalidBase64(1))
expect Base64.decode("Zh==Zg") == Err(InvalidBase64(1))
expect Base64.decode("Zh!") == Err(InvalidBase64(2))
expect Base64.decode("Q!") == Err(InvalidBase64(1))
