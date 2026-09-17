## The byte lookup table and failing byte scan that `Base64` and `Hex` share.
##
## A table has one entry per byte value: the digit's value for bytes in the
## alphabet, `absent` for every other byte.
Base64Lookup :: [].{

	## The table entry of a byte outside the alphabet.
	absent : U8
	absent = 0xFF

	## A table where the byte at position `p` of each digit list has value `p`.
	## `Hex` passes its lowercase and uppercase digits as two lists.
	table : List(List(U8)) -> List(U8)
	table = |digit_lists|
		digit_lists.fold(List.repeat(absent, byte_values), |entries, digits|
			digits.fold_with_index(entries, |acc, digit, position|
				acc.set(U8.to_u64(digit), U64.to_u8_wrap(position)) ?? acc))

	## The value of `byte` in `lookup`, or `absent`.
	value_of : List(U8), U8 -> U8
	value_of = |lookup, byte| lookup.get(U8.to_u64(byte)) ?? absent

	## `step` over each byte with its 0-based index, stopping at the first `Err`.
	scan : List(U8), state, (state, U8, U64 -> Try(state, err)) -> Try(state, err)
	scan = |bytes, initial, step|
		bytes.fold_with_index_until(Ok(initial), |acc, byte, index|
			match acc {
				Err(_) => Break(acc)
				Ok(state) =>
					match step(state, byte, index) {
						Ok(next) => Continue(Ok(next))
						Err(problem) => Break(Err(problem))
					}
			})
}

byte_values : U64
byte_values = 256

expect Base64Lookup.table([]).len() == 256
expect Base64Lookup.value_of(Base64Lookup.table([['a', 'b']]), 'b') == 1
expect Base64Lookup.value_of(Base64Lookup.table([['a'], ['A']]), 'A') == 0
expect Base64Lookup.value_of(Base64Lookup.table([['a']]), 'z') == Base64Lookup.absent
expect Base64Lookup.scan([1, 2, 3], 0, |sum, byte, _| Ok(sum + U8.to_u64(byte))) == Ok(6)
expect {
	result : Try(U64, [Stop(U64)])
	result = Base64Lookup.scan([1, 2, 3], 0, |_, byte, index| if byte == 2 { Err(Stop(index)) } else { Ok(index) })
	result == Err(Stop(1))
}
