## Text handling the formats share: the byte-order mark, error positions and
## byte-order sorting.
##
## Positions are 1-based lines and columns counted in code points. What ends a
## line is the format's, passed as a predicate: CSV breaks on LF, CRLF and a
## lone CR; TOML on LF and CRLF.
EncodingText :: [].{

	Position : { line : U64, column : U64 }

	## `bytes` without one leading U+FEFF, so position 1:1 is what follows it.
	skip_bom : List(U8) -> List(U8)
	skip_bom = |bytes|
		if bytes.starts_with(bom) {
			bytes.drop_first(bom.len())
		} else {
			bytes
		}

	## Where the byte at `index` sits. `ends_line(bytes, i)` says whether the
	## byte at `i` ends its line; a UTF-8 continuation byte does not move the
	## column.
	position_at : List(U8), U64, (List(U8), U64 -> Bool) -> Position
	position_at = |bytes, index, ends_line|
		bytes.take_first(index).fold_with_index({ line: 1, column: 1 }, |position, byte, at|
			if ends_line(bytes, at) {
				{ line: position.line + 1, column: 1 }
			} else if is_continuation(byte) {
				position
			} else {
				{ ..position, column: position.column + 1 }
			})

	## A syntax error at `position`, `expected` naming what would fix it.
	syntax : Position, Str -> [Syntax({ line : U64, column : U64, expected : Str }), ..]
	syntax = |position, expected| Syntax({ line: position.line, column: position.column, expected })

	## The `line L, column C: ` prefix of a positioned error's message.
	position_prefix : U64, U64 -> Str
	position_prefix = |line, column| "line ${line.to_str()}, column ${column.to_str()}: "

	## Order by UTF-8 bytes, which is code-point order.
	compare : Str, Str -> [Before, Same, After]
	compare = |left, right| compare_bytes(left.to_utf8(), right.to_utf8())

	## Every string in any of `lists`, once each, in byte order.
	sorted_union : List(List(Str)) -> List(Str)
	sorted_union = |lists|
		lists.join().sort_with(compare).fold([], |kept, text|
			if kept.last() == Ok(text) {
				kept
			} else {
				kept.append(text)
			})
}

bom : List(U8)
bom = [0xEF, 0xBB, 0xBF]

continuation_mask : U8
continuation_mask = 0xC0

continuation_tag : U8
continuation_tag = 0x80

is_continuation : U8 -> Bool
is_continuation = |byte| byte.bitwise_and(continuation_mask) == continuation_tag

compare_bytes : List(U8), List(U8) -> [Before, Same, After]
compare_bytes = |left, right| {
	common =
		left.fold_with_index_until(Same, |_, byte, index|
			match right.get(index) {
				Err(_) => Break(After)
				Ok(other) =>
					match byte.order_relative_to(other) {
						Same => Continue(Same)
						order => Break(order)
					}
			})
	match common {
		Same => left.len().order_relative_to(right.len())
		order => order
	}
}

# Line-break predicates as the formats define them.
csv_ends_line : List(U8), U64 -> Bool
csv_ends_line = |bytes, index|
	match bytes.get(index) {
		Ok('\n') => True
		Ok('\r') => bytes.get(index + 1) != Ok('\n')
		_ => False
	}

toml_ends_line : List(U8), U64 -> Bool
toml_ends_line = |bytes, index| bytes.get(index) == Ok('\n')

position_in : Str, U64, (List(U8), U64 -> Bool) -> EncodingText.Position
position_in = |text, index, ends_line| EncodingText.position_at(text.to_utf8(), index, ends_line)

# One BOM skipped, and only one; nothing else touched.
expect EncodingText.skip_bom("\u(FEFF)a".to_utf8()) == ['a']
expect EncodingText.skip_bom("\u(FEFF)\u(FEFF)a".to_utf8()) == "\u(FEFF)a".to_utf8()
expect EncodingText.skip_bom("a\u(FEFF)".to_utf8()) == "a\u(FEFF)".to_utf8()
expect EncodingText.skip_bom([0xEF, 0xBB]) == [0xEF, 0xBB]
expect EncodingText.position_at(EncodingText.skip_bom("\u(FEFF)ab".to_utf8()), 1, toml_ends_line) == { line: 1, column: 2 }

# Columns count code points: after two-, three- and four-byte characters.
expect position_in("abc", 0, toml_ends_line) == { line: 1, column: 1 }
expect position_in("abc", 2, toml_ends_line) == { line: 1, column: 3 }
expect position_in("é=", 2, toml_ends_line) == { line: 1, column: 2 }
expect position_in("€=", 3, csv_ends_line) == { line: 1, column: 2 }
expect position_in("😀😀x", 8, csv_ends_line) == { line: 1, column: 3 }
expect position_in("a\n😀x", 6, toml_ends_line) == { line: 2, column: 2 }

# LF, CRLF and a lone CR under each predicate.
expect position_in("a\nb", 2, csv_ends_line) == { line: 2, column: 1 }
expect position_in("a\nb", 2, toml_ends_line) == { line: 2, column: 1 }
expect position_in("a\r\nb", 3, csv_ends_line) == { line: 2, column: 1 }
expect position_in("a\r\nb", 3, toml_ends_line) == { line: 2, column: 1 }
expect position_in("a\r\nb", 1, csv_ends_line) == { line: 1, column: 2 }
expect position_in("a\r\nb", 2, toml_ends_line) == { line: 1, column: 3 }
expect position_in("a\rb", 2, csv_ends_line) == { line: 2, column: 1 }
expect position_in("a\rb", 2, toml_ends_line) == { line: 1, column: 3 }
expect position_in("\r\n\r\n\n x", 6, csv_ends_line) == { line: 4, column: 2 }
expect position_in("\r\r\n", 2, csv_ends_line) == { line: 2, column: 2 }
expect position_in("\r\r\n", 2, toml_ends_line) == { line: 1, column: 3 }

# The syntax constructor opens its row, and the message prefix.
expect {
	problem : [Syntax({ line : U64, column : U64, expected : Str }), MissingHeader]
	problem = EncodingText.syntax({ line: 3, column: 7 }, "a closing quote")
	problem == Syntax({ line: 3, column: 7, expected: "a closing quote" })
}
expect EncodingText.position_prefix(3, 7) == "line 3, column 7: "

# Byte order is code-point order, which UTF-16 order is not past U+FFFF.
expect EncodingText.compare("a", "b") == Before
expect EncodingText.compare("b", "a") == After
expect EncodingText.compare("ab", "ab") == Same
expect EncodingText.compare("ab", "abc") == Before
expect EncodingText.compare("abc", "ab") == After
expect EncodingText.compare("", "") == Same
expect EncodingText.compare("Z", "a") == Before
expect EncodingText.compare("z", "é") == Before
expect EncodingText.compare("\u(FFFD)", "\u(1F600)") == Before
expect EncodingText.compare("\u(7FF)", "\u(800)") == Before

# The sorted union: every name once, in byte order.
expect EncodingText.sorted_union([["b", "a"], ["c", "a"], []]) == ["a", "b", "c"]
expect EncodingText.sorted_union([["é", "z", "Z"], ["", "z"]]) == ["", "Z", "z", "é"]
expect EncodingText.sorted_union([]) == []
