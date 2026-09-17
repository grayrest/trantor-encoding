# toml-test's corpus (corpus/, pinned in README.md) through `Toml.parse`: the
# 1.1.0 valid and invalid lists and the 1.0.0 valid list. Valid files must
# parse to the value their JSON describes, invalid files must be refused, and
# every list must pass whole. Every valid file must also write back in both
# modes to text that parses to the same value, the 1.0 text passing the strict
# 1.0 checker, which must accept the 1.0.0 valid list and refuse the invalid
# one. Every valid file must come back byte for byte through `parse_document`
# and `to_str`, its `to_value` equal to `parse`'s, and every invalid file must
# be refused by `parse_document` with `parse`'s error. Also a timed reversed
# 10,000-key compare.
source ../lib.sh
# The corpus must reach the parser as checked in: `.gitattributes` keeps git
# from converting these CRLF files, which `core.autocrlf=input` would.
crlf=corpus/valid/newline-crlf.toml
[[ $(od -An -c "$crlf" | tr -s ' ') == *'\r \n'* ]] || { echo "FAIL: $crlf has lost its CRLF line endings"; exit 1; }
echo "ok: $crlf holds CRLF line endings"
make_world "$TMP/app" "$DEPS"
build_app "$TMP/app" app.roc conformance
out=$(capped 300 "$(bin "$TMP/app" conformance)" "$PWD/corpus" 2>&1) || { echo "FAIL: the conformance app did not finish"; echo "$out" | tail -20; exit 1; }
echo "$out" | grep '^FAIL' && { echo "FAIL: conformance cases failed (above)"; exit 1; }
for list in "1.1.0 valid" "1.1.0 invalid" "1.0.0 valid" "1.1.0 valid round trip" "1.0.0 valid round trip" \
	"1.1.0 valid document" "1.0.0 valid document" "1.1.0 invalid document" \
	"strict 1.0 checker, 1.0.0 valid" "strict 1.0 checker, 1.0.0 invalid"; do
	line=$(echo "$out" | grep "^$list: ") || { echo "FAIL: no result for $list"; echo "$out" | tail -5; exit 1; }
	echo "ok: $line"
done
line=$(echo "$out" | grep "^reversed .*: ok") || { echo "FAIL: $(echo "$out" | grep '^reversed')"; exit 1; }
echo "ok: $line"
