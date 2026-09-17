# Edit snapshots (README.md): every directory under cases/ is one edit whose
# output must match after.toml byte for byte, parse, and read back. Every case
# must pass, and the count must be every directory.
source ../lib.sh
# CRLF cases must reach the app as checked in: `.gitattributes` keeps git from
# converting them.
for file in cases/crlf/before.toml cases/crlf/after.toml; do
	[[ $(od -An -c "$file" | tr -s ' ') == *'\r \n'* ]] || { echo "FAIL: $file has lost its CRLF line endings"; exit 1; }
done
echo "ok: the crlf case holds CRLF line endings"
make_world "$TMP/app" "$DEPS"
build_app "$TMP/app" app.roc edits
names=()
for dir in cases/*/; do names+=("$(basename "$dir")"); done
out=$(capped 120 "$(bin "$TMP/app" edits)" "$PWD/cases" "${names[@]}" 2>&1) || { echo "FAIL: the edit app did not finish"; echo "$out" | tail -20; exit 1; }
echo "$out" | grep -A12 '^FAIL' && { echo "FAIL: edit cases failed (above)"; exit 1; }
line=$(echo "$out" | grep '^edit cases: ') || { echo "FAIL: no result line"; echo "$out" | tail -5; exit 1; }
[[ $line == "edit cases: ${#names[@]} passed, 0 failed" ]] || { echo "FAIL: $line (expected ${#names[@]})"; exit 1; }
echo "ok: $line"
