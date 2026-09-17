# TOML conformance

`corpus/` is the `tests/` directory of
[toml-lang/toml-test](https://github.com/toml-lang/toml-test) at commit
`ff49d109861c1ad25af53f687f2aef19ab650600` (2026-09-15), copied unchanged,
with the project's `LICENSE` (MIT) beside it. `.gitattributes` at the package
root marks it `-text`, so CRLF cases stay CRLF in every checkout.

| List | Cases | Run here |
|---|---|---|
| `files-toml-1.1.0`, valid | 218 | yes: parsed and compared with the expected JSON |
| `files-toml-1.1.0`, invalid | 494 | yes: must be refused |
| `files-toml-1.0.0`, valid | 208 | yes: parsed and compared (48 `spec-1.0.0` files are only here) |
| `files-toml-1.0.0`, invalid | 501 | only through the strict 1.0 checker: nine are valid TOML 1.1 (D-S3-35) |

`app.roc` reads each list, parses every `.toml` file it names with
`Toml.parse`, and converts the file's `.json` (toml-test's tagged JSON) into a
`Toml.Value` through a JSON reader of its own (`ExpectedJson.roc`), compared
with `==`. An invalid file that is not UTF-8 cannot become a `Str` and counts
as refused (nine files under `invalid/encoding`).

Every file in both valid lists is also written back with `Toml.to_str_with`
in `V1_0` and `V1_1` and must parse to the same value. The `V1_0` text must pass
`StrictToml.check`, a test-only TOML 1.0 checker: `Toml.parse` plus a scan for
1.1's additions (line breaks, comments and a trailing comma in inline tables,
times without seconds, `\e` and `\xHH`). The checker is held to both 1.0.0
lists: it accepts all 208 valid files and refuses all 501 invalid ones.

Every file in both valid lists also goes through `Toml.parse_document`: its
`to_str` must be the file's bytes exactly and its `to_value` equal to
`Toml.parse`'s value. Every file in the 1.1.0 invalid list must be refused by
`parse_document` with the error `parse` gives.

`test.sh` builds the app, requires every list to pass whole, checks a CRLF
corpus file still holds CRLF, and times a reversed 10,000-key table compare
against a 2-second budget.

To update the corpus, replace `corpus/` with a newer checkout's `tests/`, then
the commit and counts above.
