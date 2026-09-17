# TOML edit snapshots

Each directory under `cases/` is one edit of a document:

| File | What |
|---|---|
| `before.toml` | the document as written |
| `edit` | the edit, as TOML (below) |
| `after.toml` | the text the edit must give, byte for byte; absent when the edit must fail |

`.gitattributes` at the package root marks `cases/**` `-text`, so CRLF files
(`cases/crlf`) and the byte-order mark (`cases/bom`) reach the app as
checked in.

## The `edit` file

```toml
op = "set_with"                      # set, set_with, remove, append, set_typed
path = ["deps", "trantor-cli"]       # strings are keys, integers indices; [] is the root
value = { path = "../trantor-cli" }  # set, set_with, append, set_typed
version = "V1_1"                     # set_with only: V1_0 (the default) or V1_1
table = "Inline"                     # set_with only: Auto (the default), Inline, Header, Dotted
error = 'StyleNotPossible({ path: [Key("p"), Key("x")], style: Dotted })'   # when the edit must fail
```

- `set`, `set_with`, `remove` and `append` call the `Toml.Document` method of
  that name with `path`, `value` and, for `set_with`, a `Toml.Edit` built from
  `version` and `table`.
- `set_typed` decodes `value` into a record
  `{ path : Str, optional : Bool, features : List(Str) }`, turns it back into
  a value with `Toml.encode_value`, and `set`s that: a typed record going
  into a document.
- `error` is the error's `Str.inspect` text. Tags and paths are promised
  (D-S3-17); this text is only how the suite compares them.

## What every case checks

For an edit that succeeds:

1. the edited document's `to_str()` equals `after.toml` exactly;
2. `after.toml` parses (`Toml.parse` and `Toml.parse_document`);
3. reading it back through `get`: the value set, or the element appended as
   the array's last, equals `value` (for `set_typed`, the encoded record);
   after a `remove` the path is `NotFound`, or, when the path ends in an
   index, the array holds one element fewer.

For an edit that must fail, the error equals `error` and there is no
`after.toml`.

`test.sh` builds `app.roc`, checks that the CRLF case still holds CRLF, runs
every directory, and requires all of them to pass.

## Adding a case

Create the directory with `before.toml` and `edit`, work out `after.toml` from
the editing rules (D-S3-10, D-S3-29, D-S3-31, D-S3-40–42, D-S3-46, D-S3-47,
D-S3-53, D-S3-54.7–9, D-S3-55.17) rather than copying the output, and run
`trantor test .`. A failing case prints what the edit gave and what
`after.toml` holds.
