# Shared by the tests/*/test.sh suites. `trantor test` hands each script
# TRANTOR, ROC, PKG, TMP, DEPS (the package and its dev-deps) and DEV_DEPS.
# bash 3.2 compatible.
set -euo pipefail

# make_world DIR DEPS_BODY — DIR must be named `app`: trantor stages a world
# under target/trantor/<its directory name>, and an app's header says
# ../target/trantor/app/platform/main.roc.
make_world() {
	mkdir -p "$1/app"
	printf '[world]\nname = "app"\n\n[deps]\n%s' "$2" > "$1/world.toml"
}

# build_app WORLD SOURCE OUT — build SOURCE as bin/OUT, showing why on failure,
# with the modules beside SOURCE (every other `*.roc`) beside it in the app.
# A build with warnings fails too: the package's gate is warning-free.
build_app() {
	local module
	for module in "$(dirname "$2")"/*.roc; do
		[[ "$module" -ef "$2" ]] || cp "$module" "$1/app/"
	done
	cp "$2" "$1/app/main.roc"
	local out
	if ! out=$("$TRANTOR" build "$1" --app app --out "$3" 2>&1); then
		echo "FAIL: build $3" >&2; echo "$out" | tail -40 >&2; exit 1
	fi
	if echo "$out" | grep -Eq '[1-9][0-9]* warnings? found|and [1-9][0-9]* warnings?'; then
		echo "FAIL: build $3 has warnings" >&2; echo "$out" | tail -40 >&2; exit 1
	fi
}

bin() { echo "$1/target/trantor/app/bin/$2"; }

# capped SECONDS CMD... — a hung run fails the suite instead of hanging it.
capped() { local secs=$1; shift; perl -e 'alarm shift; exec { $ARGV[0] } @ARGV or die "capped: exec $ARGV[0]: $!\n"' "$secs" "$@"; }
