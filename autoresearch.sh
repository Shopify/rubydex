#!/usr/bin/env bash
# Autoresearch benchmark for the Rubydex resolve stage.
#
# Runs the release CLI against the Shopify core monolith and reports the
# wall-clock time of the resolution stage plus the total run time.
#
# The target repo ships a legacy `rubydex.toml` that uses a top-level
# `exclude` array (the old format). Rubydex's strict parser rejects it, so we
# temporarily rewrite it to the current `[graph] exclude = [...]` form, run,
# and always restore the original on exit.
set -euo pipefail

SHOPIFY="${SHOPIFY_CORE:-/Users/dersam/world/trees/root/src/areas/core/shopify}"
CFG="$SHOPIFY/rubydex.toml"
CLI="rust/target/release/rubydex_cli"

if [ ! -d "$SHOPIFY" ]; then
    echo "error: the benchmark target '$SHOPIFY' does not exist." >&2
    echo "Set SHOPIFY_CORE to the path of a checkout of the Shopify core monolith." >&2
    exit 1
fi

# Fast pre-check: build is up to date and compiles.
if ! cargo build --release --quiet --manifest-path rust/Cargo.toml 2>/dev/null; then
    echo "METRIC name=resolve_s value=0"
    echo "BUILD_FAILED=1"
    exit 1
fi

# Transform the legacy config in place; restore on exit.
BAK=""
restore() {
    if [ -n "$BAK" ] && [ -f "$BAK" ]; then
        mv "$BAK" "$CFG"
    fi
}
# A signal must restore the config too. Without INT, a Ctrl-C leaves the target repo with a
# modified, uncommitted `rubydex.toml`.
trap restore EXIT INT TERM HUP

if [ -f "$CFG" ] && ! grep -q '^\[graph\]' "$CFG" && grep -q '^exclude' "$CFG"; then
    BAK="$CFG.autoresearch.bak"
    cp "$CFG" "$BAK"
    # Prepend the section header. This stays in the shell, because a path that carries a quote
    # would break out of an interpolated `python3 -c` program.
    printf '[graph]\n' | cat - "$BAK" > "$CFG"
fi

# Run multiple iterations and take the minimum (least CPU-interfered) for stability.
# The shared machine has variable load, so a single run is too noisy.
BEST_RESOLVE=""
BEST_TOTAL=""
RUNS=5
for run in $(seq 1 "$RUNS"); do
    OUT=$(mktemp -t rubydex_bench.XXXXXX)
    /usr/bin/time -p "$CLI" "$SHOPIFY" --stats --stop-after resolution > "$OUT" 2>&1 || {
        cat "$OUT"
        rm -f "$OUT"
        echo "METRIC name=resolve_s value=0"
        echo "RUN_FAILED=1"
        exit 1
    }

    RESOLVE_S=$(grep -iE '^[[:space:]]*Resolution[[:space:]]' "$OUT" | grep -oE '[0-9]+(\.[0-9]+)?' | head -1 || true)
    TOTAL_S=$(awk '/^real[[:space:]]/{print $2}' "$OUT")

    # A failed parse must stop the run. Reporting 0 for a metric where lower is better would
    # look like a perfect score and would win every comparison.
    if [ -z "$RESOLVE_S" ] || [ -z "$TOTAL_S" ]; then
        echo "error: could not read the timings out of the CLI output." >&2
        cat "$OUT" >&2
        rm -f "$OUT"
        echo "PARSE_FAILED=1"
        exit 1
    fi
    rm -f "$OUT"

    echo "# run $run: resolve_s=$RESOLVE_S total_s=$TOTAL_S"
    if [ -z "$BEST_RESOLVE" ] || awk "BEGIN{exit !($RESOLVE_S < $BEST_RESOLVE)}"; then
        BEST_RESOLVE="$RESOLVE_S"
        BEST_TOTAL="$TOTAL_S"
    fi
done

echo "METRIC name=resolve_s value=$BEST_RESOLVE"
echo "METRIC name=total_s value=$BEST_TOTAL"
