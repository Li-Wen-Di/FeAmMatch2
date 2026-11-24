#!/usr/bin/env bash
set -euo pipefail
here="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
export FM2_ROOT="$(cd -- "$here/../.." && pwd -P)"
export FM2_TOOLCHAIN="${FM2_TOOLCHAIN:-$FM2_ROOT/config/toolchain.local.json}"
kernel="${FEAMMATCH2_WOLFRAM:-WolframKernel}"
output="${1:-$(mktemp -d "${TMPDIR:-/tmp}/fm2-check-XXXXXXXX")}"
mkdir -p -- "$output"
output="$(cd -- "$output" && pwd -P)"
"${FEAMMATCH2_PYTHON:-python3}" "$here/helper_contracts.py" -o "$output/helper-contracts.json"
for check in inputs planner new_channels basis_conversion performance single_flow distinct_masses local_optimizations; do
    export FM2_OUTPUT="$output/$check"
    "$kernel" -script "$here/$check.wl"
done
