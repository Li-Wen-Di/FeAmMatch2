#!/usr/bin/env bash
set -euo pipefail
example="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
root="$(cd -- "$example/../.." && pwd -P)"
kernel="${FEAMMATCH2_WOLFRAM:-WolframKernel}"
if [[ -z "${FM2_TOOLCHAIN:-}" && -f "$root/config/toolchain.local.json" ]]; then
    export FM2_TOOLCHAIN="$root/config/toolchain.local.json"
fi
if [[ -n "${FM2_OUTPUT:-}" ]]; then
    mkdir -- "$FM2_OUTPUT"
    export FM2_OUTPUT="$(cd -- "$FM2_OUTPUT" && pwd -P)"
else
    export FM2_OUTPUT="$(mktemp -d "$example/output/run-XXXXXXXX")"
fi
export FM2_EXAMPLE_DIR="$example"
exec "$kernel" -script "$root/src/wolfram/run-example.wls"
