#!/usr/bin/env bash
set -euo pipefail
here="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
exec "${FEAMMATCH2_JULIA:-julia}" --startup-file=no --project="$here" "$@"
