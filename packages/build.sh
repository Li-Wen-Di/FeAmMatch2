#!/usr/bin/env bash
set -euo pipefail
here="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
exec "${FEAMMATCH2_PYTHON:-python3}" "$here/install-tools.py"
