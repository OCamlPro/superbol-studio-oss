#!/bin/sh
set -eu

if ! command -v nix 2>&1 >/dev/null
then
  echo "GixSQL tests ignored. Install nix to run them."
  exit 0
fi

cd "import/gixsql"

# Run tests with the legacy preprocessor
# LEGACY_PP="" nix develop --command ./run_test.sh

# Run tests with the new preprocessor
export TEST_VERBOSITY=1
SUPERBOL_PP="/home/tiky/git/superbol-studio-oss/_build/install/default/bin/superbol-free" \
  nix develop --command ./run_test.sh
