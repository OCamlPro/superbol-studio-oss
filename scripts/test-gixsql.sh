#!/bin/sh
set -eu

if ! command -v nix >/dev/null 2>&1
then
  echo "GixSQL tests ignored. Install nix to run them."
  exit 0
fi

cd "import/gixsql"

# Run tests with the legacy preprocessor
# LEGACY_PP="" nix develop --command ./run_test.sh

# Run tests with the new preprocessor
export TEST_VERBOSITY=1
nix develop --command ./run_test.sh
