#!/bin/bash

set -euvxo pipefail

echo Running "${BASH_SOURCE[0]}"

cd "$1";
./build_aux/bootstrap
mkdir -p _build
cd _build
../configure --prefix="$2" \
    --with-db \
    CFLAGS="-std=c11"
make --jobs=$(($(nproc)+1))
make install

echo Done with "${BASH_SOURCE[0]}"
