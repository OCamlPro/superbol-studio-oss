#!/bin/bash

# Builds and installs GnuCOBOL (3.x or 4.x) from the sources in directory
# $1, into prefix $2.  Any further arguments are passed to `configure`,
# after the default options (so `--disable-hardening` overrides
# `--enable-hardening`).

set -euvxo pipefail

echo Running "${BASH_SOURCE[0]}"

CFLAGS="${CFLAGS:-}"
CPPFLAGS="${CPPFLAGS:-}"

SRCDIR="$1"
PREFIX="$2"
shift 2

cd "$SRCDIR";
case $(uname) in
    Linux)
        ./build_aux/bootstrap install
        mkdir -p _build
        cd _build
        ../configure --prefix="$PREFIX" \
                     --enable-cobc-internal-checks \
                     --enable-hardening \
                     --with-db \
                     "$@" \
                     CFLAGS="-std=c11 ${CFLAGS}"
        make --jobs=$(($(nproc)+1))
        make install;;
    Darwin)
        ./build_aux/bootstrap install
        gettextize -f
        mkdir -p _build
        cd _build
        ../configure --prefix="$PREFIX" \
                     --enable-cobc-internal-checks \
                     --enable-hardening \
                     --with-db \
                     "$@" \
                     --with-curses=ncurses \
                     CFLAGS="-std=gnu17 ${CFLAGS}" \
                     CPPFLAGS="-DREAD_WRITE_NEEDS_FLUSH ${CPPFLAGS}"
        make --jobs=$(($(sysctl -n hw.ncpu)+1))
        make install;;
    *)
        echo "Unexpected system: $(uname -a)";
        exit 1;;
esac

echo Done with "${BASH_SOURCE[0]}"
