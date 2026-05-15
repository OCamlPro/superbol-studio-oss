#!/bin/bash

set -euvxo pipefail

echo Running "${BASH_SOURCE[0]}"

# Requires package `gcc-mingw-w64-x86-64` on Debian and its derivatives

opam repository add windows https://github.com/ocamlpro/opam-cross-windows.git
# TOOLPREF64=x86_64-w64-mingw32.static-
opam install ocaml-windows conf-gcc-windows64
￼

echo Done with "${BASH_SOURCE[0]}"
