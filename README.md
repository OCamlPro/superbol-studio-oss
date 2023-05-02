# Superbol VSCode Extension for COBOL

## Features

* None (for now...)

## Resources

* Website: https://ocamlpro.github.io/superbol-vscode-extension
* General Documentation: https://ocamlpro.github.io/superbol-vscode-extension/sphinx
* Sources: https://github.com/ocamlpro/superbol-vscode-extension

## Test of Indentation

* I add two versions of `indentRange` in src/superbol-vscode/superbol_vscode_extension.ml. One uses Cobol_indentation as library, the other runs superbol as childprocess to get the result. To use Cobol_indentation as library, please add "padbol" into the dirs of the file dune of the main folder, and add "Cobol_indentation" as a dependency in src/superbol-vscode/package.toml.


