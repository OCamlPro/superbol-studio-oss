[![Actions Status](https://github.com/ocamlpro/superbol-studio-oss/workflows/Main%20Workflow/badge.svg)](https://github.com/ocamlpro/superbol-studio-oss/actions)
[![Release](https://img.shields.io/github/release/ocamlpro/superbol-studio-oss.svg)](https://github.com/ocamlpro/superbol-studio-oss/releases)

# Superbol Studio OSS: A New Platform for COBOL

## Features

* LSP server (`superbol-free`), with the following capabilities:
    * Syntax diagnostics
    * Go to definitions
    * Find references
    * Peek on copybook and source text replacements
    * Folding of whole divisions, sections, and paragraphs
    * Semantic highlighting
    * File and range indentation

* VSCode extension

* GNU/Emacs mode

## VSCode Extension

The VSCode extension is bundled in a VSIX file, and makes use of the
LSP server `superbol-fee`.  The latter is an executable that needs to
be available to use the extension.

### Downloading binary releases

You can download both the `superbol-fee` executable and the VSIX file
from the releases page[^releases].  Save the former in a directory
that is in already `PATH` so the extension works out of the box.

[^releases]: Available soon.

### Adding the extension to VSCode

Open VSCode and open the extensions view.

In the sidebar, click on the three dots (`⋅⋅⋅`) on the top right-hand
side (just above "`search`").

Select "`Install from VSIX…`" and pick
`superbol-vscode-platform.vsix`.

### Configuring the extension

If you have installed the `superbol-free` executable in a directory
that is already in `PATH`, then you have nothing to do, the extension
will work out of the box.

Otherwise get the path to the `superbol` executable and copy it.

Go to your VSCode settings and in the extension submenu select
`Superbol COBOL`.

In the `superbol` field, past the path to the `superbol-free`
executable.

You can check the documentation on using the extension on [this
page](https://ocamlpro.github.io/superbol-studio-oss/sphinx).

## GNU/Emacs mode

You can check the documentation on using the Superbol LSP with
GNU/Emacs on [this
page](https://ocamlpro.github.io/superbol-studio-oss/sphinx/emacs).

## Building from Sources

You first need to install a few external dependencies to build the LSP server and the VSCode extension from sources.

1. First, you need to install and initialize [opam](https://opam.ocaml.org/);

1. Then you need a decent version of our build tool [drom](https://ocamlpro.github.io/drom/).  The easiest way to have it running is via the following command:

   ```bash
   opam pin add https://github.com/OCamlPro/drom.git
   ```

1. To build the VSCode extension, you also need to have [node.js](https://nodejs.org/), and install `yarn` via:

   ```bash
   npm install yarn
   ```

1. After that, running `make` in the package's root directory should compile the LSP server, along with the VSCode extension.

## Resources

* Website: https://ocamlpro.github.io/superbol-vscode-platform
* General Documentation: https://ocamlpro.github.io/superbol-vscode-platform/sphinx
* Sources: https://github.com/ocamlpro/superbol-vscode-platform


