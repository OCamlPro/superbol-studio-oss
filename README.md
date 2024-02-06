[![Actions Status](https://github.com/ocamlpro/superbol-studio-oss/workflows/Main%20Workflow/badge.svg)](https://github.com/ocamlpro/superbol-studio-oss/actions)
[![Release](https://img.shields.io/github/release/ocamlpro/superbol-studio-oss.svg)](https://github.com/ocamlpro/superbol-studio-oss/releases)

# SuperBOL Studio OSS: A New Platform for COBOL

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
LSP server `superbol-free`.  The latter is an executable file that
needs to be downloaded separately and made available on the system to
use the extension.

### Downloading binary releases

You can download both the `superbol-free` executable and the VSIX file
from the releases page[^releases].  Save the former in a directory
that is already in the `PATH` so the extension works out of the box.

[^releases]: Available soon.

### Adding the extension to VSCode

To install the extension from its VSIX file, open VSCode and go to the
the "`Extensions`" view.

In the sidebar, click on the three dots (`⋅⋅⋅`) on the top right-hand
side (just above "`search`"), select "`Install from VSIX…`".  Pick the
`superbol-vscode-platform.vsix` file to install it.

### Configuring the extension

If you have installed the `superbol-free` executable in a directory
that is already in `PATH`, then you have nothing to do, the extension
will work out of the box.

Go to your VSCode settings and in the extensions submenu, select
`SuperBOL Studio OSS`, and then `Extension Settings`.  Fill in the
`superbol` field with the path to the `superbol-free` executable.

You can check the documentation on using the extension on [this
page](https://ocamlpro.github.io/superbol-studio-oss/sphinx).

## GNU/Emacs mode

You can check the documentation on using the Superbol LSP with
GNU/Emacs on [this
page](https://ocamlpro.github.io/superbol-studio-oss/sphinx/emacs).

## Building from sources

If you build from a clone of the git repository, make sure to update submodules:
```bash
git submodule update --init --recursive
```

You first need to install a few external dependencies to build the LSP
server and the VSCode extension from sources.

1. First, you need to install and initialize
   [opam](https://opam.ocaml.org/);

2. Then you need a recent version[^drom-version] of our build tool
   [drom](https://ocamlpro.github.io/drom/).  The
   easiest way to have it running is via the following command:

   ```bash
   opam pin add https://github.com/OCamlPro/drom.git
   ```

   [^drom-version]: Current version is 0.9.2~dev3 (commit 63a5770).

3. Install [node.js](https://nodejs.org/) (version >=5.2.0) if it
   is not already installed.

4. You can then install all remaining dependencies, and compile the
   LSP server along with the VSCode extension:

   ```bash
   make build-deps vsix-release
   ```

## Resources

* Website: https://ocamlpro.github.io/superbol-vscode-platform
* General Documentation: https://ocamlpro.github.io/superbol-vscode-platform/sphinx
* Sources: https://github.com/ocamlpro/superbol-vscode-platform
