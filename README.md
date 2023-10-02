[![Actions Status](https://github.com/ocamlpro/superbol-studio-oss/workflows/Main%20Workflow/badge.svg)](https://github.com/ocamlpro/superbol-studio-oss/actions)
[![Release](https://img.shields.io/github/release/ocamlpro/superbol-studio-oss.svg)](https://github.com/ocamlpro/superbol-studio-oss/releases)

# Superbol Studio OSS: A New Platform for COBOL

## Features

* LSP (`superbol-free`) with the following capabilities:
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

### From binary and VSIX

Get the `superbol` executable and install it in `/usr/local/bin`.

Get the `superbol-vscode-platform.vsix` file from the releases.

In VSCode open the `extension` view and select the three dots on the top right of the sidebar.

Select `Install from VSIX ...` and select the `superbol-vscode-platform.vsix` file.

### Build

Get the `superbol` executable and install it in `/usr/local/bin`.

Clone the source code of this extension:
```bash
git clone https://github.com/OCamlPro/superbol-vscode-platform.git
```

Go to the created folder and install the dependencies with
```bash
cd superbol-vscode-platform
yarn install
```

Finally build the extension with:
```bash
make vsix-debug
```

### Add the extension to VSCode

Open VSCode and go to the extension view.

In the sidebar click on the three dots on the top right (just above `search`) and select
`install from VSIX ...` and select the `superbol-vscode-platform.vsix` generated from
this extension.

### Configure the extension

If you have installed the `superbol` executable in `/usr/local/bin` then you have nothing to do,
the extension will work out of the box.

Otherwise get the path to the `superbol` executable and copy it.

Go to your VSCode settings and in the extension submenu select `Superbol COBOL`.

In the `superbol` field past the path to the `superbol` executable.

You can check the documentation on using the extension on [this page](https://ocamlpro.github.io/superbol-vscode-platform/sphinx).

## GNU/Emacs mode

You can check the documentation on using the Superbol LSP with GNU/Emacs on [this page](https://ocamlpro.github.io/superbol-vscode-platform/sphinx/emacs.html).

## Resources

* Website: https://ocamlpro.github.io/superbol-vscode-platform
* General Documentation: https://ocamlpro.github.io/superbol-vscode-platform/sphinx
* Sources: https://github.com/ocamlpro/superbol-vscode-platform


