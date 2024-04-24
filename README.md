# SuperBOL Studio OSS: A New Platform for COBOL

[![Actions Status](https://github.com/ocamlpro/superbol-studio-oss/workflows/Main%20Workflow/badge.svg)](https://github.com/ocamlpro/superbol-studio-oss/actions)
[![Release](https://img.shields.io/github/release/ocamlpro/superbol-studio-oss.svg)](https://github.com/ocamlpro/superbol-studio-oss/releases)

Visual Studio Code extension for COBOL.

## Getting started

### Requirements

> [!IMPORTANT]
>
> This extensions assumes that version 3.2 of
> [GnuCOBOL](https://sourceforge.net/projects/gnucobol/) is available
> on the system running VS Code.  Debug features additionally assume
> that [gdb](https://sourceware.org/gdb/) is installed.

### Installation

You can install SuperBOL Studio OSS either directly from within VS
Code, or via a VSIX file.

#### Installation within VS Code

First, click on the "Extensions" icon in the activity bar on the
left-hand side, or press
<kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>X</kbd>.  Then, type "superbol"
into the search bar to find it, and press its `Install` button.

You can find further instructions for installing extensions directly
within VS Code on [this
page](https://code.visualstudio.com/docs/editor/extension-marketplace).

#### Installation via a VSIX file

First, download a VSIX file from the
[Marketplace](https://marketplace.visualstudio.com/items?itemName=OCamlPro.SuperBOL)
or [Open VSX](https://open-vsx.org/extension/OCamlPro/SuperBOL).  

To install the extension from its VSIX file, open VS Code and go to
the the "Extensions" view.  In the sidebar, click on the three dots
(`⋅⋅⋅`) on the top right-hand side (just above `search`), select
`Install from VSIX…`.  Pick the VSIX file you just downloaded to
install it.

#### Using SuperBOL lsp with other editors

To use our lsp functionnality with other editors, check [this page](https://github.com/OCamlPro/superbol-studio-oss/wiki/SuperBOL-LSP).

### Configuration

<!-- #### Locating GnuCOBOL's configuration files -->

This extension should work out-of-the-box on Windows systems where
GnuCOBOL has been installed using one of the dedicated installers
(available **???**).  This should be the case with standard
installations on Linux as well—*e.g*, if GnuCOBOL is installed in
`/usr/local` or `/usr`.

<!-- See section [Troubleshooting](#troubleshooting) below in case the -->
<!-- extension reports errors that migth indicate if cannot find GnuCOBOL's -->
<!-- configuration files. -->

> [!NOTE]
> (Temporary)
>
> If the extension shows errors like "Configuration file … not found",
> you may need to set the `COB_CONFIG_DIR` environment variable to the
> directory shown by:
>
> ```shell
> cobc -i | grep COB_CONFIG_DIR
> ```
>
> You may need to restart VS Code (or your session) so the extension
> reloads properly.

### Editing an existing project

To start using the extension on an existing project, open its folder
in VS Code (`File > Add Folder to Workspace…`).  The extension will
start automatically whenever the folder contains files with usual
COBOL filename extensions (`.cob`, `.cbl`, `.cpy`, `.cbx`).

Then, open the settings (`File > Preferences > Settings`, or
<kbd>Ctrl</kbd>+<kbd>,</kbd>), and start typing "superbol…".  You will
be presented with a screen that resembles:

![SuperBOL settings](./assets/superbol-settings.png)

From here, you can notably configure:

- The COBOL dialect used in the project (see
  [here](https://get-superbol.com/gnucobol/manual/chapter2.html#configuration-options)
  for a documentation on every available dialect).  In SuperBOL, the
  `default` dialect corresponds to GnuCOBOL's default, that supports
  many features from dialects such as `COBOL2014`, `IBM`, `Micro
  Focus` (`mf`), or `GCOS` for instance;

- The default reference source-format (see
  [here](https://get-superbol.com/gnucobol/manual/chapter2.html#source-format)
  for a documentation on the supported source formats).  When `auto`
  is selected, which is the default, SuperBOL (and GnuCOBOL) will
  automagically try to guess whether the source is in `free` or
  `fixed` format.  Other source formats need to be configured
  explicitly.

- The path to copybooks.  To configure this setting, you will need to
  select `Edit in settings.json`.  This is a list, where each entry
  describes an element of the search path where your copybooks will be
  looked for.  Each entry must contain a directory name `dir`, and may
  feature an optional `file-relative` flag.  When absent or set to
  `false`, the latter field indicates that the directory name is
  either absolute, or relative to the root of the project's directory.
  When `file-relative` is `true`, the element of the search path is
  considered relative to the directory where each main source program
  is located.

  ![Editing copybook paths in `.vscode/settings.json`](./assets/superbol-editing-copybooks-path-in-vscode-settings.png)

## Syntax diagnostics

> [!NOTE]
>
> Syntax checks performed by SuperBOL Studio currently cover the
> `COBOL85` dialect, and some constructions of more recent dialects
> supported by GnuCOBOL.  Reporting of such diagnostics is currently
> disabled for dialects other than `COBOL85` to avoid misleading
> developers with false diagnostics about syntax errors.
>
> Reporting can be re-enabled for every dialect by setting the `Force
> Syntax Diagnostics` flag in SuperBOL configuration setting.

## Navigation features

### Go to Definition

When you want to locate the definition of a data item name in your
source code, position your cursor on its name, right click, and
select  `Go to Definition` (or press <kdb>F12</kbd>).

![Go to Definition](./assets/superbol-goto-definition.gif)

### Peek Definition

To only have a peek at where such a data item defined, you can
position the cursor on its name, right click, and select  `Peek > Peek
Definition` (or press
<kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>F10</kbd>).  You will then be
presented with a view of the location of the corresponding definition,
including if it lies in a copybook.

![Peek Definition in copybook](./assets/superbol-peek-at-definition-in-copybook.gif)

### Go to References

If you want to obtain a list of all references to a named data item,
right click and select  `Go to References` (or press
<kbd>Shift</kbd>+<kbd>F12</kbd>).  You will then view the location of
every reference to this item.

![Go to References](./assets/superbol-goto-references.gif)

> [!NOTE]
> (Temporary limitation)
>
> At the moment, definitions that belong to some sections of the data
> division, and some references to data items, are ignored by the
> extension.  They will be covered by the first stable release.

### Hover to Show Copybooks

Ever wondered what was behind a `COPY` directive?  Just position your
cursor over such a statement, and you will be presented with the
contents of the copybook.

![Hover over `COPY`](./assets/superbol-hover-copy.png)

### Hover to Show Source Text Replacements

What's more?  You can see the source text that results from
replacement by a `REPLACE` directive in the same way.

![Hover over replacement](./assets/superbol-hover-replacement.gif)

## Debugging

In order to debug a COBOL program, you first need to configure a
dedicated *build task* with appropriate debug options.  Once this is
done, you can *launch* the compiled program into a debugging session.

### Setting up a Build Task for Debugging

After having opened the program to debug, select `Terminal >
Run Build Task…`  (<kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>B</kbd>),
`Configure Build Task…`, and then `SuperBOL: build (debug)`.

![Select `SuperBOL: build (debug)`](./assets/superbol-configure-build-tasks.png)

Save the `task.json` as shown.  Definitions for this task notably
include a `for-debug` flag, that instructs the compiler to insert
debug annotations into generated executable files, as well as
`extra-args`, that can be edited to pass additional arguments to the
`cobc` compiler.

![`tasks.json` for debug](./assets/superbol-tasks.json.png)

### Launching the Compiled Program for Debugging

If needed, you can place a breakpoint on statements (or paragraph
titles in the procedure division) by clicking on the red dot that
appears when you hover the cursor on the left margin (or with
<kbd>F9</kbd>).  Click on the red dot or press <kbd>F9</kbd> again to
remove a breakpoint.  Then, to launch the program in debug mode,
select `Run > Start Debugging` (<kbd>F5</kbd>).  This will run your
program until a first breakpoint if reached, or to completion.

![Start Debugging](./assets/superbol-start-debugging.gif)

Once stopped on a breakpoint, you can investigate the values of data
items from the program using the `VARIABLES` panel on the left-hand
side.

Press <kbd>F10</kbd> to step to the next statement, or <kbd>F5</kbd>
again to continue until the next breakpoint, or termination of the
program.

<!-- ## Formatting the source code -->

<!-- > [!NOTE] -->
<!-- > -->
<!-- > Not working on the NIST-COBOL85 suite. -->

## Miscellaneous

### Collaborating with other developers

At this point, the settings for your project are stored and managed by
VS Code.  However, you may plan to collaborate with developers that do
not use this editor.  For instance, they might want to use our mode
for GNU/Emacs, which is located [here](./emacs/).  Then, we advise you
to let SuperBOL Studio store the configuration in a `superbol.toml`
file that will be located at the root of the project.

You can make the extension write your current project configuration
into a `superbol.toml` by entering the command palette (`View >
Command Palette…`, or press
<kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>P</kbd>), and then selecting the
command `SuperBOL: Write Project Configuration`.

![SuperBOL: Write Project Configuration](./assets/superbol-write-project-configuration-command-prompt.png)

This will save a `superbol.toml` file at the root of each currently
opened project directory.  Such a file will not store any
user-specific settings, so you can now safely put them under source
control.  Extensions dedicated to the edition of TOML files, such as
[`tamasfe.even-better-toml`](https://marketplace.visualstudio.com/items?itemName=tamasfe.even-better-toml),
provide the same level of assistance as when you edit
`.vscode/settings.json`.

> [!TIP]
>
> Install the
> [`OCamlPro.SuperBOL-studio-pack`](https://marketplace.visualstudio.com/items?itemName=OCamlPro.SuperBOL-studio-pack)
> extension to get SuperBOL Studio OSS and `tamasfe.even-better-toml`
> altogether.

![Editing `superbol.toml`](./assets/superbol-editing-superbol.toml.png)

### Further documentation

You can check further the documentation on using the extension on
[this page](https://ocamlpro.github.io/superbol-studio-oss/sphinx).

### Mode for GNU/Emacs

You can find [here](./emacs) a mode that allows you to use the
SuperBOL LSP server with GNU/Emacs.

### Building from sources

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
   LSP server along with the VS Code extension:

   ```bash
   make build-deps vsix-release
   ```

## Resources

* Website: https://ocamlpro.github.io/superbol-vscode-platform
* General Documentation: https://ocamlpro.github.io/superbol-vscode-platform/sphinx
* Sources: https://github.com/ocamlpro/superbol-vscode-platform
