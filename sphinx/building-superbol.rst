Building SuperBOL from Sources
==============================

To build the LSP server and the VSCode extension from source, you need
to install a few external dependencies.

1. First, you need to install and initialize `opam`_, the package
   manager for OCaml;

2. Then you need a recent version of our build tool `drom`_.  The
   easiest way to have it running is via the following command:

   .. code-block:: shell
   
      opam pin add https://github.com/OCamlPro/drom.git

3. To build the VSCode extension, you also need to have `node.js`_,
   and install and initialize `yarn` via:

   .. code-block:: shell
   
      npm install yarn
      yarn install

4. You can then install all remaining dependencies, and compile the
   LSP server along with the VSCode extension:

   .. code-block:: shell
   
      make build-deps vsix-release

You should obtain a file :code:`superbol-vscode-platform.vsix` in the
project's root directory.  Follow the instructions at
:ref:`install-superbol-vsix` to install it in VSCode.

.. links:

.. _opam: https://opam.ocaml.org/
.. _drom: https://github.com/OCamlPro/drom/
.. _node.js: https://nodejs.org/
