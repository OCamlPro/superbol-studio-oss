Building SuperBOL from Sources
==============================

If you build from a clone of the git repository, make sure to update submodules:

   .. code-block:: shell

     git submodule update --init --recursive

To build the LSP server and the VSCode extension from source, you need
to install a few external dependencies.

1. First, you need to install and initialize `opam`_, the package
   manager for OCaml;

2. Then you need a recent version [#drom-version]_ of our build tool
   `drom`_.  The easiest way to have it running is via the following
   command:

   .. code-block:: shell
   
      opam pin add https://github.com/OCamlPro/drom.git

3. Install `node.js`_ (version >=5.2.0) if it is not already installed.

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

.. rubric:: Footnotes

.. [#drom-version] The version needed to build the first α release of
                   SuperBOL is 0.9.2~dev3 (commit 63a5770).
