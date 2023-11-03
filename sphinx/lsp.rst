SuperBOL LSP Server
===================

*SuperBOL* provides an LSP server for COBOL. Every dialect supported by superbol is supported by the LSP server.

The server operates on the notion of *projects*, that bundle COBOL source files within a *root directory*, along with a configuration.

Project layout
--------------

A project layout associates a configuration given in a file named :file:`superbol.toml` that is located at the root of the project, with a set of files that contain COBOL source code.  A typical structure for a project may look as follows:

.. code-block:: text

    project
    ├── superbol.toml
    ├── src
    │  └── prog1.cob
    │  └── prog2.cob
    │  └── ...
    └── COPY
       └── copy1.cpy
       └── copy2.cpy
       └── ...

When a user opens any file that contains COBOL source code, the LSP server looks for the closest parent directory :file:`dir` that directly contains a configuration file :file:`superbol.toml`, and sets :file:`dir` as the root project directory for that file.  If no configuration file is found, the project's root is defined as the directory that directly contains the opened file.

.. note::
   Note that files from distinct projects may be edited all at once via the same LSP server.


Project configuration
---------------------

The :file:`superbol.toml` at the root of a project is a `TOML`_ file with a :code:`[cobol]` secton that defines the following configuration fields for the project:

* :code:`dialect`: Sets the dialect for your project.  Case insensitive. Possible values are:

  * :code:`"default"` (default if not provided)
  * :code:`"gnucobol"`
  * :code:`"cobol85"`
  * :code:`"cobol2002"`
  * :code:`"cobol2014"`
  * :code:`"acu"`
  * :code:`"bs2000"`
  * :code:`"gcos"`
  * :code:`"ibm"`
  * :code:`"microfocus"` or :code:`"mf"`
  * :code:`"mvs"`
  * :code:`"realia"`
  * :code:`"rm"`
  * :code:`"xopen"`

  Every dialect strings from :code:`"acu"` to :code:`"rm"` (included) may be suffixed with :code:`"-strict"` to indicate a *strict* configuration for the dialect should be used: such a configuration restricts the set of reserved words and disables GnuCOBOL extensions to match the language expected by other compilers for the dialect.

* :code:`source-format`: Select a specific COBOL source format. Possible values are:

  * :code:`"Auto"` (default)
  * :code:`"Free"`
  * :code:`"Fixed"`
  * :code:`"Variable"`
  * :code:`"XOpen"`
  * :code:`"xCard"`
  * :code:`"CRT"`
  * :code:`"Terminal"`
  * :code:`"COBOLX"`

* :code:`copybook`: This array is to be filled with all the directories where copybooks are located.  It only contains the current working directory by default.  Each element of the array must have a :code:`dir` field.  This field may contain the name of a directory that is either:
  1. relative to the project root; or
  2. relative to the directory that contains the COBOL source file that defines the compilation group (when the associated Boolean field :code:`file-relative` holds).

Every value given as a string for a configuration field is case insensitive, except when it describes a file or directory name.

.. note::

   The :code:`superbol-free project init` command enables you to automatically create a :file:`superbol.toml` file with the default configuration.  You can append a directory argument to indicate where the file should be placed; otherwise, the file is created in the current working directory.

Example configuration
^^^^^^^^^^^^^^^^^^^^^

Consider the following project layout:

.. code-block:: text

    project
    ├── superbol.toml
    ├── GLOBAL_COPYBOOKS
    │  └── global.cpy
    └── src
       ├── prog1
       │  ├── LOCAL_COPYBOOKS
       │  │  └── local.cpy
       │  └── prog1.cob
       └── prog2
          ├── LOCAL_COPYBOOKS
          │  └── local.cpy
          └── prog2.cob


Then you can provide the following configuration file:

.. code-block:: toml
   :caption: :file:`superbol.toml`

   [cobol]
   dialect = "GCOS"
   source-format = "COBOLX"

   [[cobol.copybook]]
   dir = "GLOBAL_COPYBOOKS"

   [[cobol.copybook]]
   dir = "LOCAL_COPYBOOKS"
   file-relative = true

All COBOL code in this project will be considered in GCOS dialect and written in COBOLX source format.

In addition, the copybook :file:`global.cpy` can be used by any source file from this project.
Furthermore, a :code:`COPY "local.cpy"` in :file:`prog1.cob` will include the copybook :file:`src/prog1/LOCAL_COPYBOOKS/local.copy` (and respectively for :file:`prog2.cob` and :file:`src/prog2/LOCAL_COPYBOOKS/local.copy`).

Server capabilities
-------------------

This is a list the LSP server's capabilities.  To see how they are used in VSCode, you
can check :ref:`superbol-vscode-features`.

Go to definition
^^^^^^^^^^^^^^^^

The server handles the :code:`textDocument/definition` request, and can find definition of any
data item in your code.

Find references
^^^^^^^^^^^^^^^

The server can list all the references to a data item in your code with the :code:`textDocument/references`
request

Code formatting
^^^^^^^^^^^^^^^

The server provides a formatter both for the full file or for a selection range. This formatter handles
the :code:`FIXED` and :code:`FREE` format.

Hover
^^^^^

The server provides a way to peek into copybooks via the :code:`textDocument/hover` request (more
hovering features to come)

Semantic tokens
^^^^^^^^^^^^^^^

The server can provide semantic tokens data via the :code:`textDocument/semanticTokens/full` request.

.. _TOML: https://toml.io/
