SuperBOL mode for GNU/Emacs
===========================

We provide an extension of the :code:`cobol-mode.el` of ELPA under the
name :code:`cobol-superbol-mode.el` that provides some fixes and run
with our LSP server for COBOL.

Standard file :code:`cobol-superbol-mode.el`
--------------------------------------------

We provide :code:`cobol-superbol-mode.el` as a fork of
:code:`cobol-mode.el` from ELPA, with a few fixes:

* Better indentation
* Minor modes to switch between formats
* Documentation accessible from the COBOL menu

Installation
~~~~~~~~~~~~

If you are already using the Cobol mode by ELPA, you need to
desinstall it. For that, use `M-x list-packages`, go to `cobol-mode`,
type `D` to select it for deletion, and then `x` to execute the
removal.

Copy the file :code:`cobol-superbol-mode.el` to your
:code:`$HOME/.emacs.d/lisp/` directory::

  wget https://github.com/OCamlPro/superbol-studio-oss/raw/master/emacs/cobol-superbol-mode.el
  wget https://github.com/OCamlPro/superbol-studio-oss/raw/master/emacs/cobol-superbol-indent.el
  mkdir -p $HOME/.emacs.d/lisp/
  mv cobol-superbol-*.el $HOME/.emacs.d/lisp/

and add the following lines to your :code:`$HOME/.emacs` file::

  (add-to-list 'load-path "~/.emacs.d/lisp/")
  (autoload 'cobol-superbol-mode "cobol-superbol-mode")
  (setq cobol-tab-width 3)
  (setq auto-mode-alist
    (append
     '(("\\.cob\\'" . cobol-superbol-mode)
       ("\\.cbl\\'" . cobol-superbol-mode)
       ("\\.cpy\\'" . cobol-superbol-mode))
     auto-mode-alist))
  (require 'cobol-superbol-indent.el)

Start a new Emacs process and open a COBOL file: verify that the
`COBOL` menu is available and contains a `SuperBOL by OCamlPro`
entry. You are done!

This configuration will set tabulations to be 3 spaces and free source
format. If you want to change the source format, you will need to
change this option using :code:`M-x customize`, save and then restart emacs.

Features
~~~~~~~~

The :code:`cobol-superbol-mode.el` provides the following features:

* colorization
* indentation
* comments
* rulers
* minor-modes for source formats
* a COBOL menu with:

  * an "Insert" item with a few constructions
  * a sub-menu with links to GnuCOBOL documentation
  * a sub-menu to switch between source formats

Customization
~~~~~~~~~~~~~

* cobol-skeleton-alist (check the `cobol--def-skeleton` function)

We advise to also use the :code:`auto-complete` mode also. This mode
will propose completions while typing keywords (use TAB or RET to
complete).

Superbol-mode with LSP
----------------------

The new Superbol-mode provides an IDE that makes use of the SuperBOL
LSP to provide advanced navigation and editing facilities for COBOL
projects.  It can be used in combination with any of the two main LSP
clients that exist within the GNU/Emacs ecosystem to interact with LSP
servers: `lsp-mode` and `eglot`.

Superbol-mode with `lsp-mode`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`lsp-mode`_ appears to be the most prominent at the moment.  This
client benefits from a large user-base, but is also considered
"bloated" by some.

The main advantages for using it in our context is its support for
`semantic tokens`_, that provide a way for LSPs to issue information
about the semantics of symbols from the source code.  Compared to
traditional regexp-based highlighting, semantic tokens-based
highlighting can drastically improve code readability of various
source code elements based on their syntactic location;

.. _lsp-mode: https://github.com/emacs-lsp/lsp-mode
.. _semantic tokens:
    https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide


Superbol-mode with `eglot`
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Another possibility is to use `eglot`_, that is sometimes considered
easier to configure and more lightweight than `lsp-mode` (which
notably makes it more reactive to user inputs).  Being more recent, it
lacks some of the features of `lsp-mode`, among which is the support
for semantic tokens [#eglot-semtok-issue]_.  However, additionally
enabling the aforementioned `cobol-mode.el` provides reasonable syntax
highlighting.

.. _eglot: https://elpa.gnu.org/packages/eglot.html

Setup
~~~~~

Let us first define an environment variable that indicates where the
``superbol-free`` executable can be found.  Additionally, define a
variable that points to the root of the source directory for the
extension:

.. code-block:: shell

   export SUPERBOL_DIR="<directory where superbol-free can be found>";
   export SUPERBOL_STUDIO_OSS_DIR="$PWD";

After this, the following command launches a GNU/Emacs instance with
an `lsp-mode`-based client configured for COBOL files:

.. code-block:: shell

   emacs -L "$SUPERBOL_STUDIO_OSS_DIR/emacs" \
         --load lsp-superbol \
         --eval "(custom-set-variables '(lsp-superbol-path \"$SUPERBOL_DIR\"))" \
         --funcall superbol-mode-enable-for-default-extensions

To use `eglot`, type the following instead:

.. code-block:: shell

   emacs -L "$SUPERBOL_STUDIO_OSS_DIR/emacs" \
         --load eglot-superbol \
         --eval "(add-to-list 'exec-path \"$SUPERBOL_DIR\")" \
         --funcall superbol-mode-enable-for-default-extensions

Further configuration for auto-indentation:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`lsp-mode` provides a ``lsp-format-region`` function that may be used
to use the LSP-provided intentation.  When using `eglot`, the same
functionality is provided by ``eglot-format``.

.. [#eglot-semtok-issue] Note there is a pending issue on this point
   at https://github.com/joaotavora/eglot/issues/615 .

Known Issues
~~~~~~~~~~~~

* `void-function -compose`: install the `dash` package on ELPA
* Install `yasnippet` ?
