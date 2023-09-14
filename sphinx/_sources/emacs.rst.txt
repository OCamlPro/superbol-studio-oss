Emacs modes
===========

Standard file :code:`cobol-mode.el`
-----------------------------------

We provide our own fork of :code:`cobol-mode.el` from ELPA, with a fix
on the indentation function preventing insertion of spaces at point.

Installation
~~~~~~~~~~~~

Copy the file :code:`cobol-mode.el` to your :code:`$HOME/.emacs.d/`
directory, and add the following lines to your :code:`$HOME/.emacs` file::

  (autoload 'cobol-mode "cobol-mode")
  (setq cobol-tab-width 3)
  (setq auto-mode-alist
  (append
     '(("\\.cob\\'" . cobol-mode)
       ("\\.cbl\\'" . cobol-mode)
       ("\\.cpy\\'" . cobol-mode))
     auto-mode-alist))
  (setq cobol-source-format 'free)

This configuration will set tabulations to be 3 spaces and free source
format. If you want to change the source format, you will need to
change this option using :code:`M-x customize`, save and then restart emacs.

Features
~~~~~~~~

The :code:`cobol-mode.el` provides a following features:

* colorization
* indentation
* comments
* rulers
* a COBOL menu with an item "Insert" with a few constructions

Customization
~~~~~~~~~~~~~

* cobol-skeleton-alist (check the `cobol--def-skeleton` function)

We advise to also use the :code:`auto-complete` mode also. This mode
will propose completions while typing keywords (use TAB or RET to
complete).
