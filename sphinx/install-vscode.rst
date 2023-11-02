Install VSCode extensions
=========================

From marketplace
----------------

TODO

From :code:`.vsix` files
------------------------

You can build the VSIX files from source.  For :code:`superbol-studio-oss`, run the following
commands:

.. code-block:: shell

    yarn install #if not already done; note you need yarn version < 2
    drom build
    make compile
    yarn package

With those commands you should obtain a file
:code:`superbol-vscode-platform.vsix` in the project's root directory.

To install it, open VSCode and go the the :code:`Extensions` view.

Click on the :code:`⋅⋅⋅` at the top right of the left pane and select :code:`Install from VSIX…`.

Select the :code:`superbol-vscode-platform.vsix` file to install it.
