Install VSCode extensions
=========================

From marketplace
----------------

TODO

From :code:`.vsix` files
------------------------

You can build the vsix files from source. For :code:`superbol-vscode-platform`, run the following
commands:

.. code-block:: shell

    yarn install #if not already done
    drom build
    make compile
    yarn package

For :code:`superbol-vscode-debug`, run the follwing command:

.. code-block:: shell

    yarn compile
    vsce package

With those commands you should have respectively :code:`superbol-vscode-platform.vsix` and
:code:`superbol-vscode-debug.vsix` in the project root directory.

To install them open VSCode and go the the :code:`Extensions` view.

Click on the :code:`...` at the top right of the left pane and select :code:`Install from VSIX ...`.

Select the :code:`superbol-vscode-debug.vsix` file first to install it.

Finally select the :code:`superbol-vscode-platform.vsix` file to install it.
