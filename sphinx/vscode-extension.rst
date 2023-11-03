.. _superbol-vscode-extension:

SuperBOL VSCode Extension
=========================

**TODO:** Update this Section if still relevant; remove it otherwise.

This VSCode extension is used to interface VSCode with the SuperBOL LSP Server and provides building
tasks for your COBOL projects.

The extension can be used as a standalone, but is required for the `SuperBOL VSCode Extension` to
work.

Tasks
-----
We provide a task definition to build your COBOL files.
The task is defined as follow:

.. code-block:: json

    {
        "type": "superbol",
        "copybooks": [],
        "sourceFormat": "",
        "dialect": "",
        "extensions": [],
        "forDebugging": false,
        "problemMatcher": [],
        "label": "superbol: Build file",
        "group": {
            "kind": "build",
            "isDefault": true
        }
    }

You can fill the fields with the following values:

* :code:`copybooks`: an array of string, each string must point a to a directory with copybooks
  inside
* :code:`sourceFormat`: a string for the COBOL source format to be used by the compiler
* :code:`dialect`: the dialect to be used by the compiler
* :code:`extensions`: file extensions to resolve COPY (without the leading :code:`.`)
* :code:`forDebugging`: if true then the compiler is :code:`cobcd`, otherwise :code:`cobc`.

To trigger the building task just push :code:`Ctrl + Shift + B`.

Settings
--------

The settings for the extension are to be modified by going to :code:`File` > :code:`Preferences` >
:code:`Settings`.

Then select the :code:`Extension` submenu and select :code:`SuperBOL COBOL`.

:code:`Path`
^^^^^^^^^^^^

This is the path to the :code:`superbol` executable.

.. _superbol-vscode-features:

SuperBOL extension features
---------------------------

Go to definition
^^^^^^^^^^^^^^^^

You can navigate to the defintion of any variable in your code. Push :code:`F12` on a variable name to go
to it's defintion. This works on:
* every variable in the :code:`PROCEDURE DIVISION`,
* every variable that is on a :code:`RENAME` clause,
* every variable that is used by a :code:`REDEFINES` clause

Go to references
^^^^^^^^^^^^^^^^

You can have a list of references of every variables in your code. Push :code:`Shift + F12` on a variable
name to find all the places it is referenced.

Semantic highlighting
^^^^^^^^^^^^^^^^^^^^^

The extension understands your code on a semantic level, giving you better highlighting to make
reading your code easier and focus on the important information of the source code.

Peek copy
^^^^^^^^^

Place your cursor on :code:`COPY` statement and a pop up with the content of the copied file will
appear, giving you all the information you need to understand the code at a glance.

Indentation
^^^^^^^^^^^

You can indent your code for a better understanding of the control flow at a glance. Simply push
:code:`Ctrl + Shift + I` and all the file is indented.

You can also just select a part of your code and push the same shortcut just to indent the selection.

Keyboard shortcuts
^^^^^^^^^^^^^^^^^^

========================================= ===============================================================================================================
Shortcut                                  Action
========================================= ===============================================================================================================
:code:`F12`                               Go to the defintion of the item under cursor
:code:`Shift + F12`                       List all references of the item under cursor.
                                          If there is only one other reference then navigate directly to it
:code:`Ctrl + Shift + I` (no selection)   Format the whole file
:code:`Ctrl + Shift + I` (with selection) Format the selection
:code:`Mouse hover`                       If on a :code:`COPY` statement peek the content of the copied file.
========================================= ===============================================================================================================

