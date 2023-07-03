Superbol extension features
===========================

Go to definition
----------------

You can navigate to the defintion of any variable in your code. Push :code:`F12` on a variable name to go
to it's defintion. This works on:
* every variable in the :code:`PROCEDURE DIVISION`,
* every variable that is on a :code:`RENAME` clause,
* every variable that is used by a :code:`REDEFINES` clause

Go to references
----------------

You can have a list of references of every variables in your code. Push :code:`Shift + F12` on a variable
name to find all the places it is referenced.

Semantic highlight
------------------

The extension understands your code on a semantic level, giving you better highlighting to make
reading your code easier and focus on the important information of the source code.

Peek copy
---------

Place your cursor on :code:`COPY` statement and a pop up with the content of the copied file will
appear, giving you all the information you need to understand the code at a glance.

Indentation
-----------

You can indent your code for a better understanding of the control flow at a glance. Simply push
:code:`Ctrl + Shift + I` and all the file is indented.

You can also just select a part of your code and push the same shortcut just to indent the selection.

Keyboard shortcuts
------------------

========================================= ===============================================================================================================
Shortcut                                  Action
========================================= ===============================================================================================================
:code:`F12`                               Go to the defintion of the item under cursor
:code:`Shift + F12`                       | List all references of the item under cursor.
                                          If there is only one other reference then navigate directly to it
:code:`Ctrl + Shift + I` (no selection)   Format the whole file
:code:`Ctrl + Shift + I` (with selection) Format the selection
:code:`Mouse hover`                       If on a :code:`COPY` statement peek the content of the copied file.
========================================= ===============================================================================================================

