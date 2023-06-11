Superbol VSCode tasks
=====================

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
