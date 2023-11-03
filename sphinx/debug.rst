SuperBOL VSCode Debug
=====================

This extension can be used to debug and check the coverage of your
COBOL code. It must be used in combination with the `SuperBOL VSCode
Extension` to recognize the COBOL files.

Usage
-----

If you do not already have a configuration, go to the :code:`Run and Debug` pane (:code:`Ctrl + Shift + D`),

Click on :code:`Show all automatic debug configurations` and select :code:`Add configuration...`
and :code:`COBOL Debugger`.

This will add the necessary configurations to your :code:`launch.json` file (or create it if needed).

Once you have your debugging configuration (see :ref:`configuration`), you can launch debugging
by pressing :code:`F5` while being in the COBOL file you wish to debug.

TODO: screenshots.

Extension settings
------------------

These settings are to be modified by going to :code:`File` > :code:`Preferences` > :code:`Settings`

Then select the :code:`Extensions` submenu and select :code:`SuperBOL Debugger`.

:code:`cobcpath` [1]_
^^^^^^^^^^^^^^^^^^^^^

This is the path to the :code:`cobc` executable, which will be used to build your application.

Default is :code:`cobc`.

:code:`gdbpath`
^^^^^^^^^^^^^^^

This is the path to the :code:`gdb` executable, which will be used to launch the debugger.

Default is :code:`gdb`.

:code:`target`
^^^^^^^^^^^^^^

This is the path to your source code, it is relative to the VSCode workspace (or it can
be an absolute path). This will be overriden by the :code:`target` field in the debugging
configurations if specified (cf. next section).

Default is :code:`${file}`.

:code:`cwd`
^^^^^^^^^^^

This is the root directory of your project. This will be overriden by the :code:`cwd` field in
the debugging configurations if specified (cf. next section).

Default is :code:`${workspaceRoot}`.

:code:`display_variable_attributes`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Set this to :code:`true` if you want to display data storages and fields attributes (e.g. size of
alphanumerics or digits and scale of numerics).

Default is :code:`false`.

.. _configuration:

Debugging configurations
------------------------

These configurations are to be added in your :code:`.vscode/launch.json` file. There are two
types of configurations: :code:`launch` and :code:`attach`.
Attach can be :code:`local` or :code:`remote`.

.. _launch:

:code:`launch` Configuration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This configuration is used to launch the compiled program with the debugger. This configuration
has the following defaults:

.. code-block:: json

   {
      "name": "SuperBOL debugger",
      "type": "gdb",
      "request": "launch",
      "target": "${file}",
      "arguments": null,
      "cwd": "${workspaceRoot}",
      "gdbpath": "gdb",
      "cobcpath": "cobc",
      "cobcargs": [
        "-free",
        "-x"
      ],
      "group": [],
      "env": null,
      "coverage": true,
      "verbose": false
   }

The minimal configuration is as follow (all missing elements are either using the extension
settings value if they exists or the default value given just above):

.. code-block:: json

   {
      "name": "SuperBOL debugger",
      "type": "gdb",
      "request": "launch",
   }

The items of the configuration have the following effects:

* :code:`target`: changes the target to be executed with the debugger;
* :code:`arguments`: the arguments that the debugger will pass to the target;
* :code:`cwd`: the path to the project root;
* :code:`gdbpath`: the path to the :code:`gdb` executable;
* :code:`cobcpath`: the path to the :code:`cobc` executable [1]_;
* :code:`cobcargs`: the arguments to pass to :code:`cobc` [1]_;
* :code:`group`: other files in the compilation group (other than :code:`target`) [1]_;
* :code:`env`: an object containing environment variables
* :code:`coverage`: weither to show the coverage of the debugged file;
* :code:`verbose`: show the debugger output in the :code:`Debug Console` view (for debugging only).

:code:`attach` Configuration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The default configuration is as follow:

.. code-block:: json

   {
      "name": "SuperBOL debugger",
      "type": "gdb",
      "request": "launch",
      "target": "${file}",
      "arguments": null,
      "cwd": "${workspaceRoot}",
      "gdbpath": "gdb",
      "cobcpath": "cobc",
      "cobcargs": [
        "-free",
        "-x"
      ],
      "group": [],
      "env": null,
      "coverage": true,
      "verbose": false,
      "pid": null,
      "remoteDebugger": null
   }

However this configuration will not work, as either :code:`pid` or :code:`remoteDebugger` must
have a value. All other values have the same usage as in :ref:`launch`.

* :code:`pid`: The id of the process to attach to in a local attach configuration;
* :code:`remoteDebugger`: The address of the :code:`gdb` server to attach to, with format :code:`host:port`.

Coverage
--------

If you are running the debugger with a :code:`launch` request and set :code:`coverage` to :code:`true`,
then once the debugger has stopped running, you will see the coverage status of every line in the
:code:`PROCEDURE DIVISION`.

A red line signifies that the line is never runned.

A green line signifies that the line is runned.

----

.. [1] These options are to be removed to use the :code:`SuperBOL VSCode Platform` build tasks.
