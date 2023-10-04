
Sub-commands and Arguments
==========================
Common arguments to all sub-commands:


* :code:`-q` or :code:`--quiet`   Set verbosity level to 0

* :code:`-v` or :code:`--verbose`   Increase verbosity level

Overview of sub-commands::
  
  indent file
    Indentation
  
  indent range
    Indentation range
  
  json vscode
    parse VSODE JSON files or generate package.json
  
  lsp
    run LSP server
  
  pp
    Preprocess a list of COBOL files, generating a preprocessed file with extension .i for each of them
  
  snapshot
    Manage environment snapshots
  
  texi2rst
    build .texi documentation from gnucobol-docs


main.exe indent file
~~~~~~~~~~~~~~~~~~~~~~

Indentation



**DESCRIPTION**




**USAGE**
::
  
  main.exe indent file FILES [OPTIONS]

Where options are:


* :code:`FILES`   Cobol files to indent

* :code:`-I DIRECTORY`   Add DIRECTORY to library search path

* :code:`--conf CONF_FILE`   Set the configuration file to be used

* :code:`--dialect DIALECT` or :code:`--std DIALECT`   Set the dialect to bu used (overriden by `--conf` if used)

* :code:`--free`   Shorthand for `--source-format FREE`

* :code:`--indent_config FILE`   User defined configuration of indentation

* :code:`--recovery BOOL`   Enable/disable parser recovery after syntax errors (default: true)

* :code:`--silence STRING`   Silence specific messages

* :code:`--source-format SOURCE_FORMAT`   Set the format of source code; allowed values are: { FIXED (the default), FREE}
Overrides `format` from configuration file if present.

* :code:`--strict`   Use the strict configuration


main.exe indent range
~~~~~~~~~~~~~~~~~~~~~~~

Indentation range



**DESCRIPTION**




**USAGE**
::
  
  main.exe indent range FILE RANGE_START RANGE_END [OPTIONS]

Where options are:


* :code:`FILE`   file to check the indentation

* :code:`RANGE_START`   start line of range

* :code:`RANGE_END`   end line of range

* :code:`-I DIRECTORY`   Add DIRECTORY to library search path

* :code:`--conf CONF_FILE`   Set the configuration file to be used

* :code:`--dialect DIALECT` or :code:`--std DIALECT`   Set the dialect to bu used (overriden by `--conf` if used)

* :code:`--free`   Shorthand for `--source-format FREE`

* :code:`--indent_config FILE`   User defined offset table file

* :code:`--recovery BOOL`   Enable/disable parser recovery after syntax errors (default: true)

* :code:`--silence STRING`   Silence specific messages

* :code:`--source-format SOURCE_FORMAT`   Set the format of source code; allowed values are: { FIXED (the default), FREE}
Overrides `format` from configuration file if present.

* :code:`--strict`   Use the strict configuration


main.exe json vscode
~~~~~~~~~~~~~~~~~~~~~~

parse VSODE JSON files or generate package.json



**DESCRIPTION**




**USAGE**
::
  
  main.exe json vscode FILES [OPTIONS]

Where options are:


* :code:`FILES`   JSON Files to parse

* :code:`--gen FILE`   Generate FILE from current configuration

* :code:`--grammar`   Parse files as syntaxes/*.json files

* :code:`--language`   Parse files as language/configuration *.json files

* :code:`--manifest`   Parse files as package.json files

* :code:`--snippets`   Parse files as snippets/*.json files

* :code:`--tasks`   Parse files as .vscode/tasks.json files


main.exe lsp
~~~~~~~~~~~~~~

run LSP server



**DESCRIPTION**


Start a COBOL LSP server

**USAGE**
::
  
  main.exe lsp [OPTIONS]

Where options are:



main.exe pp
~~~~~~~~~~~~~

Preprocess a list of COBOL files, generating a preprocessed file with extension .i for each of them



**DESCRIPTION**




**USAGE**
::
  
  main.exe pp FILE [OPTIONS]

Where options are:


* :code:`FILE`   Cobol file to preprocess

* :code:`-I DIRECTORY`   Add DIRECTORY to library search path

* :code:`--check`   If true, check the output (implies --parse)

* :code:`--cobc`   Activate cobc specific features

* :code:`--conf CONF_FILE`   Set the configuration file to be used

* :code:`--dialect DIALECT` or :code:`--std DIALECT`   Set the dialect to bu used (overriden by `--conf` if used)

* :code:`--free`   Shorthand for `--source-format FREE`

* :code:`--output FILE` or :code:`-o FILE`   Output File (use '-' for stdout)

* :code:`--parse`   If true, parse the generated cobol before printing

* :code:`--recovery BOOL`   Enable/disable parser recovery after syntax errors (default: true)

* :code:`--silence STRING`   Silence specific messages

* :code:`--source-format SOURCE_FORMAT`   Set the format of source code; allowed values are: { FIXED (the default), FREE}
Overrides `format` from configuration file if present.

* :code:`--strict`   Use the strict configuration


main.exe snapshot
~~~~~~~~~~~~~~~~~~~

Manage environment snapshots



**DESCRIPTION**


This command can be used to snapshot the environment when a command is called, typically in a test script, to be able to run this command in the same environment from outside the script.

A snapshot typically contains:

* * **cmd**
  The command to run with its arguments

* * **pwd**
  The directory where the command should be run

* * **env**
  The environment variables for the command

Snapshots are stored in **$HOME/.config/superbol/snapshots**.


**SNAPSHOT CREATION**


To snapshot a command, use:
::

  # mname tname --save SNAP_ID -- cmd args


The previous command will create a snapshot **SNAP_ID**, and then run the command **cmd args**.

You can use **--quit** if you don't want to run the command at all (the command will exit with status 2)


**SNAPSHOT USAGE**


To run a command in a previously created snapshot, use:
::

  # mname tname --load SNAP_ID -- cmd args


The previous command will load the snapshot **SNAP_ID**, go to its directory, set the environment and then run the command **cmd args**.

If **cmd args** is empty, then the snapshot command is run.

The argument **--no-cd** can be used to run the command in the current directory.

The argument **--env VAR=VALUE** can be used to add an extra variable to the environment, after the one set by the snapshot.

If a **cmd args** is provided, the following special items are substituted:

* * **__**
  is replaced by all the arguments (including the command) from the snapshot

* * **_0**
  is replaced by the command from the snapshot

* * **_1**
  is replaced by all the arguments (excluding the commnad) from the snapshot

For example:
::

  # mname tname --load ID -- gdb _0 --args __


will run the command inside **gdb** with its arguments as provided by the snapshot.

**USAGE**
::
  
  main.exe snapshot ARGS [OPTIONS]

Where options are:


* :code:`ARGS`   Command line arguments

* :code:`--env VAR=VALUE`   Set a variable in the environment

* :code:`--load ID`   Load snapshot ID

* :code:`--no-cd`   Do not change directory to run the command

* :code:`--quit`   Do not run the command, just exit

* :code:`--save ID`   Create snapshot ID from state


main.exe texi2rst
~~~~~~~~~~~~~~~~~~~

build .texi documentation from gnucobol-docs



**DESCRIPTION**


Build .texi documentation from gnucobol-docs.

**USAGE**
::
  
  main.exe texi2rst FILE [OPTIONS]

Where options are:


* :code:`FILE`   .texi file

* :code:`-I DIR`   Add to lookup path for files

* :code:`-o DIR`   Target directory for RST generation
