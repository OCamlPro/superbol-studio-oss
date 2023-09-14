
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
  
  lsp
    run LSP server
  
  pp
    Preprocess a list of COBOL files, generating a preprocessed file with extension .i for each of them
  
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

* :code:`--cobc`   Activate cobc specific features

* :code:`--conf CONF_FILE`   Set the configuration file to be used

* :code:`--dialect DIALECT` or :code:`--std DIALECT`   Set the dialect to bu used (overriden by `--conf` if used)

* :code:`--free`   Shorthand for `--source-format FREE`

* :code:`--output FILE` or :code:`-o FILE`   Output File (use '-' for stdout)

* :code:`--recovery BOOL`   Enable/disable parser recovery after syntax errors (default: true)

* :code:`--silence STRING`   Silence specific messages

* :code:`--source-format SOURCE_FORMAT`   Set the format of source code; allowed values are: { FIXED (the default), FREE}
Overrides `format` from configuration file if present.

* :code:`--strict`   Use the strict configuration


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

* :code:`-o DIR`   Target directory for RST generation
