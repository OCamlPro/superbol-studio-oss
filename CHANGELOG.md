# Changelog

## [_] Next release

### Added
- Extend syntax of ORGANIZATION to allow RECORD and BINARY optional prefix [#511](https://github.com/OCamlPro/superbol-studio-oss/pull/511)
- Support `OCCURS` with `COUNT IN` clause [#505](https://github.com/OCamlPro/superbol-studio-oss/pull/505)
- Support `$set sourceformat(fmt)` syntax [#509](https://github.com/OCamlPro/superbol-studio-oss/pull/509)
- Allow `LABEL RECORD` clause in `SD` entries [#508](https://github.com/OCamlPro/superbol-studio-oss/pull/508)
- Support for empty `IF`/`ELSE` branches [#504](https://github.com/OCamlPro/superbol-studio-oss/pull/504)
- Allow EQUAL as synonym of `=` in COMPUTE [#507](https://github.com/OCamlPro/superbol-studio-oss/pull/507)
- Support for `ADD` and `SUBTRACT` with `LENGTH OF` argument [#503](https://github.com/OCamlPro/superbol-studio-oss/pull/503)
- Support for `MOVE` with `LENGTH OF` sender [#494](https://github.com/OCamlPro/superbol-studio-oss/pull/494)
- Support for Hexadecimal literals using 'H' and 'h' prefix [#499](https://github.com/OCamlPro/superbol-studio-oss/pull/499)
- Support for `OCCURS N DEPENDING` syntax [#497](https://github.com/OCamlPro/superbol-studio-oss/pull/497)
- Allow arbitrary number of spaces before `$`-prefixed compiler directives in some fixed-width source formats [#498](https://github.com/OCamlPro/superbol-studio-oss/pull/498)
- Support for extraneous periods at the end of `SET` or `SOURCE` compiler directives [#493](https://github.com/OCamlPro/superbol-studio-oss/pull/493)
- Display reference count in hover information for data items [#429](https://github.com/OCamlPro/superbol-studio-oss/pull/429) (fix for [Issue #354](https://github.com/OCamlPro/superbol-studio-oss/issues/354))
- Dedicated libraries for testing internal behaviors of the LSP server [#487](https://github.com/OCamlPro/superbol-studio-oss/pull/487)
- Syntactic support for `LINE SEQUENTIAL` file organization [#485](https://github.com/OCamlPro/superbol-studio-oss/pull/485)
- Dedicated library for the OCaml code of the extension [#478](https://github.com/OCamlPro/superbol-studio-oss/pull/478)
- Support for customized GnuCOBOL configuration file, via the `dialect` setting [#464](https://github.com/OCamlPro/superbol-studio-oss/pull/464)
- Global data items in the Variables panel during debug sessions [#466](https://github.com/OCamlPro/superbol-studio-oss/pull/466)
- Current COBOL statement in toplevel entry of stackframe summary [#466](https://github.com/OCamlPro/superbol-studio-oss/pull/466)
- Basic language definition for toggling line comments [#463](https://github.com/OCamlPro/superbol-studio-oss/pull/463)
- Syntactic support for file and device clauses on SELECT [#495](https://github.com/OCamlPro/superbol-studio-oss/pull/495)

### Fixed
- Handling of concatenation characters that are not followed by a separator space [#500](https://github.com/OCamlPro/superbol-studio-oss/pull/500)
- Name of VSIX file included in CI build artifacts [#480](https://github.com/OCamlPro/superbol-studio-oss/pull/480) (fix for [Issue #408](https://github.com/OCamlPro/superbol-studio-oss/issues/408))
- Fallback to extension settings when `cobcPath` is not provided in task definitions [#468](https://github.com/OCamlPro/superbol-studio-oss/pull/468) (fix for [Issue #467](https://github.com/OCamlPro/superbol-studio-oss/issues/467))
- Bug that made the extension hang when debugged programs displayed signed numbers [#466](https://github.com/OCamlPro/superbol-studio-oss/pull/466)
- Requirement for specifying `gdbpath` and `libcobpath` in launch configurations (these are actually provided in extension settings) [#466](https://github.com/OCamlPro/superbol-studio-oss/pull/466)
- Enforcement of a default `preLaunchTask` that is not available [#466](https://github.com/OCamlPro/superbol-studio-oss/pull/466)
- Support for showing control-flow graphs on Windows [#479](https://github.com/OCamlPro/superbol-studio-oss/pull/479) (fix for [Issue #423](https://github.com/OCamlPro/superbol-studio-oss/issues/423))


## [0.2.2] Second release (2025-11-17)

### Added
- Support for launching the debugger on COBOL code that is in a different directory than its intermediate compilation files [#460](https://github.com/OCamlPro/superbol-studio-oss/pull/460)
- Simpler default launch configurations [#460](https://github.com/OCamlPro/superbol-studio-oss/pull/460)
- Simpler launch configuration snippets [#458](https://github.com/OCamlPro/superbol-studio-oss/pull/458) [#459](https://github.com/OCamlPro/superbol-studio-oss/pull/459)
- Support for debuging programs with binaries and sources located in distinct directories [#458](https://github.com/OCamlPro/superbol-studio-oss/pull/458)
- Ability to debug modules via `cobcrun` [#458](https://github.com/OCamlPro/superbol-studio-oss/pull/458)
- Support for debugging dynamically loaded sub-modules (e.g, via CALL + literal) [#454](https://github.com/OCamlPro/superbol-studio-oss/pull/454)
- Ability to launch a debug session when only a single file is opened (without a workspace) [#453](https://github.com/OCamlPro/superbol-studio-oss/pull/453) (fix for [issue #355](https://github.com/OCamlPro/superbol-studio-oss/issues/355))

### Fixed
- Evaluation of expressions with binary-typed fields in the debugger [#459](https://github.com/OCamlPro/superbol-studio-oss/pull/459)
- Random errors shown when debugging large programs with many data shown or watched [#459](https://github.com/OCamlPro/superbol-studio-oss/pull/459)
- Forced prelaunch task on attach mode, along with other issues in the debugger [#458](https://github.com/OCamlPro/superbol-studio-oss/pull/458)
- Handling of nested subprograms by the debugger [#455](https://github.com/OCamlPro/superbol-studio-oss/pull/455) (fix for [issue #303](https://github.com/OCamlPro/superbol-studio-oss/issues/303))
- Handling of program names with dashes by the debugger [#453](https://github.com/OCamlPro/superbol-studio-oss/pull/453) (fix for [issue #451](https://github.com/OCamlPro/superbol-studio-oss/issues/451))


## [0.2.1] Bugfix release (2025-09-23)

### Fixed
- Lookup of task configuration settings [#425](https://github.com/OCamlPro/superbol-studio-oss/pull/425)
- Clarity of some log messages, and inclusion of version and platform info on startup [#442](https://github.com/OCamlPro/superbol-studio-oss/pull/442)
- Stack overflows on semantic tokens retrieval on large files [#441](https://github.com/OCamlPro/superbol-studio-oss/pull/441)


## [0.2.0] First release (2025-05-27)

### Added
- Improved support for `EXEC SQL` blocks, with detection of COBOL variables [#375](https://github.com/OCamlPro/superbol-studio-oss/pull/375)
- Information on how to get support for the extension and GnuCOBOL [#401](https://github.com/OCamlPro/superbol-studio-oss/pull/401)
- Universal VSIX, with LSP server transpiled into JavaScript [#93](https://github.com/OCamlPro/superbol-studio-oss/pull/93)

### Fixed
- Check for x86 binaries when on `darwin-arm64` [#385](https://github.com/OCamlPro/superbol-studio-oss/pull/385)
- Binaries included in VSIXs for Darwin [#383](https://github.com/OCamlPro/superbol-studio-oss/pull/383)
- Name of debugger, to avoid conflicts with the official "gdb" [#381](https://github.com/OCamlPro/superbol-studio-oss/pull/381)


## [0.1.4] Fifth α release (2024-10-25)

### Added
- CFG explorer for COBOL programs [#368](https://github.com/OCamlPro/superbol-studio-oss/pull/368)
- Detection of copybooks based on contents prefix and configured search path [#373](https://github.com/OCamlPro/superbol-studio-oss/pull/373)
- Support for connecting to the LSP server remotely (TCP only) [#102](https://github.com/OCamlPro/superbol-studio-oss/pull/102)
- Support for Symbol Renaming command [#351](https://github.com/OCamlPro/superbol-studio-oss/pull/351)
- Show documentation comments on hover information [#350](https://github.com/OCamlPro/superbol-studio-oss/pull/350)
- Completion for more grammar constructs [#322](https://github.com/OCamlPro/superbol-studio-oss/pull/322)
- Support for CodeLens [#349](https://github.com/OCamlPro/superbol-studio-oss/pull/349)
- Show display example of `NUMERIC-EDITED` data on hover [#337](https://github.com/OCamlPro/superbol-studio-oss/pull/337)
- Support for dump and listing files, along with a task attribute for outputting the latter [#347](https://github.com/OCamlPro/superbol-studio-oss/pull/347)
- Improved information shown on completion [#336](https://github.com/OCamlPro/superbol-studio-oss/pull/336)
- Configuration flag for caching in storage provided by Visual Studio Code [#167](https://github.com/OCamlPro/superbol-studio-oss/pull/167)
- Configuration setting for copybook filename extensions [#332](https://github.com/OCamlPro/superbol-studio-oss/pull/332), with updated JSON schema [#333](https://github.com/OCamlPro/superbol-studio-oss/pull/333)
- COBOL language configuration for highlighting matching brackets and auto-insertion of line numbers in fixed-format code [#330](https://github.com/OCamlPro/superbol-studio-oss/pull/330)

### Fixed
- Issues with completion requests in informational paragraphs [#374](https://github.com/OCamlPro/superbol-studio-oss/pull/374)
- Improvements to the grammar [#331](https://github.com/OCamlPro/superbol-studio-oss/pull/331), [#353](https://github.com/OCamlPro/superbol-studio-oss/pull/353)
- Word wrapping in presence of hyphens [#330](https://github.com/OCamlPro/superbol-studio-oss/pull/330)


## [0.1.3] Fourth α release (2024-07-24)

### Added
- Support for LSP request `textDocument/documentSymbol` [#317](https://github.com/OCamlPro/superbol-studio-oss/pull/317)
- Context-sensitive completion for keywords and user-defined words [#304](https://github.com/OCamlPro/superbol-studio-oss/pull/304), [#312](https://github.com/OCamlPro/superbol-studio-oss/pull/312), [#314](https://github.com/OCamlPro/superbol-studio-oss/pull/314)
- Internal support for inspecting the parser's state right before EOF [#302](https://github.com/OCamlPro/superbol-studio-oss/pull/302)
- Show data item information on hover [#293](https://github.com/OCamlPro/superbol-studio-oss/pull/293) [#305](https://github.com/OCamlPro/superbol-studio-oss/pull/305)
- Folding of `EXEC`/`END-EXEC` blocks [#291](https://github.com/OCamlPro/superbol-studio-oss/pull/291)
- Basic syntax highlighting for SQL statements embedded in `EXEC`/`END-EXEC` blocks [#290](https://github.com/OCamlPro/superbol-studio-oss/pull/290)

### Fixed
- Improvements to the grammar [#310](https://github.com/OCamlPro/superbol-studio-oss/pull/310), [#319](https://github.com/OCamlPro/superbol-studio-oss/pull/319)
- Internal mishandling of tokens when dealing with some `EXEC`/`END-EXEC` blocks [#301](https://github.com/OCamlPro/superbol-studio-oss/pull/301)
- Internal pretty-printing routine for `EXEC`/`END-EXEC` blocks [#300](https://github.com/OCamlPro/superbol-studio-oss/pull/300)
- Support for line continuations that start with a text word [#294](https://github.com/OCamlPro/superbol-studio-oss/pull/294)
- Bug when computing the size of `RENAMES` ranges [#295](https://github.com/OCamlPro/superbol-studio-oss/pull/295)
- Internal handling of diagnostics that may be issued when dealing with `EXEC`/`END-EXEC` blocks [#292](https://github.com/OCamlPro/superbol-studio-oss/pull/292)


## [0.1.2] Third α release (2024-05-17)

### Added
- Commands to show/hide/update coverage by highlighting the source code, using `gcov-viewer` [#285](https://github.com/OCamlPro/superbol-studio-oss/pull/285)
- Ability to build modules instead of executable programs [#284](https://github.com/OCamlPro/superbol-studio-oss/pull/284)

### Fixed
- Improvements to the grammar [#280](https://github.com/OCamlPro/superbol-studio-oss/pull/280)
- Internal representation of separator comma and semicolon for proper handling in `EXEC`/`END-EXEC` blocks [#279](https://github.com/OCamlPro/superbol-studio-oss/pull/213)


## [0.1.1] Second α release (2024-04-23)

### Added
- Configuration flag to force reporting of syntax diagnostics [#213](https://github.com/OCamlPro/superbol-studio-oss/pull/213)
- Lift requirement to have `cobc` 3.2+ installed [#212](https://github.com/OCamlPro/superbol-studio-oss/pull/212)
- Support dynamic addition and removal of workspace folders [#210](https://github.com/OCamlPro/superbol-studio-oss/pull/210)
- Restart of the LSP server upon changes to some configuration settings [#207](https://github.com/OCamlPro/superbol-studio-oss/pull/207)
- Remove hard dependency to `tamasfe.even-better-toml`, and provide the `superbol-studio-pack` extension pack [#206](https://github.com/OCamlPro/superbol-studio-oss/pull/206)
- Use of syntax files for colorization when semantic tokens are missing, *e.g,* in code excerpts, or in copybooks [#200](https://github.com/OCamlPro/superbol-studio-oss/pull/200)
- Use a statically linked LSP server binary on Linux [#199](https://github.com/OCamlPro/superbol-studio-oss/pull/199)

### Fixed
- Internal bug in parsing machinery [#266](https://github.com/OCamlPro/superbol-studio-oss/pull/266)
- Integration with the debugger [#186](https://github.com/OCamlPro/superbol-studio-oss/pull/186), [#263](https://github.com/OCamlPro/superbol-studio-oss/pull/263), [#264](https://github.com/OCamlPro/superbol-studio-oss/pull/264)
- Partial support for `EXEC`/`END-EXEC` blocks [#242](https://github.com/OCamlPro/superbol-studio-oss/pull/242), [#265](https://github.com/OCamlPro/superbol-studio-oss/pull/265)
- Support for (conditional) compilation directives, *e.g,* `>>IF` [#218](https://github.com/OCamlPro/superbol-studio-oss/pull/218), [#257](https://github.com/OCamlPro/superbol-studio-oss/pull/257)
- Many more improvements to the grammar [#177](https://github.com/OCamlPro/superbol-studio-oss/pull/177), [#221](https://github.com/OCamlPro/superbol-studio-oss/pull/221), [#222](https://github.com/OCamlPro/superbol-studio-oss/pull/222), [#225](https://github.com/OCamlPro/superbol-studio-oss/pull/225), [#228](https://github.com/OCamlPro/superbol-studio-oss/pull/228), [#229](https://github.com/OCamlPro/superbol-studio-oss/pull/229), [#230](https://github.com/OCamlPro/superbol-studio-oss/pull/230), [#231](https://github.com/OCamlPro/superbol-studio-oss/pull/231), [#232](https://github.com/OCamlPro/superbol-studio-oss/pull/232), [#233](https://github.com/OCamlPro/superbol-studio-oss/pull/233), [#234](https://github.com/OCamlPro/superbol-studio-oss/pull/234), [#235](https://github.com/OCamlPro/superbol-studio-oss/pull/235), [#236](https://github.com/OCamlPro/superbol-studio-oss/pull/236), [#237](https://github.com/OCamlPro/superbol-studio-oss/pull/237), [#238](https://github.com/OCamlPro/superbol-studio-oss/pull/238), [#239](https://github.com/OCamlPro/superbol-studio-oss/pull/239), [#240](https://github.com/OCamlPro/superbol-studio-oss/pull/240), [#241](https://github.com/OCamlPro/superbol-studio-oss/pull/241), [#245](https://github.com/OCamlPro/superbol-studio-oss/pull/245), [#247](https://github.com/OCamlPro/superbol-studio-oss/pull/247), [#248](https://github.com/OCamlPro/superbol-studio-oss/pull/248), [#249](https://github.com/OCamlPro/superbol-studio-oss/pull/249), [#250](https://github.com/OCamlPro/superbol-studio-oss/pull/250), [#251](https://github.com/OCamlPro/superbol-studio-oss/pull/251), [#253](https://github.com/OCamlPro/superbol-studio-oss/pull/253), [#254](https://github.com/OCamlPro/superbol-studio-oss/pull/254), [#256](https://github.com/OCamlPro/superbol-studio-oss/pull/256), [#258](https://github.com/OCamlPro/superbol-studio-oss/pull/258), [#262](https://github.com/OCamlPro/superbol-studio-oss/pull/262), [#268](https://github.com/OCamlPro/superbol-studio-oss/pull/268), [#270](https://github.com/OCamlPro/superbol-studio-oss/pull/270)
- Case-sensitivity of exact pseudo-text matching in `COPY`/`REPLACE` statements [#224](https://github.com/OCamlPro/superbol-studio-oss/pull/224)
- Missing comments in auto-generated `superbol.toml` [#223](https://github.com/OCamlPro/superbol-studio-oss/pull/223)
- Maximum line length for X/Open free-form source format [#214](https://github.com/OCamlPro/superbol-studio-oss/pull/214)
- Preliminary support for copybook documents [#204](https://github.com/OCamlPro/superbol-studio-oss/pull/204)
- Parsing of TOML without terminating newline [#189](https://github.com/OCamlPro/superbol-studio-oss/pull/189)


## [0.1.0] Initial α release (2024-03-05)
- Activation of the extension for folders that contain at least one file with extension `.cob`, `.cbl`, `.cpy`, or `.cbx`
- Build tasks with `cobc`
- Preliminary support for debugging with `gdb`
- Support for optional project configuration files (`superbol.toml`)
- Syntax highlighting with semantic tokens
- Syntax diagnostics (filtered out by default for dialects other than `COBOL85`)
- Basic indenter for free source-format
- Hover to show copybooks
- Hover to show source text replacements
- Go to and peek definition for `WORKING-STORAGE`, `LOCAL-STORAGE`, and `LINKAGE` sections
- Find references for `WORKING-STORAGE`, `LOCAL-STORAGE`, and `LINKAGE` sections
- Partial support for workspace folders
- Partial support for copybook documents (per filename extension)
