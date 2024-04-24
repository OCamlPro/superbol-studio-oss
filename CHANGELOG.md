# Changelog

## [0.1.2] Next release

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
