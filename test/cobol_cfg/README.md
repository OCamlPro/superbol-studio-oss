# Tests for SuperBOL CFG generation

Tests in this directory primarily target the `cobol_cfg` library,
whose goal is to generate a representation for a COBOL program's
control-flow graph.  A human is more amenable to assess the
correctness of such a graph w.r.t a source program via a visual
representation than via a textual one (like the DOT format).
Moreover, only very recent versions of `graphviz` provide outputs in
ASCII format[^graphviz-ascii]—not even available in Debian trixie.
Therefore, to still enable visual debugging, most tests in this
directory are layed out in the following way:

- Test programs are to be constructed in library `cfg_tests`, based on
  modules that start with the `Cfg_` prefix;

- Corresponding expect-tests with an DOT format representation of the
  CFGs are included in the library `test_cobol_cfg_dot` (see [the
  `dune` file](./dune)), using modules that start with prefix `Dot_`;
  
- Corresponding expect-tests with an ASCII representation of the CFGs
  are included in the library `test_cobol_cfg_ascii` (see [the `dune`
  file](./dune)), using modules that start with prefix `Ascii_`.
  
The ASCII tests specifically rely on an external program called
`graph-easy`, installable via package `libgraph-easy-perl` on Debian.
These tests can only be executed or promoted with `dune` when
`graph-easy` is available; they are simply ignored otheriwse.

The suggested workfow for adding tests is to work and check results
via an ASCII test first, and then promote a correspondig test on DOT
format.

[^graphviz-ascii]: https://graphviz.org/docs/outputs/ascii/
