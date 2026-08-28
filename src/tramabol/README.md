# TramaBOL Interpreter for COBOL

This folder contains the sources for the TramaBOL Interpreter for COBOL.

Please refer to [the changelog](CHANGELOG.md) for its current implementation status.

## Building and Testing (Linux only)

1. First, you need to install [opam](https://opam.ocaml.org/).  Make sure to run `opam init` to initialize this tool;

2. You also need to have [node.js](https://nodejs.org/) (version >=5.2.0) already installed;

3. If you build from a clone of the git repository, make sure to update submodules:
   ```shell
   git submodule update --init --recursive
   ```

4. Install OCaml dependencies:
   ```shell
   make build-deps
   ```

5. Configure and build GnuCOBOL4:
   ```shell
   mkdir _gc4
   cd import/gnucobol4
   ./build_aux/bootstrap --install
   ./configure --prefix $PWD/../../_gc4
   make install
   cd ../..
   ```

   **Note 1**: a libtool version mismatch error during the call to `make install` above, can be fixed by first calling `libtoolize`, and then restarting the above procedure at the `./build_aux/bootstrap --install` step.

   **Note 2**: another issue may occur at link stage where a symbol from `libreadline` is not found.  Passing `LDFLAGS="-lreadline"` is a valid workaround here.

6. Build and test the interpreter:
   ```shell
   make GNUCOBOL4_COB_CONFIG=$PWD/_gc4/bin/cob-config tramabol-interpreter
   LD_LIBRARY_PATH=$PWD/_gc4/lib:$LD_LIBRARY_PATH ./tramabol-linux-x64 test/tramabol/01-termio.t/hello-world.cob
   ```

   Alternatively, you can build the interpreter and run every provided test with:
   ```shell
   LD_LIBRARY_PATH=$PWD/_gc4/lib:$LD_LIBRARY_PATH make GNUCOBOL4_COB_CONFIG=$PWD/_gc4/bin/cob-config test-tramabol
   ```

## Funding

This project was partly funded through the [NGI0 Core] Fund, a fund established by [NLnet] with financial support from the European Commission's [Next Generation Internet] program.  Information about the grant can be found [here](https://nlnet.nl/project/COBOL-compiler/).

[Next Generation Internet]: https://ngi.eu
[NGI0 Core]: https://nlnet.nl/core
[NLnet]: https://nlnet.nl
