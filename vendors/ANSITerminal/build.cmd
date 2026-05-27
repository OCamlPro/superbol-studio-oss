REM Build ANSITerminal until oasis/ocamlbuild works on Windows

cd src

ocaml choose_implementation.ml

ocamlc -verbose -c ANSITerminal_stubs.c

ocamlc -c ANSITerminal.mli
ocamlmklib -verbose -o ANSITerminal ANSITerminal_common.ml ANSITerminal.ml ANSITerminal_stubs.obj

@echo version = "0.6.5"> META
@echo description = "Basic control of the windows shell.">> META
@echo requires = "unix">> META
@echo archive(byte) = "ANSITerminal.cma">> META
@echo archive(byte, plugin) = "ANSITerminal.cma">> META
@echo archive(native) = "ANSITerminal.cmxa">> META
@echo archive(native, plugin) = "ANSITerminal.cmxs">> META

ocamlfind install ANSITerminal ANSITerminal.cmi ANSITerminal.mli ANSITerminal.cma ANSITerminal.cmxa ANSITerminal.cmx ANSITerminal_common.cmx dllANSITerminal.dll libANSITerminal.lib ANSITerminal_stubs.obj META

cd ../tests
REM ocamlfind list
ocamlfind ocamlc -verbose -o showcolors.exe -package ANSITerminal -linkpkg showcolors.ml

dir showcolors.exe
