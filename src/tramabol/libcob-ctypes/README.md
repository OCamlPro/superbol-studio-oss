# `ctypes`-based bindings against `libcob`

This package relies on a mechanism provided by `dune` to auto-generate
stubs based on OCaml `ctypes` declarations.

Types are declared in `libcob_types.ml`, inside the `Types` functor.
Type names (like `struct __cob_pic_symbol`) need to match existing
types from `libcob/common.h`.  Additional helper types may be declared
in `../cobaux/cobaux_typing_helper.h` (see for instance
`Field_type.t`, that builds up an enumeration type from C
pre-processor constants with the help of an additional type `enum
ml_cob_field_type`).

Functions are delcared in `Functions` functors in the other
`libcob_*.ml` files.  No abstraction other than for simplifying types
(with enums) may be defined in `libcob-ctypes`.  Simplification of the
API (e.g. with labeled and/or optional arguments) is the purpose of
library `ez-cob`.
