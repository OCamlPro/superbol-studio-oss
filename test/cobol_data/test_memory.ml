(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Testing_helpers.Make (Cobol_parser.INTERNAL.Dummy.Tags)
module NEL = Cobol_common.Basics.NEL

module Memory = Cobol_data.Memory

let show_size ?name size =
  Pretty.out "@[%a%a@]@." Fmt.(option (fmt "%s = ")) name Memory.pp_size size

let show_factor factor =
  Pretty.out "@[%a@]@." Memory.pp_factor factor

let pp_symbolic_vars ppf vars =
  Pretty.list ~fopen:"@[" ~fclose:"@]" ~fsep:",@ "
    Memory.pp_symbolic_var ppf (NEL.to_list vars)

let check_nonlinear comp =
  try
    ignore @@ Lazy.force comp;
  with Memory.NON_LINEAR vars ->
    Pretty.out "non-linearity: %a@." pp_symbolic_vars vars

let%expect_test "size-0" =
  show_size Memory.point_size;
  show_size Memory.(repeat (const_size 1) ~by:(int 0));
  show_size Memory.(repeat point_size ~by:(int 1));
  show_size Memory.(increase point_size ~by:point_size);
  [%expect  {|
    0
    0
    0
    0 |}];;

let%expect_test "size-1" =
  show_size Memory.(const_size 1);
  show_size Memory.(repeat (const_size 1) ~by:(int 1));
  show_size Memory.(increase point_size ~by:(const_size 1));
  show_size Memory.(increase (const_size 1) ~by:point_size);
  [%expect  {|
    1
    1
    1
    1 |}];;

let%expect_test "size-2" =
  show_size Memory.(const_size 2);
  show_size Memory.(repeat (const_size 1) ~by:(int 2));
  show_size Memory.(repeat (const_size 2) ~by:(int 1));
  show_size Memory.(increase point_size ~by:(const_size 2));
  show_size Memory.(increase (const_size 2) ~by:point_size);
  show_size Memory.(increase (const_size 1) ~by:(const_size 1));
  [%expect  {|
    2
    2
    2
    2
    2
    2 |}];;

let%expect_test "size-3" =
  show_size Memory.(const_size 3);
  show_size Memory.(repeat (const_size 1) ~by:(int 3));
  show_size Memory.(repeat (const_size 3) ~by:(int 1));
  show_size Memory.(increase (const_size 1) ~by:(const_size 2));
  show_size Memory.(increase (const_size 2) ~by:(const_size 1));
  show_size Memory.(increase point_size ~by:(const_size 3));
  [%expect  {|
    3
    3
    3
    3
    3
    3 |}];;

let%expect_test "byte-size" =
  show_size Memory.(byte_size);
  show_size Memory.(repeat byte_size ~by:(int 1));
  show_size Memory.(repeat byte_size ~by:(int 2));
  show_size Memory.(repeat byte_size ~by:(int 3));
  show_size Memory.(increase byte_size ~by:byte_size);
  show_size Memory.(increase byte_size ~by:(const_size 42));
  show_size Memory.(increase byte_size ~by:byte_size |>
                    increase ~by:(const_size 41) |>
                    increase ~by:(const_size 1));
  show_size Memory.(increase byte_size ~by:byte_size |>
                    increase ~by:(const_size 42) |>
                    increase ~by:(const_size 0));
  [%expect  {|
    8
    8
    16
    24
    16
    50
    58
    58 |}];;

let%expect_test "valof-size" =
  show_factor Memory.(valof Term.(name "A"));
  [%expect  {| (valof A) |}];;

let%expect_test "valof-size/substitutions" =
  let a = Term.name "A" and b = Term.name "B" in
  show_size Memory.(valof_size a |>
                    increase ~by:(valof_size b));
  let x = Memory.(valof_size a |>
                    increase ~by:(valof_size b) |>
                    increase ~by:(valof_size a) |>
                    increase ~by:(valof_size b)) in
  show_size ~name:"X"
    x;
  show_size ~name:"X[A=0]"
    Memory.(assign_value Term.(name "A") 0 x);
  show_size ~name:"X[B=0]"
    Memory.(assign_value b 0 x);
  show_size ~name:"X[A=1,B=0]"
    Memory.(x |>
            assign_values @@ valuation_of_list [a, 1; b, 0]);
  show_size
    Memory.(valof_size a |>
            increase ~by:(valof_size b) |>
            increase ~by:(valof_size a) |>
            increase ~by:(repeat (valof_size b) ~by:(int 5)) |>
            increase ~by:(const_size 42));
  [%expect  {|
    (+ (valof A) (valof B))
    X = (+ (* 2 (valof A)) (* 2 (valof B)))
    X[A=0] = (* 2 (valof B))
    X[B=0] = (* 2 (valof A))
    X[A=1,B=0] = 2
    (+ 42 (* 2 (valof A)) (* 6 (valof B))) |}];;

let%expect_test "hybrid-size" =
  let a = Term.name "A" and b = Term.name "B" in
  show_size Memory.(repeat point_size ~by:(valof a));
  show_size Memory.(valof_size a);
  show_size Memory.(const_size 1);
  show_size Memory.(repeat (valof_size a) ~by:(int 1));
  show_size Memory.(repeat (valof_size a) ~by:(int 2));
  show_size Memory.(repeat (valof_size a) ~by:(int 3));
  show_size Memory.(repeat (const_size 1) ~by:(valof a));
  show_size Memory.(repeat (const_size 2) ~by:(valof a));
  show_size Memory.(repeat (const_size 3) ~by:(valof a));
  show_size Memory.(repeat (const_size 3) ~by:(valof a) |>
                    increase ~by:(valof_size a));
  show_size Memory.(increase point_size ~by:(valof_size a));
  show_size Memory.(increase byte_size ~by:(valof_size a));
  show_size Memory.(increase point_size ~by:(valof_size a) |>
                    increase ~by:byte_size);
  show_size Memory.(increase byte_size ~by:(valof_size a) |>
                    repeat ~by:(int 2));
  show_size Memory.(increase byte_size ~by:(valof_size a) |>
                    repeat ~by:(int 2) |>
                    repeat ~by:(int 2) |>
                    increase ~by:(valof_size b));
  [%expect  {|
    0
    (valof A)
    1
    (valof A)
    (* 2 (valof A))
    (* 3 (valof A))
    (valof A)
    (* 2 (valof A))
    (* 3 (valof A))
    (* 4 (valof A))
    (valof A)
    (+ 8 (valof A))
    (+ 8 (valof A))
    (+ 16 (* 2 (valof A)))
    (+ 32 (* 4 (valof A)) (valof B)) |}];;

let%expect_test "non-linear-size" =
  let a = Term.name "A" and b = Term.name "B" in
  check_nonlinear @@
  lazy Memory.(increase byte_size ~by:(valof_size a) |>
               repeat ~by:(valof a));
  check_nonlinear @@
  lazy Memory.(increase point_size ~by:(const_size 5) |>
               repeat ~by:(valof b) |>
               repeat ~by:(valof a));
  [%expect  {|
    non-linearity: (valof A)
    non-linearity: (valof A), (valof B) |}];;

let%expect_test "elementary-sizes" =
  show_size ~name:"C-long"
    Memory.size_of_C_long;
  show_size ~name:"C-long{amd64|bits}"
    Memory.(assign_consts amd64_memory_config size_of_C_long);
  show_size ~name:"C-long*3{amd64|bits}"
    Memory.(size_of_C_long |>
            repeat ~by:(int 3) |>
            assign_consts amd64_memory_config);
  [%expect  {|
    C-long = size-of-C-long
    C-long{amd64|bits} = 64
    C-long*3{amd64|bits} = 192 |}];;

let%expect_test "valof-n-elementary-size/substitutions" =
  let a = Term.name "A" and b = Term.name "B" in
  let x = Memory.(valof_size a |>
                  increase ~by:size_of_C_long |>
                  repeat ~by:(int 2) |>
                  increase ~by:(valof_size b)) in
  show_size ~name:"X" x;
  show_size ~name:"X[A=0,B=0]"
    Memory.(x |>
            assign_values @@ valuation_of_list [a, 0; b, 0]);
  show_size ~name:"X[A=3,B=4]"
    Memory.(x |>
            assign_values @@ valuation_of_list [a, 3; b, 4]);
  show_size ~name:"X[A=3,B=4]{amd64}"
    Memory.(x |>
            assign_values @@ valuation_of_list [a, 3; b, 4] |>
            assign_consts amd64_memory_config);
  [%expect  {|
    X = (+ (* 2 (valof A)) (valof B) (* 2 size-of-C-long))
    X[A=0,B=0] = (* 2 size-of-C-long)
    X[A=3,B=4] = (+ 10 (* 2 size-of-C-long))
    X[A=3,B=4]{amd64} = 138 |}];;
