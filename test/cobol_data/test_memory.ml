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

let show_size size =
  Pretty.out "@[%a@]@." Memory.pp_size size

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
  show_size Memory.(size_of_byte);
  show_size Memory.(repeat size_of_byte ~by:(int 1));
  show_size Memory.(repeat size_of_byte ~by:(int 2));
  show_size Memory.(repeat size_of_byte ~by:(int 3));
  show_size Memory.(increase size_of_byte ~by:size_of_byte);
  show_size Memory.(increase size_of_byte ~by:(const_size 42));
  show_size Memory.(increase size_of_byte ~by:size_of_byte |>
                    increase ~by:(const_size 41) |>
                    increase ~by:(const_size 1));
  show_size Memory.(increase size_of_byte ~by:size_of_byte |>
                    increase ~by:(const_size 42) |>
                    increase ~by:(const_size 0));
  [%expect  {|
    size-of-byte
    size-of-byte
    (* 2 size-of-byte)
    (* 3 size-of-byte)
    (* 2 size-of-byte)
    (+ 42 size-of-byte)
    (+ 42 (* 2 size-of-byte))
    (+ 42 (* 2 size-of-byte)) |}];;

let%expect_test "valof-size" =
  show_factor Memory.(valof Term.(name "A"));
  [%expect  {| (valof A) |}];;

let%expect_test "valof-size" =
  let a = Term.name "A" and b = Term.name "B" in
  show_size Memory.(valof_size a |>
                    increase ~by:(valof_size b));
  show_size Memory.(valof_size a |>
                    increase ~by:(valof_size b) |>
                    increase ~by:(valof_size a) |>
                    increase ~by:(valof_size b));
  show_size Memory.(valof_size a |>
                    increase ~by:(valof_size b) |>
                    increase ~by:(valof_size a) |>
                    increase ~by:(repeat (valof_size b) ~by:(int 5)) |>
                    increase ~by:(const_size 42));
  [%expect  {|
    (+ (valof A) (valof B))
    (+ (* 2 (valof A)) (* 2 (valof B)))
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
  show_size Memory.(increase size_of_byte ~by:(valof_size a));
  show_size Memory.(increase point_size ~by:(valof_size a) |>
                    increase ~by:size_of_byte);
  show_size Memory.(increase size_of_byte ~by:(valof_size a) |>
                    repeat ~by:(int 2));
  show_size Memory.(increase size_of_byte ~by:(valof_size a) |>
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
    (+ (valof A) size-of-byte)
    (+ (valof A) size-of-byte)
    (+ (* 2 (valof A)) (* 2 size-of-byte))
    (+ (* 4 (valof A)) (valof B) (* 4 size-of-byte)) |}];;

let%expect_test "non-linear-size" =
  let a = Term.name "A" and b = Term.name "B" in
  check_nonlinear @@
  lazy Memory.(increase size_of_byte ~by:(valof_size a) |>
               repeat ~by:(valof a));
  check_nonlinear @@
  lazy Memory.(increase point_size ~by:(const_size 5) |>
               repeat ~by:(valof b) |>
               repeat ~by:(valof a));
  [%expect  {|
    non-linearity: (valof A)
    non-linearity: (valof A), (valof B) |}];;
