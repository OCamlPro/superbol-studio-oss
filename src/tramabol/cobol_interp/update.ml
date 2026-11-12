(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Ezlibcob.V1
open Cir_types

(* --- *)

let move ~vm:_ value ~into state =
  cob_move value into;
  Ok state

let store_option flag =
  CobStoreOpt.(to_int @@ enc flag)

let store_options ~rounded ~checked =
  S32.of_int_unsafe @@
  (if rounded then store_option COB_STORE_ROUND else 0) lor
  (if checked then store_option COB_STORE_KEEP_ON_OVERFLOW else 0)

let arithmetic ~vm:_ operator operand ~target ~rounded ~checked state =
  let options = store_options ~rounded ~checked in
  match operator with
  | Add ->
      cob_add target operand options;
      Ok state
  | Subtract ->
      cob_sub target operand options;
      Ok state

let clear_size_error ~vm:_ state =
  cob_reset_exception_code ();
  Ok state

(** Exception codes of the EC-SIZE class (as per libcob's `exception.def`). *)
let ec_size_class = 0x1000

let size_error ~vm:_ state =
  Ok (state, cob_get_exception_code () land 0xff00 = ec_size_class)
