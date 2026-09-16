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

let start () =
  let argv = CArray.create (CPtr Char) 2 in
  CArray.set argv 0 (CArray.to_ptr (CArray.of_string "main"));
  CArray.set argv 1 (CPtr.null Char);
  cob_init S32.one (CArray.to_ptr argv)

let stop () =
  (* Never leave the interpreter like this... *)
  (* cob_stop_run S32.zero *)
  ()

let run ~f =
  start ();
  let res = try f () with e -> stop (); raise e in
  stop ();
  res
