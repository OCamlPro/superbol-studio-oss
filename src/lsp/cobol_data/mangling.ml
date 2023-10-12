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

open Cobol_common.Srcloc.INFIX
open Cobol_ptree

(* TODO: Don't require naming of fillers to avoid this kind of exceptions. *)
exception Not_mangled

let filler_num = ref 0

let new_filler_num () =
  let num = !filler_num in
  incr filler_num;
  num

let new_filler_string () =
  Printf.sprintf "Filler-%u" (new_filler_num ())

let mangle_data_name ~default_loc data_name = match data_name with
  | Some { payload = DataName _; _ } -> data_name
  | _ -> Some (DataName (new_filler_string () &@ default_loc) &@ default_loc)

let mangled_data_name data_name = match data_name with
  | Some { payload = DataName name; _ } -> ~&name
  | _ -> raise Not_mangled
