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

open Unit_types

open Cobol_common.Srcloc.INFIX

let pp_cobol_unit ?(show_items = false) =
  let open Cobol_data.Printer in
  pp_braced_record_with_conditional_fields [
    T Fmt.(field "name" (fun x -> ~&(x.unit_name)) string);
    T (vfield "records" (fun x -> x.unit_data.data_records)
         (Fmt.(list ~sep:nop) pp_record));
    C (show_items,
       vfield "items" (fun x -> x.unit_data.data_items.named)
         (Unit_qualmap.pp_qualmap pp_item));
  ]

let pp_group ppf group =
  Unit_collections.SET.iter
    (Fmt.(vbox @@ (styled `Yellow @@ any "unit") ++
                  any ": " ++ pp_cobol_unit ++ any "@\n") ppf) group
