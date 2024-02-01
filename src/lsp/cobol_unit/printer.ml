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

open Types

open Cobol_common.Srcloc.INFIX

let pp_cobol_unit ?(show_items = false) =
  Pretty.record_with_conditional_fields [
    T Fmt.(field "name" (fun x -> ~&(x.unit_name)) string);
    T (Pretty.vfield "records" (fun x -> x.unit_data.data_records)
         (Fmt.(list ~sep:nop) Cobol_data.Printer.pp_record));
    C'(show_items,
       Pretty.vfield "items" (fun x -> x.unit_data.data_items.named)
         (Qualmap.pp_qualmap Cobol_data.Printer.pp_data_definition));
  ]

let pp_cobol_unit' ppf u = pp_cobol_unit ppf ~&u

let pp_group ppf group =
  Collections.SET.iter begin fun u ->
    Fmt.(vbox @@ (styled `Yellow @@ any "unit") ++ any ": " ++
                 pp_cobol_unit' ++ any "@\n") ppf u
  end group
