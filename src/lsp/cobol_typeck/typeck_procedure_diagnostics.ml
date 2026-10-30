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

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

module QUAL = Cobol_unit.Qual
module NEL = Cobol_common.Basics.NEL

type qualname_ambiguity =
  {
    given_qualname: Cobol_ptree.qualname with_loc;
    matching_qualnames: Cobol_ptree.qualname NEL.t;
  }

type error =
  | Unknown_proc_name of Cobol_ptree.qualname with_loc
  | Undefined_data_name of Cobol_ptree.qualname with_loc
  | Ambiguous_data_name of qualname_ambiguity
  | Ambiguous_proc_name of qualname_ambiguity
  | Subfield_where_record_expected of
      {
        name: Cobol_ptree.name with_loc;
        data_def: Cobol_data.Types.data_definition;
      }
  | Invalid_proc_arg_storage of
      {
        arg_name: Cobol_ptree.name with_loc;
        actual_storage: Cobol_data.Types.data_storage;
      }

let error_loc = function
  | Unknown_proc_name { loc; _ }
  | Undefined_data_name { loc; _ }
  | Ambiguous_data_name { given_qualname = { loc; _ }; _ }
  | Ambiguous_proc_name { given_qualname = { loc; _ }; _ }
  | Subfield_where_record_expected { name = { loc; _ }; _ }
  | Invalid_proc_arg_storage { arg_name = { loc; _ }; _ } ->
      loc

let pp_qualname_ambiguity ~kind ppf { given_qualname; matching_qualnames } =
  Pretty.print ppf "Ambiguous@ %s@ '%a';@ known@ matching@ names@ are@ %a"
    kind
    QUAL.pp ~&given_qualname
    (NEL.pp ~fopen:"" ~fclose:"" (fun ppf -> Fmt.fmt "'%a'" ppf QUAL.pp))
    matching_qualnames

let pp_error ppf = function
  | Unknown_proc_name qn ->
      Pretty.print ppf "Unknown@ procedure-name@ '%a'" QUAL.pp ~&qn
  | Undefined_data_name qn ->
      Pretty.print ppf "Undefined@ data-name@ '%a'" QUAL.pp ~&qn
  | Ambiguous_data_name ambiguity ->
      pp_qualname_ambiguity ~kind:"data-name" ppf ambiguity
  | Ambiguous_proc_name ambiguity ->
      pp_qualname_ambiguity ~kind:"procedure-name" ppf ambiguity
  | Subfield_where_record_expected { name; _ } ->
      Pretty.print ppf
        "Data@ name@ %a@ does@ not@ reference@ the@ root@ of@ a@ record@ in@ DATA@ DIVISION@ as@ expected"
        Cobol_ptree.pp_name' name
  | Invalid_proc_arg_storage { arg_name; actual_storage } ->
      Pretty.print ppf
        "Invalid@ storage@ %a@ for@ PROCEDURE@ DIVISION@ argument@ %a@ (LINKAGE@ \
         expected)"
        Cobol_data.Printer.pp_data_storage actual_storage
        Cobol_ptree.pp_name' arg_name
