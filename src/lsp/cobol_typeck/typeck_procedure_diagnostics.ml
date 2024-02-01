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

module DIAGS = Cobol_common.Diagnostics
module QUAL = Cobol_unit.Qual
module NEL = Cobol_common.Basics.NEL

type qualname_ambiguity =
  {
    given_qualname: Cobol_ptree.Types.qualname with_loc;
    matching_qualnames: Cobol_ptree.Types.qualname NEL.t;
  }

type error =
  | Unknown_proc_name of Cobol_ptree.Types.qualname with_loc
  | Ambiguous_data_name of qualname_ambiguity
  | Ambiguous_proc_name of qualname_ambiguity


let error_loc = function
  | Unknown_proc_name { loc; _ }
  | Ambiguous_data_name { given_qualname = { loc; _ }; _ }
  | Ambiguous_proc_name { given_qualname = { loc; _ }; _ } ->
      Some loc

let pp_qualname_ambiguity ~kind ppf { given_qualname; matching_qualnames } =
  Pretty.print ppf "Ambiguous@ %s@ '%a';@ known@ matching@ names@ are@ %a"
    kind
    QUAL.pp ~&given_qualname
    (NEL.pp ~fopen:"" ~fclose:"" (fun ppf -> Fmt.fmt "'%a'" ppf QUAL.pp))
    matching_qualnames

let pp_error ppf = function
  | Unknown_proc_name qn ->
      Pretty.print ppf "Unknown@ procedure-name@ '%a'" QUAL.pp ~&qn
  | Ambiguous_data_name ambiguity ->
      pp_qualname_ambiguity ~kind:"data-name" ppf ambiguity
  | Ambiguous_proc_name ambiguity ->
      pp_qualname_ambiguity ~kind:"procedure-name" ppf ambiguity
