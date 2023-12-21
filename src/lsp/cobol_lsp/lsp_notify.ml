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

module QUAL = Cobol_unit.Qual
module NEL = Cobol_common.Basics.NEL

let unknown kind given_qualname =
  Lsp_io.pretty_notification ~type_:Warning
    "Unknown %s '%a'" kind QUAL.pp given_qualname

let ambiguous kind given_qualname ~matching_qualnames =
  Lsp_io.pretty_notification ~type_:Warning
    "%s '%a' is ambiguous; known@ matching@ names@ are@ %a"
    (String.capitalize_ascii kind)
    QUAL.pp given_qualname
    (NEL.pp ~fopen:"" ~fclose:"" (fun ppf -> Fmt.fmt "'%a'" ppf QUAL.pp))
    matching_qualnames
