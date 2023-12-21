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

let pp_section title ppf = function
  | None -> ()
  | Some s ->
      Pretty.print ppf "\n\
        \       %s SECTION.\n\
        \       %s" title (String.trim s)

let pp_maybe ppf = function
  | None -> ()
  | Some s ->
      Pretty.print ppf "\n\
        \       %s" (String.trim s)

let prog
    ?working_storage
    ?local_storage
    ?procedure
    prog_name =
  Pretty.to_string "\
       \       PROGRAM-ID. %s.\n\
       \       DATA DIVISION.%a%a\n\
       \       PROCEDURE DIVISION.%a\n"
    prog_name
    (pp_section "WORKING-STORAGE") working_storage
    (pp_section "LOCAL-STORAGE") local_storage
    pp_maybe procedure
