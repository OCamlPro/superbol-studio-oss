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

let pp_paragraph title ppf = function
  | None -> ()
  | Some s ->
      Pretty.print ppf
        "\n\
        \       %s.\n\
        \       %s" title (String.trim s)

let pp_section title ppf = function
  | None -> ()
  | Some s ->
      Pretty.print ppf
        "\n\
        \       %s SECTION.\n\
        \       %s" title (String.trim s)

let pp_maybe ppf = function
  | None -> ()
  | Some s ->
      Pretty.print ppf
        "\n\
        \       %s" (String.trim s)

let pp_optional_division_header title ppf = function
  | None -> ()
  | Some _ ->
      Pretty.print ppf
        "\n\
        \       %s DIVISION." title

let pp_optional_section_header title ppf = function
  | None -> ()
  | Some _ ->
      Pretty.print ppf
        "\n\
        \       %s SECTION." title

let prog
    ?file_control
    ?file_section
    ?working_storage
    ?local_storage
    ?procedure
    prog_name =
  Pretty.to_string
    "\
       \       PROGRAM-ID. %s.%a%a%a\n\
       \       DATA DIVISION.%a%a%a\n\
       \       PROCEDURE DIVISION.%a\n"
    prog_name
    (pp_optional_division_header "ENVIRONMENT") file_control
    (pp_optional_section_header "INPUT-OUTPUT") file_control
    (pp_paragraph "FILE-CONTROL") file_control
    (pp_section "FILE") file_section
    (pp_section "WORKING-STORAGE") working_storage
    (pp_section "LOCAL-STORAGE") local_storage
    pp_maybe procedure
