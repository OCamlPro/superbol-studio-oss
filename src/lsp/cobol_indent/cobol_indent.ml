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

(* If you delete or rename this file, you should add
   'src/cobol_indent/main.ml' to the 'skip' field in "drom.toml" *)

module Type = Indent_type

(*return the result of indentation. use user-defined indent_config*)
let indent_range' = Indenter.indent_range'

(*indent the whole file and print*)
let indent_file ~source_format ~file ~indent_config =
  indent_range' ~source_format ~range:None ~indent_config ~file
  |> Fmt.pr "%s"

(*indent a range of file and print*)
let indent_range ~source_format ~file ~range ~indent_config =
  indent_range' ~source_format ~range ~indent_config ~file
  |> Fmt.pr "%s"
