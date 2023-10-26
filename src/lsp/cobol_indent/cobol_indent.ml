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

module Type = Indent_type

(*return the result of indentation. use user-defined indent_config*)
let indent_range' = Indenter.indent_range'

(*indent the whole file and print*)
let indent_file ~dialect ~source_format ~file ~indent_config =
  let contents = Ez_file.V1.EzFile.read_file file in
  indent_range'
    ~dialect ~source_format ~range:None ~indent_config
    ~filename:file ~contents
  |> Fmt.pr "%s"

(*indent a range of file and print*)
let indent_range ~dialect ~source_format ~file ~range ~indent_config =
  let contents = Ez_file.V1.EzFile.read_file file in
  indent_range' ~dialect ~source_format ~range ~indent_config
    ~filename:file ~contents
  |> Fmt.pr "%s"
