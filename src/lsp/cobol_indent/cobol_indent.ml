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
let indent_range = Indenter.indent_range

let indent_range_str
  ~dialect ~source_format ~indent_config ~range ~filename ~contents
=
indent_range
  ~dialect ~source_format ~indent_config ~range ~filename ~contents
|> Indent_util.apply contents
