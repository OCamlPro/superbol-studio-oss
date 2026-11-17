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

include Ast_types
module Printer = Ast_printer
module Visitor = Ast_visitor

(** [cobol_vars esql] lists the COBOL variables referenced in [esql], in
    reverse order of appearance. *)
let cobol_vars esql =
  let folder = object
    inherit [cobolVarId list] Visitor.folder
    method! fold_cobol_var_id cob_var acc =
      Cobol_common.Visitor.skip (cob_var :: acc)
  end in
  Visitor.fold_esql_instruction folder esql []
