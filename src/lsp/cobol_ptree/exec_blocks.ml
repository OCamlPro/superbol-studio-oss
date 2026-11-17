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

(** Contents of EXEC/EXECUTE ... END-EXEC blocks.

    Note this type is closed on purpose (instead of extensible), so that its
    values can be marshalled (e.g, into caches), and still be matched against
    after unmarshalling. *)
type exec_block =
  | Esql of Sql_ast.esql_instruction
  | Unparsed of exec_text   (** non-SQL blocks, as their words
                                (including "EXEC" and "END-EXEC") *)
and exec_text = string with_loc list
[@@deriving ord]

let pp_exec_text: exec_text Pretty.printer =
  Fmt.(list ~sep:sp (using (fun { payload; _ } -> payload) string))

let pp_exec_block ppf = function
  | Esql esql -> Sql_ast.Printer.pp ppf esql
  | Unparsed text -> pp_exec_text ppf text
