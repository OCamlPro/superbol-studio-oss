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

val token_types: string list
val token_modifiers: string list

(* TODO: once we decide to fix the min OCaml version to >=4.14, we can upgrade
   to lsp>=16, and avoid having to use an array below. *)
val data
  : filename: string
  -> range: Lsp.Types.Range.t option
  -> tokens: Cobol_parser.Outputs.tokens_with_locs
  -> pplog: Cobol_preproc.Trace.log
  -> rev_comments: Cobol_preproc.Text.comments
  -> rev_ignored: Cobol_common.Srcloc.lexloc list
  -> ptree: Cobol_ptree.Types.compilation_group
  -> int array
