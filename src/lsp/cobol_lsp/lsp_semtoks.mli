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
  -> ptree: Cobol_ptree.compilation_group
  -> int array

(** [whole ~filename artifacts checked] computes all the semantic tokens of the
    document [filename] (and none of its copybooks), from its parsing artifacts
    and typechecking outputs.  Forces the tokens of [artifacts]. *)
val whole
  : filename: string
  -> Cobol_parser.Outputs.artifacts
  -> Cobol_typeck.Outputs.t
  -> Superbol_project.Analysis.semantic_tokens

(** [restrict ~range data] keeps the semantic tokens of [data] that intersect
    [range].  This yields the same as computing semantic tokens for [range]
    directly, except for regions that span several lines (ignored by the
    preprocessor): these are kept only when their first line intersects
    [range], which is the only line they are highlighted on anyway. *)
val restrict
  : range: Lsp.Types.Range.t
  -> int array
  -> int array
