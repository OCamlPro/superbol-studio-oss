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

include module type of Parser_options

type 'm parsing_function
  = ?source_format:Cobol_config.source_format_spec
  -> ?config:Cobol_config.t
  -> ?recovery:recovery
  -> ?verbose:bool
  -> ?show:[`Pending] list
  -> libpath:string list
  -> Cobol_preproc.input
  -> (PTree.compilation_group option, 'm) parsed_result

val parse: memory: 'm Parser_options.memory -> 'm parsing_function
val parse_simple: Cobol_common.Behaviors.amnesic parsing_function
val parse_with_tokens: Cobol_common.Behaviors.eidetic parsing_function

val parsed_tokens
  : (_, Cobol_common.Behaviors.eidetic) parsed_result -> tokens_with_locs Lazy.t
val preproc_rev_log
  : (_, Cobol_common.Behaviors.eidetic) parsed_result -> Cobol_preproc.rev_log
