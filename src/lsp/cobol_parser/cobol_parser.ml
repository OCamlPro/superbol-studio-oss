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

(** {1 Exported modules} *)

(** Parse tree *)
module PTree = PTree
module PTree_visitor = PTree_visitor

(** Options to tune the parser engine *)
include Parser_options

type ('a, 'm) parsed_result = ('a, 'm) Parser_engine.parsed_result =
  {
    parsed_input: Cobol_preproc.input;
    parsed_diags: Cobol_common.Diagnostics.Set.t;
    parsed_output: ('a, 'm) Parser_options.output;
  }

type 'm parsed_compilation_group =
  (PTree.compilation_group option, 'm) parsed_result

(** {1 Exported modules} *)
(*TODO: remove these extra modules once the parser provides the proper tokens.*)
module Grammar_contexts = Grammar_contexts
module Grammar_tokens = Grammar_tokens
module Text_keywords = Text_keywords

(** {1 Exported functions} *)

type 'x source_handling = ?source_format:Cobol_config.source_format_spec -> 'x

let parse_simple: _ source_handling = Parser_engine.parse_simple
let parse_with_tokens: _ source_handling = Parser_engine.parse_with_tokens
let parsing_artifacts = Parser_engine.parsing_artifacts

(* --- *)

(** {1 Modules and functions exported for testing purposes}

    Signatures of modules below may change unexpectedly. *)

module INTERNAL = struct

  (** {2 COBOL tokens} *)
  module Tokens = Grammar_tokens

  let pp_token = Text_tokenizer.pp_token
  let pp_tokens = Text_tokenizer.pp_tokens

  (** {2 COBOL grammar} *)
  module Grammar (* : Grammar_sig.S *) = Grammar

  (** {2 Parser with dummy source locations, that can be fed directly with a
      list of tokens} *)
  module Dummy = struct
    module Tags: Cobol_ast.Testing_helpers.TAGS = struct
      let loc = Cobol_common.Srcloc.raw Lexing.(dummy_pos, dummy_pos)
    end

    let parse_as item toks =
      let toks = ref toks
      and dummy_lexer = Lexing.from_string ~with_positions:false "" in
      item begin fun _ -> match !toks () with
        | Seq.Nil -> Grammar_tokens.EOF
        | Cons (x, tl) -> toks := tl; x
      end dummy_lexer

    let parse_list_as parse lx = parse_as parse (List.to_seq lx)
  end
end
