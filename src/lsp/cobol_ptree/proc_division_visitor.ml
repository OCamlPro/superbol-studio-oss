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

open PTree_types

open Cobol_common.Srcloc.TYPES
open Cobol_common.Visitor
open Cobol_common.Visitor.INFIX                         (* for `>>` (== `|>`) *)
open Terms_visitor
open Statements_visitor

(* --- *)

class virtual ['a] folder = object
  inherit ['a] Statements_visitor.folder
  method fold_procedure_division      : (procedure_division         , 'a) fold = default
  method fold_procedure_division'     : (procedure_division with_loc, 'a) fold = default

  method fold_declarative                 : (declarative                      , 'a) fold = default
  method fold_declarative'                : (declarative with_loc             , 'a) fold = default
  method fold_paragraph                   : (paragraph                        , 'a) fold = default
  method fold_paragraph'                  : (paragraph with_loc               , 'a) fold = default
  method fold_procedure_by_clause         : (procedure_by_clause              , 'a) fold = default
  method fold_procedure_by_clause'        : (procedure_by_clause with_loc     , 'a) fold = default
  method fold_by_reference                : (by_reference                     , 'a) fold = default
  method fold_declarative_use             : (declarative_use                  , 'a) fold = default
  method fold_use_after_exception         : (use_after_exception              , 'a) fold = default
  method fold_use_for_debugging_target    : (use_for_debugging_target         , 'a) fold = default
  method fold_raising_phrase              : (raising_phrase                   , 'a) fold = default
  method fold_raising_phrase'             : (raising_phrase with_loc          , 'a) fold = default
  method fold_procedure_calling_style     : (procedure_calling_style          , 'a) fold = default
  method fold_procedure_args              : (procedure_args                   , 'a) fold = default
end

let fold_use_exception_on (v: _ #folder) = function
  | UseFileExceptionOnNames n -> fold_name'_list v n
  | UseFileExceptionOnOpenMode m -> Operands_visitor.fold_open_mode v m

let fold_use_for_debugging_target (v: _ #folder) =
  handle v#fold_use_for_debugging_target
    ~continue:begin fun t x -> match t with
      | UseForDebuggingProcedure { all; procedure } -> x
          >> fold_bool v all
          >> fold_procedure_name' v procedure
      | UseForDebuggingAllProcedures -> x
    end

let fold_use_after_exception (v: _ #folder) =
  handle v#fold_use_after_exception
    ~continue:begin fun { use_after_exception;
                          use_after_exception_on_files } x -> x
      >> fold_name' v use_after_exception
      >> fold_name'_list v use_after_exception_on_files
    end

let fold_declarative_use (v: _ #folder) =
  handle v#fold_declarative_use
    ~continue:begin fun u x -> match u with
      | UseAfterFileException { global; trigger } -> x
          >> fold_bool v global
          >> fold_use_exception_on v trigger
      | UseBeforeReporting { global; report_group } -> x
          >> fold_bool v global
          >> fold_ident v report_group
      | UseForDebugging l -> x
          >> fold_list ~fold:fold_use_for_debugging_target v l
      | UseAfterIOException l -> x
          >> fold_list ~fold:fold_use_after_exception v l
      | UseAfterExceptionObject n -> x
          >> fold_name' v n
    end

let fold_declarative (v: _ #folder) =
  handle v#fold_declarative
    ~continue:begin fun { declarative_name; declarative_segment;
                          declarative_use; declarative_sentences } x -> x
      >> fold_name' v declarative_name
      >> fold_integer_opt v declarative_segment
      >> fold_option ~fold:fold_declarative_use v declarative_use
      >> fold_list ~fold:fold_statements' v declarative_sentences
    end

let fold_declarative' (v: _ #folder) =
  handle' v#fold_declarative' ~fold:fold_declarative v

let fold_paragraph (v: _ #folder) =
  handle v#fold_paragraph
    ~continue:begin fun { paragraph_name; paragraph_is_section;
                          paragraph_segment; paragraph_sentences } x ->
      ignore paragraph_is_section; x
      >> fold_name'_opt v paragraph_name
      >> fold_integer_opt v paragraph_segment
      >> fold_list ~fold:fold_statements' v paragraph_sentences
    end

let fold_paragraph' (v: _ #folder) =
  handle' v#fold_paragraph' ~fold:fold_paragraph v

let fold_by_reference (v: _ #folder) =
  handle v#fold_by_reference
    ~continue:begin fun { by_reference;
                          by_reference_optional } x -> x
      >> fold_name' v by_reference
      >> fold_bool v by_reference_optional
    end

let fold_procedure_by_clause (v: _ #folder) =
  handle v#fold_procedure_by_clause
    ~continue:begin function
      | ByReference l -> fold_list ~fold:fold_by_reference v l
      | ByValue l -> fold_name'_list v l
    end

let fold_procedure_by_clause' (v: _ #folder) =
  handle' v#fold_procedure_by_clause' ~fold:fold_procedure_by_clause v

let fold_raising_phrase (v: _ #folder) =
  handle v#fold_raising_phrase
    ~continue:begin fun { raising; raising_factory } x -> x
      >> fold_name' v raising
      >> fold_bool v raising_factory
    end

let fold_raising_phrase' (v: _ #folder) =
  handle' v#fold_raising_phrase' ~fold:fold_raising_phrase v

let fold_procedure_calling_style (v: _ #folder) =
  leaf v#fold_procedure_calling_style

let fold_procedure_args (v: _ #folder) =
  handle v#fold_procedure_args
    ~continue:begin fun { procedure_calling_style = style;
                          procedure_by_clause = args } x -> x 
      >> fold' ~fold:fold_procedure_calling_style v style
      >> fold_list ~fold:fold_procedure_by_clause' v args
    end

let fold_procedure_division (v: _ #folder) =
  handle v#fold_procedure_division
    ~continue:begin fun { procedure_args; procedure_returning;
                          procedure_raising_phrases; procedure_declaratives;
                          procedure_paragraphs } x -> x
      >> fold_option ~fold:fold_procedure_args v procedure_args
      >> fold_ident'_opt v procedure_returning
      >> fold_list ~fold:fold_raising_phrase' v procedure_raising_phrases
      >> fold_list ~fold:fold_declarative' v procedure_declaratives
      >> fold_list ~fold:fold_paragraph' v procedure_paragraphs
    end

let fold_procedure_division' (v: _ #folder) =
  handle' v#fold_procedure_division' ~fold:fold_procedure_division v

(* end *)
