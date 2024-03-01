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
open Terms_visitor

let todo    x = Cobol_common.Visitor.todo    __FILE__ x
let partial x = Cobol_common.Visitor.partial __FILE__ x

(* --- *)

class virtual ['a] folder = object
  inherit ['a] Terms_visitor.folder
  method fold_arithmetic_mode: (arithmetic_mode, 'a) fold = default
  method fold_entry_convention: (entry_convention, 'a) fold = default
  method fold_options_clause: (options_clause, 'a) fold = default

  method fold_informational_paragraph': (informational_paragraph with_loc, 'a) fold = default
  method fold_informational_paragraphs: (informational_paragraphs, 'a) fold = default
  method fold_options_paragraph': (options_paragraph with_loc, 'a) fold = default
  method fold_options_paragraph: (options_paragraph, 'a) fold = default
end

let todo    x = todo    __MODULE__ x
and partial x = partial __MODULE__ x

let fold_arithmetic_mode (v: _ #folder) =
  leaf v#fold_arithmetic_mode

let fold_entry_convention (v: _ #folder) =
  leaf v#fold_entry_convention

let fold_options_clause (v: _ #folder) =
  handle v#fold_options_clause
    ~continue:begin function
      | Arithmetic a -> fold_arithmetic_mode v a
      | DefaultRoundedMode r
      | IntermediateRounding r -> fold_rounding_mode v r
      | EntryConvention e -> fold_entry_convention v e
      | FloatBinaryDefault e -> Data_descr_visitor.fold_endianness_mode v e
      | FloatDecimalDefault e -> Data_descr_visitor.fold_encoding_endianness v e
    end

(* --- *)

let fold_informational_paragraph' (v: _ #folder) =
  leaf' v#fold_informational_paragraph' v

let fold_informational_paragraphs (v: _ #folder) =
  handle v#fold_informational_paragraphs
    ~continue:(fold_list ~fold:fold_informational_paragraph' v)

let fold_options_paragraph (v: _ #folder) =
  handle v#fold_options_paragraph
    ~continue:(fold_with_loc_list ~fold:fold_options_clause v)

let fold_options_paragraph' (v: _ #folder) =
  handle' v#fold_options_paragraph' v
    ~fold:fold_options_paragraph
