(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Cobol_common.Srcloc.TYPES
open Cir_types

module NEL = Cobol_common.Basics.NEL

(* --- *)

type unsupported_stuff = ..
type unsupported_stuff +=
  | Statement of Cobol_ptree.statement
  | Term: _ Cobol_ptree.term -> unsupported_stuff
  | Field_in_occurs
  | Variable_length_field

type undefined_stuff =
  | Data_reference of Cobol_ptree.qualname

type ambiguous_stuff =
  | Data_reference of Cobol_ptree.qualname

type error = ..
type error +=
  | Unsupported of
      {
        loc: srcloc;
        stuff: unsupported_stuff;
      }
  | Undefined of
      {
        loc: srcloc;
        stuff: undefined_stuff;
      }
  | Ambiguous of
      {
        loc: srcloc;
        stuff: ambiguous_stuff;
        candidates: Cobol_ptree.qualname NEL.t;
      }
  | Data_error of
      Cobol_data.Types.error

type errors = error NEL.t

(* --- *)

type ('f, 'r, 'module_memory) value_builder =
  {
    create_record_data:
      Cobol_data.Types.record -> 'r record_handle;
    create_mutable_field:
      Cobol_data.Types.field_definition with_loc -> 'r record_handle ->
      ('f mutable_field, errors) result;
    create_field_from_literal_value:
      Cobol_data.Types.literal_value with_loc ->
      ('f immutable_field, errors) result;

    create_module_memory:
      name:string -> source_file:string -> 'module_memory;
  }
