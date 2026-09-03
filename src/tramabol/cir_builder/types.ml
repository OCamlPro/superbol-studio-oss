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

type ambiguous_stuff =
  | Data_reference of Cobol_ptree.qualname

type extraneous_stuff =
  | Data_reference_subscripts of
      {
        qn: Cobol_ptree.qualname;
        amount: int;
      }

type missing_stuff =
  | Data_reference_subscripts of
      {
        qn: Cobol_ptree.qualname;
        amount: int;
      }

(* Note: call `Printer.register_unsupported_stuff_printer` when extending this
   type. *)
type unsupported_stuff = ..

(* Note: edit the corresponding function in `printer.ml` when adjusting this
   type. *)
type unsupported_stuff +=
  | Statement of Cobol_ptree.statement
  | Term: _ Cobol_ptree.term -> unsupported_stuff
  | Variable_length_field
  | Dynamic_table

type undefined_stuff =
  | Data_reference of Cobol_ptree.qualname

(* Note: call `Printer.register_error_printer` and
   `Error.register_error_loc_retriever` when extending this type. *)
type error = ..

(* Note: edit the corresponding functions in `printer.ml` and `error.ml` when
   adjusting this type. *)
type error +=
  | Ambiguous of
      {
        loc: srcloc;
        stuff: ambiguous_stuff;
        candidates: Cobol_ptree.qualname NEL.t;
      }
  | Extraneous of
      {
        locs: srcloc NEL.t;
        stuff: extraneous_stuff;
      }
  | Missing of
      {
        loc: srcloc;
        stuff: missing_stuff;
      }
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
  | Data_error of
      Cobol_data.Types.error

type errors = error NEL.t

(* --- *)

module CONST_REF = struct
  type t = Cobol_data.Types.literal_value
  let equal a b = Cobol_data.Types.compare_literal_value a b = 0
  let hash a = Hashtbl.hash a
end
module CONST_TABLE = Ephemeron.K1.Make (CONST_REF)

type ('f, 'r, 'm) builder =
  {
    create_record_data:
      Cobol_data.Types.record ->
      ('r record_handle, errors) result;
    create_field_from_definition:
      Cobol_data.Types.field_definition with_loc -> 'r record_handle ->
      ('f, errors) result;
    create_field_from_literal_value:
      Cobol_data.Types.literal_value with_loc ->
      ('f immutable_field, errors) result;

    create_module_memory:
      name:string -> source_file:string -> 'm;

    const_fields: 'f immutable_field CONST_TABLE.t;
  }

type ('f, 'r, 'm) env =
  {
    named_fields: 'f fields_map;
    builder: ('f, 'r, 'm) builder;
  }
