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

(** Representation of COBOL compilation units *)

open Cobol_common.Srcloc.TYPES

(* utils *)

(** A type to manage sets of items in both named and ordered form.  Note that in
    many cases anonymous elements of [list] many not belong to [named]. *)
type 'a named_n_ordered =
  {
    named: 'a Unit_qualmap.t;
    list: 'a list;
  }

type ('a, 'ref_name) resolved_reference =
  {
    resolved_name: 'ref_name;
    resolved: 'a;                                  (* note: skipped in visitor *)
  }

type 'a resolved_name =
  ('a, Cobol_ptree.name with_loc) resolved_reference

type 'a resolved_qualname =
  ('a, Cobol_ptree.qualname with_loc) resolved_reference

(* config *)

(** Corresponds to the contents of the CONFIGURATION SECTION. *)
type unit_config =
  {
    unit_currency_signs: Cobol_common.Basics.CharSet.t;
    unit_decimal_point: char;
    unit_sign_config: Cobol_data.Picture.TYPES.sign_config;
  }

(* TODO: add a dedicated type to hold info from the INPUT-OUTPUT SECTION. Maybe
   building up a more global `unit_env` type in that process. *)

(* data items *)

type data_definitions =
  {
    data_items: Cobol_data.Types.data_definition named_n_ordered;
    data_records: Cobol_data.Types.record list;
  }

(* proc *)

type procedure_paragraph =
  {
    paragraph_name: Cobol_ptree.procedure_name with_loc option;
    paragraph: Cobol_ptree.paragraph with_loc;
  }

type procedure_section =
  {
    section_name: Cobol_ptree.procedure_name with_loc;
    section_paragraphs: procedure_paragraph with_loc named_n_ordered;
  }

type procedure_block =
  | Paragraph of procedure_paragraph with_loc
  | Section of procedure_section with_loc

type procedure_using = procedure_arg with_loc list

and procedure_arg =
  {
    arg_data_definition: Cobol_data.Types.data_definition resolved_name;
    arg_passing_style: arg_passing_style;
  }

and arg_passing_style =
  | Arg_by_reference of { optional: bool }
  | Arg_by_value

type procedure =
  {
    procedure_blocks: procedure_block named_n_ordered;
    procedure_using: procedure_using with_loc option;
  }

(* main *)

(** Type of a main COBOL compilation unit *)
type cobol_unit =
  {
    unit_name: string with_loc;
    unit_parent_name: string with_loc option;
    unit_config: unit_config;
    unit_data: data_definitions;
    unit_procedure: procedure;
  }

type t = cobol_unit with_loc

(* --- *)

let block_name: _ -> Cobol_ptree.qualname with_loc option = function
  | Paragraph { payload = p; _ } -> p.paragraph_name
  | Section { payload = s; _ } -> Some s.section_name
