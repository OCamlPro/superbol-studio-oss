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

type 'a named_n_ordered =
  {
    named: 'a Unit_qualmap.t;
    list: 'a list;
  }

(* config *)

(** Corresponds to the contents of the CONFIGURATION SECTION. *)
type unit_config =
  {
    unit_currency_signs: Cobol_common.Basics.CharSet.t;
    unit_decimal_point: char;
  }

(* data items *)

type data_definitions =
  {
    data_items: Cobol_data.Types.item named_n_ordered;
    data_records: Cobol_data.Types.record list;
  }

(* proc *)

type procedure_section =
  {
    section_name: Cobol_ptree.name with_loc;
    section_paragraphs: Cobol_ptree.paragraph with_loc list;          (* nel? *)
  }

type procedure_paragraph =
  | Paragraph of Cobol_ptree.paragraph with_loc
  | Section of procedure_section

type procedure_paragraphs = procedure_paragraph named_n_ordered

(* main *)

(** Type of a main COBOL compilation unit *)
type cobol_unit =
  {
    unit_name: string with_loc;
    unit_parent_name: string with_loc option;
    unit_loc: srcloc;
    unit_config: unit_config;
    unit_data: data_definitions;
    unit_proc_paragraphs: procedure_paragraphs;
  }

type t = cobol_unit

(* --- *)

let paragraph_name: _ -> Cobol_ptree.name with_loc option = function
  | Paragraph { payload = p; _ } -> p.paragraph_name
  | Section s -> Some s.section_name
