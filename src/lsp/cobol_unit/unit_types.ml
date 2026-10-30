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
open Cobol_common.Srcloc.INFIX

(* utils *)

(** A type to manage sets of items in both named and ordered form.  Note that in
    many cases anonymous elements of [list] do not appear in [named]. *)
type 'a named_n_ordered =
  {
    named: 'a Unit_resolver_map.t;
    list: 'a list;
  }

(* config *)

type display_sign =
  {
    sign_position: sign_position; (** relevant for non-binary representations *)
    sign_separate: bool;         (** [true] = separate character (extra byte) *)
  }

(* FIXME: these are not properties that can be derived from picture
   strings. They should be attached to the relevant usage instead. *)
and sign_position = Leading | Trailing

(** Corresponds to the contents of the CONFIGURATION SECTION. *)
type unit_config =
  {
    unit_currency_signs: Cobol_common.Basics.CharSet.t;
    unit_decimal_point: char;
    unit_display_sign_config: Cobol_data.Types.display_sign_config;
  }

type unit_env =
  {
    env_config: unit_config;         (* CONFIGURATION SECTION *)
    (* TODO: INPUT-OUTPUT SECTION (at least) *)
  }

(* data items *)

(** [data_definitions] exposes two views on the items (fields and tables) in DATA DIVISION:
    - [data_items] contains all the items with direct access by name.
    - [data_records] is a list of record trees where each element represents
      a record definition (typically a level 01 or 77 data item) *)
type data_definitions =
  {
    data_items: Cobol_data.Types.data_definition named_n_ordered;
    (* LATER: recheck if the list of ordered data_definition is useful here.
       All the structure information should already be accessible trhough [data_records]. *)

    data_records: Cobol_data.Types.record list;
  }

(* proc *)

type paragraph =
  {
    paragraph_name: Cobol_ptree.procedure_name with_loc option;
    (** [None] for the unnamed paragraph at the begining of a section.
        [Some qn] where [qn] is fully qualified otherwise.
        Note that [paragraph_name] is different than [paragraph_name] in
        [Cobol_ptree.paragraph].
        For an unnamed paragraph at the begining of a section
        [Cobol_ptree.paragraph] contains the (optionnal) section name whereas
        this [paragraph_name] contains [None] to indicate that the paragraph is
        anonymous. *)

    paragraph_sentences: Cobol_procedure.Types.statements with_loc list;
  }

type section =
  {
    section_name: Cobol_ptree.name with_loc option;
    (** [None] for the unnamed section at the begining of PROCEDURE DIVISION
        For convenience when collecting references to all procedure names,
        [section_name] is visited as a [procedure_name with_loc option]. *)

    section_segment: Cobol_data.Types.integer_value option;
    (** Optional segment priority number (obsolete in COBOL-85) *)

    section_paragraphs: paragraph with_loc named_n_ordered;
    (** Includes an anonymous empty paragraph if the section is empty.
        However, this list does not start with an unnamed paragraph,
        when the section immediately starts with a paragraph name. *)
  }

type procedure_block =
  | Paragraph of paragraph with_loc
  | Section of section with_loc

type procedure_using = Cobol_procedure.Types.procedure_arg with_loc list

type procedure =
  {
    procedure_using: procedure_using with_loc option; (** PROCEDURE DIVISION USING ... *)
    procedure_sections: section with_loc list;

    procedure_blocks: procedure_block Unit_resolver_map.t;
    (* skipped by visitors:
       procedure blocks are all visited in source order through [procedure_sections] *)
  }

(* main *)

(** Type of a main COBOL compilation unit *)
type cobol_unit =
  {
    unit_name: string with_loc;
    unit_parent_name: string with_loc option;
    unit_env: unit_env;
    unit_data: data_definitions;
    unit_procedure: procedure;
  }

type t = cobol_unit with_loc

(* --- *)

let block_name: _ -> Cobol_ptree.qualname with_loc option = function
  | Paragraph { payload = p; _ } -> p.paragraph_name
  | Section { payload = s; _ } -> Option.map (fun s -> Cobol_ptree.Name s &@<- s) s.section_name
