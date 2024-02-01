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

(** Utilities to deal with sections/paragraphs of PROCEDURE DIVISIONs *)

open Types

let rec find
    ?(in_section: procedure_section option)
    (procedure_name: Cobol_ptree.Types.procedure_name)
    (procedure: procedure)
  : procedure_block =
  match in_section with
  | None ->
      Qualmap.find procedure_name procedure.named
  | Some { section_paragraphs; _ } ->
      try
        Paragraph (Qualmap.find procedure_name section_paragraphs.named)
      with Not_found -> find procedure_name procedure

let rec full_qn
    ?(in_section: procedure_section option)
    (procedure_name: Cobol_ptree.Types.procedure_name)
    (procedure: procedure)
  =
  match in_section with
  | None ->
      (Qualmap.find_binding procedure_name procedure.named).full_qn
  | Some { section_paragraphs; _ } ->
      try
        (Qualmap.find_binding procedure_name section_paragraphs.named).full_qn
      with Not_found -> full_qn procedure_name procedure
