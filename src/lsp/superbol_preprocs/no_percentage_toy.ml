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

(** This is a toy block scanner, used in tests, and to demonstrate registration
    of preprocessors and diagnostics for EXEC blocks. *)

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

type Cobol_common.Exec_block.TYPES.exec_block +=   (* TEMPORARY, for testing. *)
  | Nopers_exec_block of Cobol_preproc.Text.t

type Cobol_common.Exec_block.TYPES.diagnostic +=
  | Unexpected_character_in_exec_block of
      {
        char: char;
        loc: srcloc;
      }

let () =
  Cobol_common.Exec_block.register_exec_block_type
    ~name:"NO-%-EXEC-BLOCK"
    ~compare:begin fun a b -> match a, b with
      | Nopers_exec_block a, Nopers_exec_block b ->
          Some ((* FIXME: text comparison *)Stdlib.compare a b)
      | _ ->
          None
    end
    ~pp:begin function
      | Nopers_exec_block a ->
          Some (Pretty.delayed "%a" Cobol_preproc.Text.pp_text a)
      | _ ->
          None
    end;
  Cobol_common.Exec_block.register_diagnostic_type
    ~name:"NO-%-EXEC-BLOCK"
    ~severity:begin function
      | Unexpected_character_in_exec_block _ ->
          Some Error
      | _ ->
          None
    end
    ~loc:begin function
      | Unexpected_character_in_exec_block { loc; _ } ->
          Some loc
      | _ ->
          None
    end
    ~pp:begin function
      | Unexpected_character_in_exec_block { char; _ } ->
          Some (Pretty.delayed "Unexpected@ character@ `%c'@ in@ \
                                no-percentage-allowed@ EXEC/END-EXEC@ block\
                               " char)
      | _ ->
          None
    end

let scanner =
  Cobol_parser.Options.Stateless_exec_scanner begin fun text ->
    Nopers_exec_block text,
    List.filter_map begin fun word ->
      if Cobol_preproc.Text.textword_eqp ~eq:"%" word
      then Some (Unexpected_character_in_exec_block { loc = ~@word;
                                                      char = '%' })
      else None
    end text
  end
