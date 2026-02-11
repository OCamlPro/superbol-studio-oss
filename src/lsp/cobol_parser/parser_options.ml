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

module EXEC_MAP = Cobol_preproc.Options.EXEC_MAP

(** Gathers some types used to define options for the parser engine. *)
(* We are only defining types here so an MLI would only be redundant. *)

(** Switch for the recovery mechanism *)
type recovery =
  | DisableRecovery
  | EnableRecovery of recovery_options
and recovery_options =
  {
    silence_benign_recoveries: bool; (** Whether to silence reports about some
                                         missing tokens (e.g, periods).  *)
  }

(** Scanners for EXECs are functions fed with EXEC(UTE)/END-EXEC text blocks
    (including the "EXEC" and "END-EXEC" text words). *)
type exec_scanner =
  | Stateless_exec_scanner:
      (** Stateless EXEC block parsers just transform pre-processed text into an
          AST node, whatever the history of blocks already parsed. *)
      (Cobol_preproc.Text.t ->
       Cobol_common.Exec_block.t *
       Cobol_common.Exec_block.diagnostic list) -> exec_scanner
  | Stateful_exec_scanner:
      (** Stateful parsers accumulate some data (their state), that is passed
          upon each call to the scanner on successive blocks of the same input
          program.  Note that, contrary to what its name suggests, the
          accumulated state MUST be an immutable structure (this is so that
          rewinds of the parser always re-trigger scanner executions from the
          proper state). *)
      (Cobol_preproc.Text.t -> 'state ->
       Cobol_common.Exec_block.t *
       Cobol_common.Exec_block.diagnostic list * 'state) *
      'state -> exec_scanner
type exec_scanners =
  {
    exec_scanner_fallback: exec_scanner;
    exec_scanners: exec_scanner EXEC_MAP.t;
  }

type 'm memory =
  | Amnesic: Cobol_common.Behaviors.amnesic memory
  | Eidetic: Cobol_common.Behaviors.eidetic memory

type parser_options =
  {
    verbose: bool;
    show: [`Pending] list;
    recovery: recovery;
    config: Cobol_common.Config.TYPES.cobol_config;
    exec_scanners: exec_scanners;
  }

let default_recovery =
  EnableRecovery { silence_benign_recoveries = false }

let default ~exec_scanners =
  {
    verbose = false;
    show = [`Pending];
    recovery = default_recovery;
    config = Cobol_common.Config.default;
    exec_scanners;
  }
