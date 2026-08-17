(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

open Cobol_common.Srcloc.TYPES

module TYPES: sig
  type log_entry =
    | FileCopy of
        {
          copyloc: srcloc;
          status: copy_event_status;
        }
    | Replace of
        {
          replloc: srcloc;
        }
    | Replacement of
        {
          matched_loc: srcloc;
          replacement_text: Text.text;
        }
    | CompilerDirective of
        {
          compdir: Preproc_directives.compiler_directive;
          loc: srcloc;
        }
    | Exec_block of
        {
          preamble_loc: srcloc;
          text: Text.text;
          postamble_loc: srcloc option;
        }
    | Ignored of
        {
          text: Text.text;
          ignored_loc: srcloc;
        }
    | Variable_definition of
        {
          loc: srcloc;
          var: Preproc_env.var;
          def: variable_definition;
        }
    | Variable_substitution of                 (* Note: parser-specific event *)
        {
          loc: srcloc;
          var: Preproc_env.var;
          def: Preproc_env.compilation_var_definition;
        }
    | Variable_evaluation of
        {
          loc: srcloc;
          var: Preproc_env.var;
          def: Preproc_env.compilation_var_definition option; (* [None] if undef *)
        }

  and copy_event_status =
    | CopyDone of string
    | CyclicCopy of string
    | MissingCopy of Cobol_common.Copybook.TYPES.lookup_error

  and variable_definition =
    | Compilation_variable of Preproc_env.compilation_var_definition
    | Preproc_variable of Preproc_env.preproc_var_definition

  type log
end

include module type of TYPES
  with type variable_definition = TYPES.variable_definition
   and type copy_event_status = TYPES.copy_event_status
   and type log_entry = TYPES.log_entry
   and type log = TYPES.log

val empty: log
val append
  : log_entry
  -> log -> log
val append_entries
  : log_entry list
  -> log -> log
val new_compdir
  : loc: srcloc
  -> compdir:Preproc_directives.compiler_directive
  -> log -> log
val copy_done
  : loc: srcloc
  -> filename: string
  -> log -> log
val cyclic_copy
  : loc: srcloc
  -> filename: string
  -> log -> log
val missing_copy
  : loc: srcloc
  -> error: Cobol_common.Copybook.TYPES.lookup_error
  -> log -> log
val new_replace
  : loc: srcloc
  -> log -> log
val exec_block
  : preamble_loc: srcloc
  -> ?postamble_loc: srcloc
  -> Text.text
  -> log -> log
val ignored
  : Text.text                                                    (* non-empty *)
  -> log -> log
val ppvar_def
  : loc: srcloc
  -> var: Preproc_env.var
  -> def: Preproc_env.preproc_var_definition
  -> log -> log
val compvar_def
  : loc: srcloc
  -> var: Preproc_env.var
  -> def: Preproc_env.compilation_var_definition
  -> log -> log
val compvar_subst
  : loc: srcloc
  -> var: Preproc_env.var
  -> def: Preproc_env.compilation_var_definition
  -> log -> log
val compvar_eval
  : loc: srcloc
  -> var: Preproc_env.var
  -> ?def: Preproc_env.compilation_var_definition
  -> log -> log

(* --- *)

val events: log -> log_entry list

(** Fold in any order *)
val fold: f:(log_entry -> 'a -> 'a) -> log -> 'a -> 'a
