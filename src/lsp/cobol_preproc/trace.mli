(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

module TYPES: sig
  type log_entry =
    | FileCopy of
        {
          copyloc: Cobol_common.Srcloc.t;
          status: copy_event_status;
        }
    | Replace of
        {
          replloc: Cobol_common.Srcloc.t;
        }
    | Replacement of
        {
          matched_loc: Cobol_common.Srcloc.t;
          replacement_text: Text.text;
        }
    | CompilerDirective of
        {
          compdir: Directives.compiler_directive;
          loc: Cobol_common.Srcloc.t;
        }

  and copy_event_status =
    | CopyDone of string
    | CyclicCopy of string
    | MissingCopy of Cobol_common.Copybook.lookup_info

  type log
end

include module type of TYPES
  with type copy_event_status = TYPES.copy_event_status
   and type log_entry = TYPES.log_entry
   and type log = TYPES.log

val empty: log
val append
  : log_entry
  -> log -> log
val new_compdir
  : loc: Cobol_common.Srcloc.t
  -> compdir:Directives.compiler_directive
  -> log -> log
val copy_done
  : loc: Cobol_common.Srcloc.t
  -> filename: string
  -> log -> log
val cyclic_copy
  : loc: Cobol_common.Srcloc.t
  -> filename: string
  -> log -> log
val missing_copy
  : loc: Cobol_common.Srcloc.t
  -> info: Cobol_common.Copybook.lookup_info
  -> log -> log
val new_replace
  : loc: Cobol_common.Srcloc.t
  -> log -> log

(* --- *)

val events: log -> log_entry list
