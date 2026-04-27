(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2024 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(*
   This module implements a `platform` record, that is used to virtualize the
   system and some choices in the application that is running our COBOL
   parser. All system calls (Sys, Unix, open_) should go through this record, so
   that the application can override them, typically to run in full-memory.

   Some heuristics are also implemented through this record, so that they can be
   specialize by the application. For example, `autodetect_format` can be used
   to automatically detect the format, or `find_lib` can be used to decide how
   to lookup copybooks.

   The 'context argument is used to pass the application configuration to these
   functions.
*)


module TYPES = struct

  type source_format_id =
    | SFFree
    | SFFixed
    | SFCOBOL85
    | SFVariable
    | SFXOpen
    | SFxCard
    | SFCRT
    | SFTrm
    | SFCOBOLX

  type platform = {
    mutable verbosity : int ;
    mutable tab_stop : int ;

    mutable eprintf :  'a. ('a, out_channel, unit) format -> 'a ;
    mutable error :  'a. ('a, Format.formatter, unit) format -> 'a ;
    mutable read_file : string -> string ;
    mutable getenv_opt : string -> string option ;
    mutable mk_temp_dir: ?mode:int -> ?dir:string -> string -> string;
    mutable remove_dir: ?all:bool -> string -> unit;

    mutable autodetect_format:
      ?source_contents:string ->
      string ->                                                    (* filename *)
      source_format_id;
    mutable find_lib:
      lookup_config:Copybook.TYPES.lookup_config ->
      ?fromfile:string ->
      ?libname:Copybook.TYPES.fileloc ->
      Copybook.TYPES.fileloc -> (string, Copybook.TYPES.lookup_error) result;

  }

end

open TYPES

(** The [innocuous] platform never performs any I/O operation other than on
    [stdin] or [stdout]. *)
let innocuous = {
  verbosity = 0;
  tab_stop = 8;
  eprintf = Printf.eprintf;
  error = Pretty.error;
  read_file = begin fun file ->
    Pretty.string_to (fun msg -> raise (Sys_error msg))
      "%s: Filesystem operations are unavailable" file
  end;
  mk_temp_dir = begin fun ?mode:_ ?dir:_ dirname ->
    Pretty.string_to (fun msg -> raise (Sys_error msg))
      "%s: Filesystem operations are unavailable" dirname
  end;
  remove_dir = begin fun ?all:_ dir ->
    Pretty.string_to (fun msg -> raise (Sys_error msg))
      "%s: Filesystem operations are unavailable" dir
  end;
  autodetect_format = (fun ?source_contents:_ _filename -> SFFixed);
  find_lib = begin fun ~lookup_config ?fromfile:_ ?libname:_ (`Alphanum w |
                                                              `Word w) ->
    Error { lookup_libname = w; lookup_config }
  end;
  getenv_opt = (fun _variable -> None);
}

let copy ~dst ~src:{ verbosity; tab_stop; eprintf; error; read_file;
                     mk_temp_dir; remove_dir;
                     autodetect_format; find_lib; getenv_opt }  =
  dst.verbosity <- verbosity;
  dst.tab_stop <- tab_stop;
  dst.eprintf <- eprintf;
  dst.error <- error;
  dst.read_file <- read_file;
  dst.mk_temp_dir <- mk_temp_dir;
  dst.remove_dir <- remove_dir;
  dst.autodetect_format <- autodetect_format;
  dst.find_lib <- find_lib;
  dst.getenv_opt <- getenv_opt

(* --- *)

(** High-level filesystem operations *)
module FS = struct

  (** [with_temp_dir ~platform ?given_temp_dir f] creates a temporary directory
      [temp_dir] and executes [f ~temp_dir] if [given_temp_dir] is [None], or
      executes [f ~temp_dir:dir] if [given_temp_dir] is [Some dir].  [temp_dir] is
      removed upon termination of [f] in the former case; otherwise, the directory
      is left in place.

      [default_pattern_prefix] is used to forge a name for the directory created,
      if any. *)
  let with_temp_dir ~platform ~default_pattern_prefix ?given_temp_dir f =
    let temp_dir =
      match given_temp_dir with
      | None ->
          platform.mk_temp_dir default_pattern_prefix
      | Some dirname ->
          dirname
    in
    let finalize () =
      if given_temp_dir = None then
        platform.remove_dir ~all:true temp_dir
    in
    match f ~temp_dir with
    | res ->
        finalize (); res
    | exception e ->
        finalize (); raise e

end
