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
    | SFVariable
    | SFXOpen
    | SFxCard
    | SFCRT
    | SFTrm
    | SFCOBOLX

  type platform = {
    mutable verbosity : int ;

    mutable eprintf :  'a. ('a, out_channel, unit) format -> 'a ;
    mutable error :  'a. ('a, Format.formatter, unit) format -> 'a ;
    mutable read_file : string -> string ;
    mutable getenv_opt : string -> string option ;

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
  eprintf = Printf.eprintf;
  error = Pretty.error;
  read_file = begin fun file ->
    Pretty.string_to (fun msg -> raise (Sys_error msg))
      "%s: Filesystem operations are unavailable" file
  end;
  autodetect_format = (fun ?source_contents:_ _filename -> SFFixed);
  find_lib = Copybook.dummy_find_lib;
  getenv_opt = (fun _variable -> None);
}

let copy ~dst ~src:{ verbosity; eprintf; error; read_file; autodetect_format;
                     find_lib; getenv_opt }  =
  dst.verbosity <- verbosity;
  dst.eprintf <- eprintf;
  dst.error <- error;
  dst.read_file <- read_file;
  dst.autodetect_format <- autodetect_format;
  dst.find_lib <- find_lib;
  dst.getenv_opt <- getenv_opt
