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
   This module implement a `platform` record, that is used to virtualize
   the system and some choices in the application that is running
   our COBOL parser. All system calls (Sys, Unix, open_) should go through
   this record, so that the application can override them, typically to
   run in full-memory.

   Some heuristics are also implemented through this record, so that they
   can be specialize by the application. For example, `autodetect_format`
   can be used to automatically detect the format, or `find_lib` can
   be used to decide how to lookup copybooks.

   The 'context argument is used to pass the application configuration
   to these functions.
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

    mutable autodetect_format :
      ?source_contents:string -> string -> source_format_id ;
    mutable find_lib :
      lookup_config:Copybook.lookup_config ->
      ?fromfile:string ->
      ?libname:Copybook.fileloc ->
      Copybook.fileloc -> (string, Copybook.lookup_error) result ;

  }

end

open TYPES

let default_platform = {
  verbosity = 1 ;
  eprintf = Printf.eprintf ;
  error = Pretty.error ;
  read_file = (fun file -> Ez_file.V1.EzFile.read_file file) ;
  autodetect_format = (fun ?source_contents:_ _ -> SFFixed);
  find_lib = Copybook.find_lib;
  getenv_opt = (fun variable -> Sys.getenv_opt variable) ;
}
