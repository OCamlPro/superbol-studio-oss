(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

open EzCompat
open Cobol_common.Platform.TYPES
open Cobol_indent.Types
open Types

(* Known limitations:

   * We do not correctly handle continuation lines in the middle of
     EXEC SQL commands ;

   * tabulations in margin in fixed format breaks the computation of
     indentation ;
*)

let preproc ~platform ~filename ?(sql_in_copybooks = false) ?(copy_path = [])
    ?(copy_exts = []) ?contents ~source_format
    ~cobol_unit () =
  let contents =
    match contents with
    | None -> platform.read_text_file filename
    | Some c -> c
  in
  let scanner_config = Cobol_indent.Config.load ~platform
      ~source_format ~filename in

  if scanner_config.verbosity > 0 then
    Printf.eprintf "Parsing file %S...\n%!" filename;

  let scanner_config = { scanner_config with scan_for_indent = false } in

  let copy_exts =
    match copy_exts with
    | [] -> [ ".cpy" ]
    | _ -> copy_exts
  in
  let copy_path = Filename.dirname filename :: copy_path in
  let copy_path =
    lazy
      (List.map
         (fun dir ->
           let files =
             match Sys.readdir dir with
             | exception _ -> [||]
             | files -> files
           in
           let map = ref StringMap.empty in
           Array.iter
             (fun file ->
               map := StringMap.add (String.lowercase_ascii file) file !map )
             files;
           (dir, !map) )
         copy_path )
  in

  let config =
    { platform;
      scanner_config;
      sql_in_copybooks;
      copy_path;
      copy_exts;
      verbosity = scanner_config.verbosity
    }
  in

  let sql_statements = Parse.parse ~config ~filename ~contents in

  let contents =
    Generate.generate ~filename ~contents ~cobol_unit sql_statements
  in

  contents
