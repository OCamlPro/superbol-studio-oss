(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

(** `parse` jcl command *)

open Ezcmd.V2
open EZCMD.TYPES

open Common_args

let parse
    ~sql_in_copybooks
    ~copy_exts
    common files =
  let { preproc_options = { source_format; libpath = copy_path ; _ } ; _ } = common in
  let source_format = Cobol_indent.Config.source_format source_format in
  List.iter (fun filename ->
      let contents =
        Sql_preproc.Main.preproc
          ~sql_in_copybooks
          ~copy_path
          ~copy_exts
          ~filename
          ~source_format ()
      in
      Printf.printf "%s%!" contents) files

let preproc_cmd =
  let sql_in_copybooks = ref false in
  let copy_exts = ref [] in
  let files = ref [] in
  let common, common_args = Common_args.get () in
  EZCMD.sub
    "sql preproc"
    (fun () ->
       let common = common () in
      Printexc.record_backtrace true;
      parse
        ~sql_in_copybooks:!sql_in_copybooks
        ~copy_exts:!copy_exts
        common !files)
    ~args:
      ( common_args @ [

            [], Arg.Anons (fun l -> files := l),
            EZCMD.info ~docv:"FILE" "COBOL files to preproc" ;

            [ "copybooks" ], Arg.Set sql_in_copybooks,
            EZCMD.info "Preprocess copybooks also (without REPLACING)";

            [ "ext" ], Arg.String (fun s -> copy_exts := !copy_exts @ ["." ^ s]),
            EZCMD.info ~docv:"EXT"
              "Add .EXT as an extension to find copybooks (default is cpy)" ;

          ])
    ~doc:"Preprocess SQL EXECs"
