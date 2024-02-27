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

open Ez_file.V1
open EzFile.OP

open Ezcmd.V2
open EZCMD.TYPES

open Cobol_indent.Types
open Common_args

let action ~numeric ~intext ~inplace ?suffix ?range
    { preproc_options = { source_format; config; _ } ; _ } files =
  let module Config = (val config) in

  let f ?contents filename =
    let project = Project.for_ ~filename in

    let output =
      match suffix with
      | Some ext ->
        Some ( filename ^ "." ^ ext )
      | None ->
        if inplace then
          Some filename
        else
          None
    in

    let edits, _ops = Cobol_indent.Main.indent
        ~source_format
        ~config:project.config.indent_config
        ~dialect:Config.dialect
        ~filename
        ?output
        ?range
        ?contents
        ()
    in

    match output with
    | Some _ ->
      ()
    | None ->

      if numeric then
        let source_format = Cobol_indent.Config.source_format source_format in

        let output_numeric oc =
          let rec iter line edits =
            match edits with
            | [] -> ()
            | edit :: edits ->
              iter_edit line edit edits

          and iter_edit line edit edits =
            if line > edit.lnum then
              assert false;
            if line < edit.lnum then begin
              Printf.fprintf oc "-1\n";
              iter_edit (line+1) edit edits
            end else begin
              let indent =
                if intext || source_format.free then
                  edit.offset_modif
                else
                  source_format.skip_before + 1 + edit.offset_modif
              in
              Printf.fprintf oc "%d\n" indent ;
              iter (line+1) edits
            end
          in
          let start_line = match range with
            | None -> 1
            | Some { start_line ; _ } -> start_line
          in
          iter start_line edits
        in
        output_numeric stdout
      else
      if edits = [] then
        Printf.eprintf "File %S: good indentation\n%!" filename
      else begin
        Printf.eprintf "File %S: %d lines to modify\n%!" filename
          ( List.length edits );
        List.iter (fun edit ->
            Printf.printf "  Line %d: move from %d to %d\n%!"
              edit.lnum edit.offset_orig edit.offset_modif
          ) edits;
      end;

  in
  match files with
  | [] ->
    let contents = FileChannel.read_file stdin in
    f ~contents ( Sys.getcwd () // "stdin.cob" )
  | files ->
    List.iter (fun filename ->
        f filename
      ) files

let generate_config () =
  let config = Cobol_indent.Config.load
      ~source_format:(Cobol_indent.Config.source_format Cobol_config.Types.(SF SFFixed))
      ~filename:"file.cob" in
  Cobol_indent.Config.generate ~config ".superbol-indent"

let cmd =
  let files = ref [] in
  let inplace = ref false in
  let intext = ref false in
  let suffix = ref None in
  let numeric = ref false in
  let range = ref None in
  let common, common_args = Common_args.get () in
  let args = common_args  in
  EZCMD.sub
    "indent file"
    (fun () ->
       let common = common () in
       action
         ~intext:!intext
         ~numeric:!numeric
         ~inplace:!inplace
         ?suffix:!suffix
         ?range:!range
         common
         !files)
    ~args:(args @ [
        [], Arg.Anons (fun list -> files := list),
        EZCMD.info ~docv:"FILES" "Cobol files to indent" ;

        [ "inplace" ], Arg.Set inplace,
        EZCMD.info "Modify files in place";

        [ "gen-config" ], Arg.Unit generate_config,
        EZCMD.info "Generate a config file .superbol-indent in this directory";

        [ "suffix" ], Arg.String (fun s ->
            suffix := Some s),
        EZCMD.info ~docv:"EXT"
          "Set an extension for the file being generated";

        [ "numeric" ], Arg.Set numeric,
        EZCMD.info "Output indentation size at the beginning of each line";

        [ "intext" ], Arg.Set intext,
        EZCMD.info "For numeric, indentation size is relative to area A";

        [ "lines" ], Arg.String (fun s ->
            let start_line, end_line = EzString.cut_at s '-' in
            let start_line = int_of_string start_line in
            let end_line = int_of_string end_line in
            range := Some { start_line ; end_line }
          ),
        EZCMD.info ~docv:"LINE-LINE" "Indent only lines between these lines";

      ]
      )
    ~doc: "Indentation"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P ""
      ];
    ]
