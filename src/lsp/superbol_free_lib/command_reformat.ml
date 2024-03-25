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

open Common_args

let action ~inplace ?suffix
    { preproc_options = { source_format; config; _ } ; _ } files =
  let module Config = (val config) in

  let f ?contents filename =
    let contents = match contents with
      | Some contents -> contents
      | None -> EzFile.read_file filename
    in
    let new_contents =
      try
        Cobol_indent.Reformat.to_free
          ~source_format
          ~filename
          ~contents
      with
      | Cobol_indent.Reformat.Error (pos, msg) ->
        Printf.eprintf "Error in %s at pos %d: %s\n%!"
          filename pos msg;
        exit 2
    in

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

    match output with
    | Some filename ->
      EzFile.write_file filename new_contents
    | None ->
      Printf.printf "%s%!" new_contents
  in
  match files with
  | [] ->
    let contents = FileChannel.read_file stdin in
    f ~contents ( Sys.getcwd () // "stdin.cob" )
  | files ->
    List.iter (fun filename ->
        f filename
      ) files

let to_free_cmd =
  let files = ref [] in
  let inplace = ref false in
  let suffix = ref None in
  let common, common_args = Common_args.get () in
  let args = common_args  in
  EZCMD.sub
    "reformat to-free"
    (fun () ->
       let common = common () in
       action
         ~inplace:!inplace
         ?suffix:!suffix
         common
         !files)
    ~args:(args @ [
        [], Arg.Anons (fun list -> files := list),
        EZCMD.info ~docv:"FILES" "Cobol files to indent" ;

        [ "inplace" ], Arg.Set inplace,
        EZCMD.info "Modify files in place";

        [ "suffix" ], Arg.String (fun s ->
            suffix := Some s),
        EZCMD.info ~docv:"EXT"
          "Set an extension for the file being generated";

      ]
      )
    ~doc: "Reformat"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P ""
      ];
    ]
