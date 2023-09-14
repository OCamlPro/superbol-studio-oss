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

open Ezcmd.V2
open EZCMD.TYPES

open Common_args

let cmd =
  let files = ref [] in
  let cobc = ref false in
  let output = ref None in
  let common_get, common_args = Common_args.get () in
  EZCMD.sub
    "pp"
    (fun () ->
       match List.rev !files with
       | [] ->
           failwith "Provide at list one file"
       | files ->
           List.iter (fun file ->
               let filename = match !output with
                 | None -> ( Filename.chop_extension file ) ^ ".i"
                 | Some output ->
                     begin match files with
                       | [ _ ] -> ()
                       | _ ->
                           failwith "Option -o conflicts with providing multiple files"
                     end;
                     output
               in
               if filename = file then
                 Pretty.failwith "Source file conflicts with target %s" file;
               let text =
                 let common = common_get () in
                 Cobol_preproc.text_of_file file
                   ~verbose: common.verbose
                   ~source_format:common.source_format
                   ~libpath:common.libpath
               in
               let s =
                 Cobol_preproc.Text_printer.string_of_text
                   ~cobc:!cobc
                   ~max_line_gap:100
                   text in
               match filename with
               | "-" ->
                   Printf.printf "%s\n%!" s
               | _ ->
                   let oc = open_out filename in
                   output_string oc s;
                   close_out oc;
                   Printf.eprintf "File %S generated\n%!" filename;)
             files)
    ~args: (common_args @ [
        [ "cobc" ], Arg.Set cobc,
        EZCMD.info "Activate cobc specific features";

        [ "output"; "o" ], Arg.String (fun f -> output := Some f),
        EZCMD.info ~docv:"FILE" "Output File (use '-' for stdout)";

        [], Arg.Anon (0, fun f -> files := f :: !files),
        EZCMD.info ~docv:"FILE" "Cobol file to preprocess";
      ])
    ~doc: "Preprocess a list of COBOL files, generating a preprocessed \
           file with extension .i for each of them"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P ""
      ];
    ]
