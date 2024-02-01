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

let output_file filename s =
  match filename with
  | "-" ->
    Printf.printf "%s\n%!" s
  | _ ->
    let oc = open_out filename in
    output_string oc s;
    close_out oc;
    Printf.eprintf "File %S generated\n%!" filename

let cmd =
  let files = ref [] in
  let cobc = ref false in
  let output = ref None in
  let check = ref false in
  let parse = ref false in
  let common_get, common_args = Common_args.get () in
  EZCMD.sub
    "pp"
    (fun () ->
       match List.rev !files with
       | [] ->
           failwith "Provide at least one file"
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
               if !parse || !check then
                 let parse ?source_format input =
                   let common = common_get () in
                   let preproc_options =
                     { common.preproc_options with
                       source_format =
                         Option.value source_format
                         ~default:  common.preproc_options.source_format }
                   in
                   input |>
                   Cobol_preproc.Main.preprocessor ~options:preproc_options |>
                   Cobol_parser.Main.parse_simple ~options:common.parser_options
                 in
                 let my_text = Cobol_preproc.Src_input.from ~filename:file ~f:parse in
                 Format.eprintf "%a@." Cobol_common.Diagnostics.Set.pp my_text.diags;
                 match my_text.result with
                 | Only (Some cg) -> (
                     let print =
                       Format.asprintf "@[%a@]@." Cobol_ptree.Types.pp_compilation_group
                     in
                     let contents = print cg in
                     output_file filename contents;
                     if !check then
                       match
                         parse ~source_format:(SF SFFree) (String { filename; contents })
                       with
                       | { result = Only (Some cg'); _ } ->
                           if Cobol_ptree.Types.compare_compilation_group cg' cg <> 0 then (
                             Format.eprintf "Reparse: different@.";
                             exit 1
                           )
                       | { diags; _ } ->
                           Format.eprintf "Reparse: %a@." Cobol_common.Diagnostics.Set.pp diags;
                           exit 1
                       | exception _ ->
                           Format.eprintf "Reparse: ERROR!!!@.";
                           exit 1
                   )
                 | _ -> assert false
               else
                 let text =
                   let common = common_get () in
                   Cobol_common.Diagnostics.show_n_forget @@
                   Cobol_preproc.Main.text_of_file file
                     ~options:common.preproc_options
                 in
                 let s =
                   Cobol_preproc.Text_printer.string_of_text
                     ~cobc:!cobc
                     ~max_line_gap:100
                     text in
                 output_file filename s)
             files)
    ~args: (common_args @ [
        [ "cobc" ], Arg.Set cobc,
        EZCMD.info "Activate cobc specific features";

        [ "output"; "o" ], Arg.String (fun f -> output := Some f),
        EZCMD.info ~docv:"FILE" "Output File (use '-' for stdout)";

        [ "parse" ], Arg.Set parse,
        EZCMD.info ~docv:"PARSE"
          "If true, parse the generated cobol before printing";

        [ "check" ], Arg.Set check,
        EZCMD.info ~docv:"CHECK" "If true, check the output (implies --parse)";

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
