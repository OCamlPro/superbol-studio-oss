(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

(** `parse` sql command *)

open Ezcmd.V2
open EZCMD.TYPES
open Common_args

(** Analyzes [filename] according to the configuration of its project, reusing
    and filling the project's cache.  Returns the effective preprocessing
    options for [filename], its contents, and the compilation unit it holds;
    or an explanation of why there is no such unit. *)
let typeck_file ({ platform; analysis_overrides = overrides; _ } as common)
    filename =
  let Caching_commands.{ filename; project; contents; analysis; _ } =
    Caching_commands.analyze_file common filename
  in
  Cobol_common.Diagnostics.Set.pp ~platform Fmt.stderr @@
  Superbol_project.Analysis.diagnostics analysis;
  match analysis.checked with
  | None ->
      Error (Pretty.to_string "%s@ appears@ to@ be@ a@ copybook" filename)
  | Some { group; _ } ->
      match Cobol_unit.Collections.SET.choose group with
      | cu' ->
          let cu = cu'.payload in
          (* let x_info =
               (* May raise Not_found | Cobol_unit.Resolver_map.Ambiguous _ *)
               Cobol_unit.Resolver_map.find
                 (Cobol_unit.Qual.name ( Cobol_common.Srcloc.flagit "VBFLD" Cobol_common.Srcloc.dummy))
                 cu.unit_data.data_items.named
             in
             match x_info with
             | Data_field { def = { payload = { field_layout; field_size; _ } ; _ } ; _ } ->
                 Pretty.out "Size of VBFLD is %u Bytes@."
                   Cobol_data.Memory.(as_bits field_size / 8);
                 begin match field_layout with
                   | Elementary_field { usage = Display picture; _ } ->
                       Pretty.out "PIC is %a@."
                         Cobol_data.Picture.pp picture;
                       (match picture.category with
                        | FixedNum { digits = _; scale = _; sign = _; _ } -> ()
                        | _ -> ())
                   | Elementary_field _
                   | Struct_field _ ->
                       ()
                 end
             | _ -> ()
          *)
          Ok (Superbol_project.Analysis.preproc_options ~platform ~overrides
                ~filename project,
              contents, cu)
      | exception Not_found ->
          Error (Pretty.to_string "No@ compilation@ unit@ found@ in@ %s"
                   filename)

let parse ~sql_in_copybooks ~test_extension common files =
  let platform = common.platform in
  List.iter
    (fun filename ->
       match typeck_file common filename with
       | Error msg ->
           Pretty.error "@[Skipping:@ %s@]@." msg
       | Ok (Cobol_preproc.Options.{ source_format;
                                     copybook_lookup_config = { lookup_path;
                                                                lookup_exts };
                                     _ },
             contents, cobol_unit) ->
           let contents =
             Sql_preproc.Main.preproc ()
               ~platform ~sql_in_copybooks ~filename ~contents
               ~source_format:(Cobol_indent.Config.source_format source_format)
               ~copy_path:lookup_path
               ~copy_exts:(List.map (fun ext -> "." ^ ext) lookup_exts)
               ~cobol_unit
           in
           let output_file filename s =
             match filename with
             | "-" ->
               Printf.printf "%s\n%!" s
             | _ ->
               let oc = open_out filename in
               output_string oc s;
               close_out oc;
               Printf.eprintf "File %S generated\n%!" filename
           in
           let new_filename =
             let extension = Filename.extension filename in
             if test_extension
             then
               let base_name = Filename.chop_suffix filename extension in
               base_name ^ ".cbsql"
             else if String.equal extension ".cob" || String.equal extension ".cbl"
             then
               let base_name = Filename.chop_suffix filename extension in
               base_name ^ ".pp" ^ extension
             else
               filename
           in
           output_file new_filename contents)
    files

let preproc_cmd =
  let sql_in_copybooks = ref false in
  let test_extension = ref false in
  let files = ref [] in
  let common, common_args = Common_args.get () in
  EZCMD.sub "sql preproc"
    (fun () ->
      let common = common () in
      Printexc.record_backtrace true;
      parse ~sql_in_copybooks:!sql_in_copybooks common
      ~test_extension:!test_extension !files )
    ~args:
      ( common_args
      @ [ ( [],
            Arg.Anons (fun l -> files := l),
            EZCMD.info ~docv:"FILE" "COBOL files to preproc" );
          ( [ "copybooks" ],
            Arg.Set sql_in_copybooks,
            EZCMD.info "Preprocess copybooks also (without REPLACING)" );
          ( [ "test-ext" ],
            Arg.Set test_extension,
            EZCMD.info "Set file extension to .cbsql" );
          (* I (@NeoKaios) removed that as it conflicts with another option *)
          (* ( [ "ext" ], *)
          (*   Arg.String (fun s -> copy_exts := !copy_exts @ [ "." ^ s ]), *)
          (*   EZCMD.info ~docv:"EXT" *)
          (*     "Add .EXT as an extension to find copybooks (default is cpy)" ) *)
        ] )
    ~doc:"Preprocess SQL EXECs"
