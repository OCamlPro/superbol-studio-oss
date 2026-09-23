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

open Cobol_common.Srcloc.INFIX

let default_options =
  Cobol_cfg.Options.{
    hide_unreachable = false;
    collapse_fallthru = false;
    in_degree_upper_limit = None;
    transformation = None;
    hidden_nodes = [];
    split_nodes = [];
  }

let compute_cfgs ?parser_options ?source_format ?filename
    ?(cfg_options = default_options) contents =
  Prog_typeck.typeck ?parser_options ?source_format ?filename contents |>
  Cobol_common.Diagnostics.show_n_forget ~set_status:false ~ppf:Fmt.stdout
    ~platform:Prog_common.platform |> fun typeck_outputs ->
  Cobol_unit.Group.fold begin fun unit acc ->
    let name = ~&(~&unit.unit_name) in
    let base_cfg, _transformed_cfg =
      Cobol_cfg.Builder.make ~options:cfg_options ~name typeck_outputs
    in
    (name, base_cfg) :: acc
  end typeck_outputs.group []

let show_dot_cfgs ?parser_options ?source_format ?filename
    ?cfg_options contents =
  Pretty.out "%s@\n---@\n@." contents;
  compute_cfgs ?parser_options ?source_format ?filename ?cfg_options contents |>
  List.iter begin fun (name, cfg) ->
    Pretty.out "@[<2>%s:@;%a@]@." name Cobol_cfg.Printer.pp_cfg_dot cfg
  end

let show_ascii_cfgs ?parser_options ?source_format ?filename
    ?cfg_options contents =
  Pretty.out "%s@\n---@\n@." contents;
  compute_cfgs ?parser_options ?source_format ?filename ?cfg_options contents |>
  List.iter begin fun (name, cfg) ->
    let dot_string = Pretty.to_string "%a" Cobol_cfg.Printer.pp_cfg_dot cfg in
    let dot_file = EzFile.temp_file name ".dot" in
    EzFile.write_text_file dot_file dot_string;
    ignore (Pretty.string_to Unix.system "graph-easy %s" dot_file);
    EzFile.remove dot_file
  end
