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

open EzCompat
open Ez_file.V1
open EzFile.OP

include Types

module Options = Options
module Default = Default
module Diagnostics = Config_diagnostics

module DIAGS = Cobol_common.Diagnostics

exception ERROR of Config_diagnostics.error

let __init_default_exn_printers =
  Printexc.register_printer begin function
    | ERROR e ->
        Some (Pretty.to_string "@[<h>%a@]" Config_diagnostics.pp_error e)
    | _ ->
        None
  end

(* --- *)

let print_options: Pretty.delayed = fun ppf ->
  Pretty.list ~fopen:"@[<v>" ~fsep:"@\n" ~fclose:"@]" (fun ppf v -> v#pp ppf)
    ppf !all_configs

let default_search_path =
  lazy begin
    let cwd = EzFile.getcwd () in
    let cob_conf_dir = match Sys.getenv_opt "COB_CONFIG_DIR" with
      | Some d -> [ d ]
      | None -> []
    and xdg_conf_dir = match Sys.getenv_opt "XDG_CONFIG_HOME" with
      | Some p -> p
      | None -> Sys.getenv "HOME" // ".config"
    in
    cwd :: xdg_conf_dir // "superbol" :: cob_conf_dir
  end

let retrieve_search_path ?search_path () =
  match search_path with
  | Some p -> p
  | None -> Lazy.force default_search_path

let find_file ~search_path filename =
  try EzFile.find_in_path search_path filename
  with Not_found -> raise @@ ERROR (Missing_file (filename, search_path))

(* --- *)

let default = (module Default: T)

let meet_support (s1: Conf_ast.support_value as 's) (s2: 's) : 's =
  match s1, s2 with
  | Ok, _ -> Ok
  | Warning, (Warning | Archaic | Obsolete | Skip | Ignore | Error |
              Unconformable) -> Warning
  | Archaic, (Archaic | Obsolete | Skip | Ignore | Error |
              Unconformable) -> Archaic
  | Obsolete, (Obsolete | Skip | Ignore | Error | Unconformable) -> Obsolete
  | Skip, (Skip | Ignore | Error | Unconformable) -> Skip
  | Ignore, (Ignore | Error | Unconformable) -> Ignore
  | Error, (Error | Unconformable) -> Error
  | _ -> s2

let make_conf (module Words: Words.S) conf_ptree =
  (* NB: a needlessly mutable state is hidden in `Words` (aarggghh...). *)
  let error k v = raise @@ ERROR (Invalid_key_value_pair (k, v)) in
  let conf =
    List.fold_left begin fun conf elt ->
      match elt with
      | Conf_ast.Value { key = "reserved"; value = ContextWord s } ->
          Words.RESERVED.add_reserved (s^"*"); conf
      | Value { key = "reserved"; value = String s } ->
          Words.RESERVED.add_reserved s; conf
      | Value { key = "reserved"; value = Alias (Normal (alias, base)) } ->
          Words.RESERVED.add_alias alias base; conf
      | Value { key = "reserved"; value = Alias (Context (alias, base)) } ->
          Words.RESERVED.add_alias (alias^"*") base; conf
      | Value { key = "reserved"; value } ->
          error "reserved" value
      | Value {key = "not-reserved"; value = String s } ->
          Words.RESERVED.remove_reserved s;
          Words.INTRINSIC.remove_intrinsic s;
          Words.SYSTEM_NAMES.remove_system_name s;
          Words.REGISTERS.remove_register s;
          conf
      | Value {key = "not-reserved"; value } ->
          error "not-reserved" value
      | Value {key = "intrinsic-function"; value = String s } ->
          Words.INTRINSIC.add_intrinsic s;
          conf
      | Value {key = "intrinsic-function"; value } ->
          error "intrinsic-function" value
      | Value {key = "not-intrinsic-function"; value = String s} ->
          Words.INTRINSIC.remove_intrinsic s;
          conf
      | Value {key = "not-intrinsic-function"; value} ->
          error "not-intrinsic-function" value
      | Value {key = "system-name"; value = String s} ->
          Words.SYSTEM_NAMES.add_system_name s;
          conf
      | Value {key = "system-name"; value} ->
          error "system-name" value
      | Value {key = "not-system-name"; value = String s} ->
          Words.SYSTEM_NAMES.remove_system_name s;
          conf
      | Value {key = "not-system-name"; value} ->
          error "not-system-name" value
      | Value {key = "register"; value = String s} ->
          Words.REGISTERS.add_register s;
          conf
      | Value {key = "register"; value} ->
          error "register" value
      | Value {key = "not-register"; value = String s} ->
          Words.REGISTERS.remove_register s;
          conf
      | Value {key = "not-register"; value} ->
          error "not-register" value
      | Value {key; value} ->
          let value = match value with
            | Support (Additional s) ->
                begin match StringMap.find_opt key conf with
                  | Some (Conf_ast.Support (Additional v | Normal v)) ->
                      Conf_ast.Support (Normal (meet_support s v))
                  | _ -> value
                end
            | _ -> value
          in
          let conf = StringMap.add key value conf in
          conf
      | Include s ->
          raise @@ ERROR (Missing_file (s, []))        (* should never happen *)
      | ReservedWords s ->
          raise @@ ERROR (Missing_file (s ^ ".words", []))           (* ditto *)
    end StringMap.empty conf_ptree
  in
  (conf,
   Words.RESERVED.words (),
   Words.INTRINSIC.intrinsic_functions (),
   Words.SYSTEM_NAMES.system_names (),
   Words.REGISTERS.registers ())

let parse_file file =
  let open Lexing in
  let module I = Conf_parser.MenhirInterpreter in
  let syntax_error ?loc msg = raise @@ ERROR (Syntax_error (loc, msg)) in
  let raw_loc (s, e) = Cobol_common.Srcloc.raw (s, e) in
  let rec handle_parser_error lexbuf checkpoint =
    match checkpoint with
    | I.HandlingError env ->
        begin match I.top env with
          | None ->
              syntax_error None
                ~loc:(raw_loc (lexbuf.lex_start_p, lexbuf.lex_curr_p))
          | Some (I.Element (state, _, start_pos, end_pos)) ->
              let msg =
                try Some (Conf_parser_messages.message (I.number state))
                with Not_found -> None
              in
              syntax_error msg
                ~loc:(raw_loc (start_pos, end_pos))
        end;
    | I.Rejected ->
        syntax_error (Some "Invalid input")
    | _ ->
        parse lexbuf checkpoint
  and parse lexbuf (checkpoint: Conf_ast.t I.checkpoint) =
    I.loop_handle
      Fun.id
      (handle_parser_error lexbuf)
      (fun () ->
         let token = Conf_lexer.main lexbuf in
         token, lexbuf.lex_start_p, lexbuf.lex_curr_p)
      checkpoint
  in
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ~with_positions:true ic in
  let ast =
    try parse lexbuf (Conf_parser.Incremental.file lexbuf.lex_curr_p)
    with Conf_lexer.ERROR e -> raise @@ ERROR (Lexing_error e)
  in
  close_in ic;
  ast

let load_file (module Words: Words.S) ?search_path file =
  let search_path = retrieve_search_path ?search_path () in
  let search_path = EzFile.dirname file :: search_path in
  let rec load acc file =
    let options = parse_file file in
    List.fold_left begin fun acc option -> match option with
      | Conf_ast.Value _ ->
          option :: acc
      | ReservedWords words
        when (String.lowercase_ascii words) = "default" ->
          Words.RESERVED.add_reserved "DIALECT-ALL";
          acc
      | ReservedWords words
        when (String.lowercase_ascii words) = "off" ->
          acc
      | ReservedWords words ->
          let basename = String.lowercase_ascii words in
          let filename = EzFile.add_suffix basename ".words" in
          load acc @@ find_file ~search_path filename
      | Include filename ->
          load acc @@ find_file ~search_path filename
    end acc options
  in
  List.rev @@ load [] file

let from_file ?search_path file =
  Pretty.error "Loading: `%s'@." file;
  let module Words = Words.Make () in
  let options = load_file (module Words) ?search_path file in
  let options, words, intrinsic, system_names, registers
    = make_conf (module Words) options in
  let module Diags = DIAGS.InitStateful () in
  let module Config =
    From_file.Make (Diags) (struct
      let config =
        let name =
          match StringMap.find "name" options with
          | Conf_ast.String s -> s
          | v -> raise @@ ERROR (Invalid_key_value_pair ("name", v))
        and strict =                                         (* NB: ugly hack *)
          EzString.ends_with ~suffix:"-strict.conf" file
        in
        { name; strict }
      let dialect =
        try DIALECT.of_name config.name
        with Invalid_argument _ ->
          raise @@ ERROR (Unknown_dialect config.name)
      let options = options
      let words = words
      let intrinsic_functions = intrinsic
      let system_names = system_names
      let registers = registers
    end)
  in
  DIAGS.result (module Config: T) ~diags:(Diags.inspect ~reset:true)

let from_dialect ?search_path ~strict d =
  let search_path = retrieve_search_path ?search_path () in
  let config_filename ~strict conf =
    Pretty.to_string "%s%s.conf" conf (if strict then "-strict" else "")
  in
  let load_gnucobol_conf ~strict conf =
    from_file ~search_path @@
    find_file ~search_path (config_filename ~strict conf)
  in
  match d with
  | DIALECT.Default -> DIAGS.result (module Default: T)
  | GnuCOBOL        -> load_gnucobol_conf ~strict:false "default"
  | COBOL85         -> load_gnucobol_conf ~strict:false "cobol85"
  | COBOL2002       -> load_gnucobol_conf ~strict:false "COBOL2002"
  | COBOL2014       -> load_gnucobol_conf ~strict:false "COBOL2014"
  | ACU             -> load_gnucobol_conf ~strict       "acu"
  | BS2000          -> load_gnucobol_conf ~strict       "bs2000"
  | GCOS            -> load_gnucobol_conf ~strict       "gcos"
  | IBM             -> load_gnucobol_conf ~strict       "ibm"
  | MicroFocus      -> load_gnucobol_conf ~strict       "mf"
  | MVS             -> load_gnucobol_conf ~strict       "mvs"
  | Realia          -> load_gnucobol_conf ~strict       "realia"
  | RM              -> load_gnucobol_conf ~strict       "rm"
  | XOpen           -> load_gnucobol_conf ~strict       "xopen"

let dialect (module C: T) = C.dialect
