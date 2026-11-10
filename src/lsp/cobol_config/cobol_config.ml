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

module Types = Types
include Types

module Options = Options
module Default = Default
module Diagnostics = Config_diagnostics

module DIAGS = Cobol_common.Diagnostics
open Cobol_common.Platform.TYPES

let fallback_config_dir_var = "COB_CONFIG_DIR_FALLBACK"

exception ERROR of Config_diagnostics.error

let __init_default_exn_printers =
  Printexc.register_printer begin function
    | ERROR e ->
        Some (Pretty.to_string "@[<h>%a@]"
                (Config_diagnostics.pp_error ?platform:None) e)
    | _ ->
        None
  end

(* --- *)

let print_options: Pretty.delayed = fun ppf ->
  Pretty.list ~fopen:"@[<v>" ~fsep:"@\n" ~fclose:"@]" (fun ppf v -> v#pp ppf)
    ppf !all_configs

let default_search_path ~platform =
  let getenv_opt = platform.getenv_opt in
  let append root ~sub = match root with
    | Some d -> [d // sub]
    | None -> []
  in
  let cwd = platform.getcwd () in
  let cob_config_dir =
    Option.to_list (getenv_opt "COB_CONFIG_DIR")
  and fallback_dir =
    Option.to_list (getenv_opt fallback_config_dir_var)
  in
  let xdg_superbol_dir =        (* is this available on win32/cygwin as well? *)
    append ~sub:"superbol/gnucobol-config" @@
    match getenv_opt "XDG_CONFIG_HOME" with
    | Some p -> Some p
    | None -> Option.map (fun d -> d // ".config") (getenv_opt "HOME")
  and windose_specific =
    if Sys.(win32 || cygwin)
    then append ~sub:"SuperBOL" (getenv_opt "APPDATA") @
         append ~sub:"SuperBOL" (getenv_opt "LOCALAPPDATA")
    else []
  and unix_specific =
    if Sys.(unix || cygwin)
    then ["/usr/local/share/gnucobol/config";
          "/usr/share/gnucobol/config";
          "/etc/gnucobol"]
    else []
  in
  cwd :: cob_config_dir @ xdg_superbol_dir @ windose_specific @ unix_specific @
  fallback_dir

let retrieve_search_path ~platform ?search_path () =
  (match search_path with
   | Some p -> p
   | None -> []) @ default_search_path ~platform

(* Gives the path where [filename] is found in [search_path], along with its
   contents. *)
let read_file_in_path ~platform ~search_path filename =
  match (platform.read_text_file_in_path ~search_path filename
         : (string * string) option) with
  | Some found -> found
  | None -> raise @@ ERROR (Missing_file (filename, search_path))

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

let valid_tab_stops : Conf_ast.value -> bool = function
  | Int i -> Cobol_common.Tab_stops.is_valid [i]
  | Any s | String s ->
    (try Cobol_common.Tab_stops.is_valid (Cobol_common.Tab_stops.of_string s) with _ -> false)
  | _ -> false

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
      | Value { key = "tab-width"; value } when not (valid_tab_stops value) ->
          error "tab-width" value
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

let parse_contents ~filename contents =
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
  let lexbuf = Lexing.from_string ~with_positions:true contents in
  Lexing.set_filename lexbuf filename;
  try parse lexbuf (Conf_parser.Incremental.file lexbuf.lex_curr_p)
  with Conf_lexer.ERROR e -> raise @@ ERROR (Lexing_error e)

(* Gives the path where [file] is found, and its options, including those of the
   files it includes (looked up next to it first). *)
let load_file (module Words: Words.S) ~platform ?search_path file =
  let search_path = retrieve_search_path ~platform ?search_path () in
  let path, contents =
    read_file_in_path ~platform file
      ~search_path:(EzFile.dirname file :: search_path)
  in
  let search_path = EzFile.dirname path :: search_path in
  let rec load acc file =
    let path, contents = read_file_in_path ~platform ~search_path file in
    process acc ~path contents
  and process acc ~path contents =
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
          load acc filename
      | Include filename ->
          load acc filename
    end acc (parse_contents ~filename:path contents)
  in
  path, List.rev @@ process [] ~path contents

(* Same as [from_file], also giving the path of the file loaded. *)
let from_file_n_path ~platform ?search_path ?(verbose=false) file =
  if verbose then
    Pretty.error "Loading: `%s'@." file;
  let module Words = Words.Make () in
  let path, options = load_file (module Words) ~platform ?search_path file in
  let options, words, intrinsic, system_names, registers
    = make_conf (module Words) options in
  let module Diags = DIAGS.InitStateful () in
  let module Config =
    From_file.Make (Diags) (struct
      let config =
        { name = match StringMap.find "name" options with
              | Conf_ast.String s -> s
              | v -> raise @@ ERROR (Invalid_key_value_pair ("name", v)) }
      let dialect =
        try DIALECT.of_gnucobol_config_name config.name
        with Invalid_argument _ ->
          raise @@ ERROR (Unknown_dialect config.name)
      let options = options
      let words = words
      let intrinsic_functions = intrinsic
      let system_names = system_names
      let registers = registers
    end)
  in
  DIAGS.result ((module Config: T), path) ~diags:(Diags.inspect ~reset:true)

let from_file ~platform ?search_path ?verbose file =
  DIAGS.map_result ~f:fst @@
  from_file_n_path ~platform ?search_path ?verbose file

(* NB: [Some] is shadowed by a constructor of {!Types}. *)
let dialect_config_filename: DIALECT.t -> string option = function
  | DIALECT.Default ->
      Option.none
  | DIALECT.GnuCOBOL ->
      Option.some "default.conf"
  | dialect ->
      let name = DIALECT.to_string dialect in
      Option.some @@
      if String.ends_with ~suffix:".conf" name
      then name
      else Pretty.to_string "%s.conf" name

let from_dialect_n_file ~platform ?search_path ?verbose d =
  match (dialect_config_filename d: string option) with
  | None ->
      DIAGS.result ((module Default: T), Option.none)
  | Some filename ->
      DIAGS.map_result ~f:(fun (config, path) -> config, Option.some path) @@
      from_file_n_path ~platform ?search_path ?verbose filename

let from_dialect ~platform ?search_path ?verbose d =
  DIAGS.map_result ~f:fst @@
  from_dialect_n_file ~platform ?search_path ?verbose d

let dialect (module C: T) = C.dialect

let tab_stops (module C: T) = C.tab_stops#value
