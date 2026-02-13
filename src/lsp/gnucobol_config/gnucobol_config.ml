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

let fallback_config_dir_var = "COB_CONFIG_DIR_FALLBACK"

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
  let append root ~sub = match root with
    | Some d -> [d // sub]
    | None -> []
  in
  lazy begin
    let cwd = EzFile.getcwd () in
    let cob_config_dir =
      Option.to_list (Sys.getenv_opt "COB_CONFIG_DIR")
    and fallback_dir =
      Option.to_list (Sys.getenv_opt fallback_config_dir_var)
    in
    let xdg_superbol_dir =      (* is this available on win32/cygwin as well? *)
      append ~sub:"superbol/gnucobol-config" @@
      match Sys.getenv_opt "XDG_CONFIG_HOME" with
      | Some p -> Some p
      | None -> Option.map (fun d -> d // ".config") (Sys.getenv_opt "HOME")
    and windose_specific =
      if Sys.(win32 || cygwin)
      then append ~sub:"SuperBOL" (Sys.getenv_opt "APPDATA") @
           append ~sub:"SuperBOL" (Sys.getenv_opt "LOCALAPPDATA")
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

let from_file ?search_path ?(verbose=false) file =
  if verbose then
    Pretty.error "Loading: `%s'@." file;
  let module Words = Words.Make () in
  let options = load_file (module Words) ?search_path file in
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
  DIAGS.result (module Config: T) ~diags:(Diags.inspect ~reset:true)

let from_dialect ?search_path ?verbose d =
  let search_path = retrieve_search_path ?search_path () in
  let config_filename = function
    | DIALECT.GnuCOBOL -> "default.conf"
    | dialect -> Pretty.to_string "%s.conf" (DIALECT.to_string dialect)
  in
  let load_gnucobol_conf conf =
    from_file ~search_path ?verbose @@
    find_file ~search_path (config_filename conf)
  in
  match d with
  | DIALECT.Default -> DIAGS.result (module Default: T)
  | d               -> load_gnucobol_conf d

let dialect (module C: T) = C.dialect

module COMMON_CONFIG = struct
  open Cobol_common.Config.TYPES

  let of_dialect dialect =
    match dialect with
    | DIALECT.MicroFocus _ -> Dialect_MicroFocus
    | GnuCOBOL -> Dialect_GnuCOBOL
    | COBOL85 -> Dialect_STD85
    | COBOL2002 -> Dialect_STD2002
    | COBOL2014 -> Dialect_STD2014
    | XOpen -> Dialect_Other "XOpen"
    | ACU _ -> Dialect_ACU
    | BS2000 _ -> Dialect_BS2000
    | GCOS _ -> Dialect_Other "GCOS"
    | IBM _ -> Dialect_IBM
    | MVS _ -> Dialect_MVS
    | Realia _ -> Dialect_Other "Realia"
    | RM _ -> Dialect_RM
    | Custom (name, _) -> Dialect_Other name
    | Default -> Dialect_Other "Default"

  let dialect (module C : T) = of_dialect C.dialect

  let of_level = function
    | FEATURE.Ok _ -> Level_ok
    | Error -> Level_error
    | Skip -> Level_skip
    | Ignore -> Level_ignore
    | Unconformable -> Level_unconformable
    | Warning _ -> Level_warning
    | Archaic _ -> Level_archaic
    | Obsolete _ -> Level_obsolete

  let of_config (module C : T) =
      {
        Cobol_common.Config.TYPES.pic_length = C.pic_length#value ;
        dialect = of_dialect C.dialect ;
        features = {
          safe_partial_replacing_when_src_literal = {
            feature_name = { name = "safe_partial_replacing_when_src_literal" };
            feature_safe = false ;
            feature_level =
              of_level C.safe_partial_replacing_when_src_literal#level ;
          };
          free_redefines_position = {
            feature_name = { name = "free_redefines_position" };
            feature_safe = true ;
            feature_level =
              of_level C.free_redefines_position#level ;
          };
        };
        intrinsic_functions = C.intrinsic_functions ;
        words = C.words ;
      }

end
