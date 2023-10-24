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

include Types

module Options = Options
module Default = Default

module DIAGS = Cobol_common.Diagnostics
module FATAL = DIAGS.Fatal           (* any error here should end up in exit. *)

let print_options: Pretty.delayed = fun ppf ->
  Pretty.list ~fopen:"@[<v>" ~fsep:"@\n" ~fclose:"@]" (fun ppf v -> v#pp ppf)
    ppf !all_configs

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

let make_conf conf =
  let error k v =
    FATAL.error "Unexpected error for `%s` key: %a" k Conf_ast.pp_value v
  in
  let conf =
    List.fold_left
      (fun conf elt ->
         match elt with
         | Conf_ast.Value {key = "reserved"; value} ->
             begin match value with
               | ContextWord s ->
                   Words.RESERVED.add_reserved (s^"*")
               | String s ->
                   Words.RESERVED.add_reserved s
               | Alias (Normal (alias, base)) ->
                   Words.RESERVED.add_alias alias base
               | Alias (Context (alias, base)) ->
                   Words.RESERVED.add_alias (alias^"*") base
               | _ -> error "reserved" value
             end;
             conf
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
             FATAL.error "Unresolved include: %s" s
         | ReservedWords s ->
             FATAL.error "Unresolved reserved words: %s.words" s)
      StringMap.empty
      conf
  in
  (conf,
   Words.RESERVED.words (),
   Words.INTRINSIC.intrinsic_functions (),
   Words.SYSTEM_NAMES.system_names (),
   Words.REGISTERS.registers ())


let parse_file file =
  let open Lexing in
  let module I = Conf_parser.MenhirInterpreter in
  let rec handle_parser_error lexbuf checkpoint =
    match checkpoint with
    | I.HandlingError env ->
        begin match I.stack env with
          | lazy Nil ->
              let loc = Cobol_common.Srcloc.raw (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
              FATAL.error ~loc "Syntax error"
          | lazy (Cons (I.Element (state, _, start_pos, end_pos), _)) ->
              let loc = Cobol_common.Srcloc.raw (start_pos, end_pos) in
              FATAL.error ~loc "Syntax error: %s" @@
              try Conf_parser_messages.message (I.number state) with Not_found -> ""
        end;
    | I.Rejected ->
        FATAL.error "Syntax error: input reject";
    | _ -> parse lexbuf checkpoint
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
    with Conf_lexer.LexError (msg, start_p, end_p) ->
      let loc = Cobol_common.Srcloc.raw (start_p, end_p) in
      FATAL.error ~loc "Lexing Error: %s" msg;
  in
  close_in ic;
  ast

let path_to_search =
  lazy begin
    let cwd = Ez_file.FileString.getcwd () in
    let l = "/usr/local/share/gnucobol/config"::[] in
    let l = match Sys.getenv_opt "COB_CONFIG_DIR" with
      | Some d -> d::l
      | None -> l
    in
    let xdg_conf = match Sys.getenv_opt "XDG_CONFIG_HOME" with
      | None -> Ez_file.FileString.add_path (Sys.getenv "HOME") ".config"
      | Some p -> p
    in
    cwd :: Ez_file.FileString.add_path xdg_conf "superbol" :: l
  end

let first_file paths filename =
  Cobol_common.Basics.LIST.fold_left_while
    Option.is_none
    (fun _ dir ->
       let filepath = Ez_file.FileString.add_path dir filename in
       if Ez_file.FileString.exists filepath then
         if not @@ Ez_file.FileString.is_directory filepath then
           Some filepath
         else
           None
       else
         None)
    None
    paths


let from_file ?(dialect: dialect option) file =
  let rec aux file =
    let options = parse_file file in
    let path_to_search =
      Ez_file.FileString.dirname file :: Lazy.force path_to_search
    in
    List.fold_left begin fun acc option -> match option with
      | Conf_ast.Value _ ->
          acc @ [option]
      | ReservedWords words
        when (String.lowercase_ascii words) <> "off" &&
             (String.lowercase_ascii words) <> "default" ->
          let basename = String.lowercase_ascii words in
          let filename = Ez_file.FileString.add_suffix basename ".words" in
          begin match first_file path_to_search filename with
            | Some file_path ->
                acc @ aux file_path
            | None ->
                FATAL.error "Words@ file@ not@ found:@ %s" filename;
          end
      | ReservedWords words
        when words = "default" ->
          Words.RESERVED.add_reserved "DIALECT-ALL";
          acc
      | ReservedWords _ ->
          acc
      | Include file ->
          begin match first_file path_to_search file with
            | Some file_path ->
                acc @ aux file_path
            | None ->
                FATAL.error "Configuration@ file@ not@ found:@ %s" file
          end
    end [] options
  in
  Pretty.error "@[Loading@ configuration@ from@ `%s'@]@." file;
  let options = aux file in
  let options, words, intrinsic, system_names, registers
    = make_conf options in
  let module Diags = DIAGS.InitStateful () in
  let module Config =
    From_file.Make (Diags) (struct
      let config =
        { name = match StringMap.find "name" options with
              | Conf_ast.String s -> s
              | v -> Pretty.failwith "Expecting a string for the `name' option, \
                                      got: %a" Conf_ast.pp_value v }
      let dialect = match dialect with
        | Some d -> d
        | None -> try DIALECT.of_name config.name with
          | Invalid_argument _ -> FATAL.error "unknown dialect: %s" config.name
      let options = options
      let words = words
      let intrinsic_functions = intrinsic
      let system_names = system_names
      let registers = registers
    end)
  in
  DIAGS.result (module Config: T) ~diags:(Diags.inspect ~reset:true)

let try_from_file dialect file =
  match first_file (Lazy.force path_to_search) file with
  | Some f ->
      from_file ~dialect f
  | None ->
      FATAL.error "Unable@ to@ locate@ a@ configuration@ file@ for@ dialect:@ \
                   `%s'" (DIALECT.name dialect)

let from_dialect ~strict d =
  let load_gnucobol_conf dialect ~strict confname =
    try_from_file dialect
      (Pretty.to_string "%s%s.conf" confname (if strict then "-strict" else ""))
  in
  match d with
  | DIALECT.Default -> DIAGS.result (module Default: T)
  | GnuCOBOL        -> load_gnucobol_conf d ~strict:false "default"
  | COBOL85         -> load_gnucobol_conf d ~strict:false "cobol85"
  | COBOL2002       -> load_gnucobol_conf d ~strict:false "COBOL2002"
  | COBOL2014       -> load_gnucobol_conf d ~strict:false "COBOL2014"
  | ACU             -> load_gnucobol_conf d ~strict       "acu"
  | BS2000          -> load_gnucobol_conf d ~strict       "bs2000"
  | GCOS            -> load_gnucobol_conf d ~strict       "gcos"
  | IBM             -> load_gnucobol_conf d ~strict       "ibm"
  | MicroFocus      -> load_gnucobol_conf d ~strict       "mf"
  | MVS             -> load_gnucobol_conf d ~strict       "mvs"
  | Realia          -> load_gnucobol_conf d ~strict       "realia"
  | RM              -> load_gnucobol_conf d ~strict       "rm"
  | XOpen           -> load_gnucobol_conf d ~strict       "xopen"

let dialect (module C: T) = C.dialect
