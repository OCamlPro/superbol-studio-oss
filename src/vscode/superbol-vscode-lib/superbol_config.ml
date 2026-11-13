(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

module WS = Vscode.Workspace
module WS_CONF = Vscode.WorkspaceConfiguration

let section = "superbol"
let non_empy s = if s = "" then None else Some s

let full ?scope () =
  WS.getConfiguration ?scope ~section ()

let cobc_exe ?scope () =
  let config = WS.getConfiguration ?scope ~section () in
  match WS_CONF.get ~section:"cobcPath" config with
  | None -> None
  | Some o when Ojs.is_null o -> None
  | Some s -> non_empy (Ojs.string_of_js s)

let bool ?scope key =
  let config = WS.getConfiguration ?scope ~section () in
  match WS_CONF.get ~section:key config with
  | None -> false
  | Some o when Ojs.is_null o -> false
  | Some s -> Ojs.bool_of_js s

let string ?scope key =
  let config = WS.getConfiguration ?scope ~section () in
  match WS_CONF.get ~section:key config with
  | None -> ""
  | Some o when Ojs.is_null o -> ""
  | Some s -> Ojs.string_of_js s

let strings ?scope key =
  let config = WS.getConfiguration ?scope ~section () in
  match WS_CONF.get ~section:key config with
  | None -> []
  | Some o when Ojs.is_null o -> []
  | Some s -> Ojs.(list_of_js string_of_js) s      (* TODO: how may this fail? *)

(* [default] when unset, or not a positive integer. *)
let positive_int ?scope key ~default =
  let config = WS.getConfiguration ?scope ~section () in
  match WS_CONF.get ~section:key config with
  | None -> default
  | Some o when Ojs.is_null o || Ojs.type_of o <> "number" -> default
  | Some o -> match Ojs.int_of_js o with n when n > 0 -> n | _ -> default

let any ?scope key =
  let config = WS.getConfiguration ?scope ~section () in
  match WS_CONF.get ~section:key config with
  | None -> Ojs.null
  | Some s -> s

let any_list ?scope key =
  let config = WS.getConfiguration ?scope ~section () in
  match WS_CONF.get ~section:key config with
  | None -> []
  | Some o when Ojs.is_null o -> []                               (* required? *)
  | Some s -> [%js.to: Ojs.t list] s

(** {2 Encoding of COBOL files} *)

let string_in config ~key =
  match WS_CONF.get ~section:key config with
  | Some o when Ojs.type_of o = "string" -> Some (Ojs.string_of_js o)
  | _ -> None

(** [cobol_files_encoding ~scope] is the encoding VS Code opens COBOL files in
    [scope] with: [files.encoding], unless the [[cobol]] section overrides it.
    The LSP server follows the same rule (see `file_encoding_in_client_config'
    in `cobol_lsp/lsp_server.ml'). *)
let cobol_files_encoding ~scope =
  let files = WS.getConfiguration ~section:"files" ~scope () in
  let is_object o = Ojs.type_of o = "object" && not (Ojs.is_null o) in
  let string_prop o key =
    match Ojs.get_prop_ascii o key with
    | v when Ojs.type_of v = "string" -> Some (Ojs.string_of_js v)
    | _ -> None
  in
  let cobol_overrides =
    (* A plain object, that holds settings by their full names.  The section
       is given as [""] rather than omitted, as the binding would then pass
       [scope] in its place. *)
    match WS_CONF.get ~section:"[cobol]"
            (WS.getConfiguration ~section:"" ~scope ()) with
    | Some o when is_object o ->
        begin match string_prop o "files.encoding" with
          | Some name -> Some name
          | None ->
              let files = Ojs.get_prop_ascii o "files" in
              if is_object files then string_prop files "encoding" else None
        end
    | _ -> None
  in
  match cobol_overrides with
  | Some name -> Some name
  | None -> string_in files ~key:"encoding"

(** Whether VS Code guesses the encoding of files in [scope] (in which case the
    encoding of COBOL files may differ from {!cobol_files_encoding}). *)
let guesses_encoding ~scope =
  let files = WS.getConfiguration ~section:"files" ~scope () in
  match WS_CONF.get ~section:"autoGuessEncoding" files with
  | Some o when Ojs.type_of o = "boolean" -> Ojs.bool_of_js o
  | _ -> false

(** {2 Caches} *)

(** Whether [s] is a size as the server accepts it (see
    `Arg_utils.size_of_string'): a number of bytes, with an optional [k], [M]
    or [G] suffix. *)
let is_size s =
  s <> "" &&
  let digits =
    match s.[String.length s - 1] with
    | 'k' | 'K' | 'm' | 'M' | 'g' | 'G' -> String.sub s 0 (String.length s - 1)
    | _ -> s
  in
  digits <> "" && String.for_all (function '0'..'9' -> true | _ -> false) digits

(** [bytes_of_size s] is the number of bytes size [s] stands for. *)
let bytes_of_size s =
  let n = String.length s - 1 in
  match s.[n] with
  | 'k' | 'K' -> float_of_string (String.sub s 0 n) *. 1024.
  | 'm' | 'M' -> float_of_string (String.sub s 0 n) *. 1024. ** 2.
  | 'g' | 'G' -> float_of_string (String.sub s 0 n) *. 1024. ** 3.
  | _ -> float_of_string s

(** Size under which the server keeps the cache of each project (setting
    [cache.sizeLimit]); [None] if empty or invalid. *)
let cache_size_limit () =
  match String.trim @@ string "cache.sizeLimit" with
  | s when is_size s -> Some s
  | _ -> None

(** Number of days after which the server reclaims cache entries that have not
    been used (setting [cache.maxAge]); [None] for no limit. *)
let cache_max_age () =
  match positive_int "cache.maxAge" ~default:0 with
  | 0 -> None
  | days -> Some days
