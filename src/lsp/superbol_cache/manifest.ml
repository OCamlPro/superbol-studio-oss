(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)


open Types

let shallow_key ~label ~inputs =
  (* The producer tag takes part in the key rather than being checked after the
     fact, so that entries written by another binary are not even reachable. *)
  Digest.to_hex @@ Digest.string @@
  String.concat "\x00" (Producer.tag :: label :: List.map Digest.to_hex inputs)

(* --- *)

(* Fields are separated by spaces and records by newlines, so those two
   characters -- and the escape character itself -- are the ones that have to go
   away. *)
let escape s =
  let buf = Buffer.create (String.length s) in
  String.iter begin function
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | ' ' -> Buffer.add_string buf "\\s"
    | c -> Buffer.add_char buf c
  end s;
  Buffer.contents buf

exception Bad_escape

let unescape s =
  let buf = Buffer.create (String.length s) in
  let n = String.length s in
  let rec loop i =
    if i >= n then Buffer.contents buf else
      match s.[i] with
      | '\\' when i + 1 >= n -> raise Bad_escape
      | '\\' ->
          begin match s.[i + 1] with
            | '\\' -> Buffer.add_char buf '\\'
            | 'n' -> Buffer.add_char buf '\n'
            | 's' -> Buffer.add_char buf ' '
            | _ -> raise Bad_escape
          end;
          loop (i + 2)
      | c -> Buffer.add_char buf c; loop (i + 1)
  in
  loop 0

let encode { manifest_label; manifest_witnesses; manifest_object } =
  let record = function
    | File { name; digest } ->
        Printf.sprintf "F %s %s" (Digest.to_hex digest) (escape name)
    | Dir { name; digest } ->
        Printf.sprintf "D %s %s" (Digest.to_hex digest) (escape name)
    | Env { var; value = None } ->
        Printf.sprintf "E %s" (escape var)
    | Env { var; value = Some value } ->
        Printf.sprintf "V %s %s" (escape var) (escape value)
    | Sub { key } ->
        Printf.sprintf "S %s" key
  in
  String.concat "\n" @@
  Printf.sprintf "L %s" (escape manifest_label) ::
  Printf.sprintf "O %s" (Digest.to_hex manifest_object) ::
  List.map record manifest_witnesses

(* --- *)

exception Bad_record

let digest_of_hex hex =
  match Digest.from_hex hex with
  | digest -> digest
  | exception _ -> raise Bad_record

let decode s =
  let record (label, obj, witnesses) line =
    match String.split_on_char ' ' line with
    | ["L"; l] -> unescape l, obj, witnesses
    | ["O"; d] -> label, Some (digest_of_hex d), witnesses
    | ["F"; d; name] ->
        label, obj,
        File { name = unescape name; digest = digest_of_hex d } :: witnesses
    | ["D"; d; name] ->
        label, obj,
        Dir { name = unescape name; digest = digest_of_hex d } :: witnesses
    | ["E"; var] ->
        label, obj, Env { var = unescape var; value = None } :: witnesses
    | ["V"; var; value] ->
        label, obj,
        Env { var = unescape var; value = Some (unescape value) } :: witnesses
    | ["S"; key] ->
        label, obj, Sub { key } :: witnesses
    | _ -> raise Bad_record
  in
  match List.fold_left record ("", None, []) (String.split_on_char '\n' s) with
  | _, None, _ -> Error (Malformed "manifest names no value")
  | label, Some manifest_object, witnesses ->
      Ok { manifest_label = label;
           manifest_object;
           manifest_witnesses = List.rev witnesses }
  | exception (Bad_record | Bad_escape) ->
      Error (Malformed "manifest is not well-formed")
