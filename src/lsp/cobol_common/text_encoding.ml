(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

type endianness = LE | BE

type kind =
  | UTF8
  | UTF16 of endianness
  | Single_byte of int array        (* code points of bytes 0x80 to 0xFF *)

type t = { name: string; kind: kind }

let encodings =
  [ { name = "utf8"; kind = UTF8 };
    { name = "utf8bom"; kind = UTF8 };
    { name = "utf16le"; kind = UTF16 LE };
    { name = "utf16be"; kind = UTF16 BE } ] @
  List.map (fun (name, upper_half) -> { name; kind = Single_byte upper_half })
    Text_encoding_tables.upper_halves

let supported_names = List.map (fun { name; _ } -> name) encodings

let name { name; _ } = name

let of_name name =
  match List.find_opt (fun e -> e.name = name) encodings with
  | Some e -> Ok e
  | None ->
      Error (Pretty.to_string "Unsupported@ encoding@ `%s'@ (supported@ \
                               encodings@ are:@ %a)" name
               Fmt.(list ~sep:comma string) supported_names)

(* --- *)

let utf8_bom = "\xEF\xBB\xBF"
let utf16le_bom = "\xFF\xFE"
let utf16be_bom = "\xFE\xFF"

let starts_with ~prefix s =
  String.length s >= String.length prefix &&
  String.sub s 0 (String.length prefix) = prefix

(* Byte order marks take precedence over the configured encoding, as in VS
   Code. *)
let bom_kind s =
  if starts_with ~prefix:utf8_bom s then Some (UTF8, String.length utf8_bom)
  else if starts_with ~prefix:utf16le_bom s then Some (UTF16 LE, 2)
  else if starts_with ~prefix:utf16be_bom s then Some (UTF16 BE, 2)
  else None

let decode_utf8 s ~from =
  let len = String.length s in
  if from = 0 && String.is_valid_utf_8 s
  then s
  else begin
    let buf = Buffer.create (len - from) in
    let rec loop i =
      if i < len then begin
        (* On ill-formed input, [d] is U+FFFD and spans a maximal subpart. *)
        let d = String.get_utf_8_uchar s i in
        Buffer.add_utf_8_uchar buf (Uchar.utf_decode_uchar d);
        loop (i + Uchar.utf_decode_length d)
      end
    in
    loop from;
    Buffer.contents buf
  end

let decode_utf16 endianness s ~from =
  let get = match endianness with
    | LE -> String.get_utf_16le_uchar
    | BE -> String.get_utf_16be_uchar
  in
  (* An odd trailing byte is dropped, as iconv-lite does. *)
  let len = from + (String.length s - from) land (lnot 1) in
  let buf = Buffer.create (len - from) in
  let rec loop i =
    if i < len then begin
      let d = get s i in
      Buffer.add_utf_8_uchar buf (Uchar.utf_decode_uchar d);
      loop (i + Uchar.utf_decode_length d)
    end
  in
  loop from;
  Buffer.contents buf

let is_ascii s =
  let rec loop i = i < 0 || (Char.code s.[i] < 0x80 && loop (pred i)) in
  loop (String.length s - 1)

let decode_single_byte upper_half s =
  if is_ascii s
  then s
  else begin
    let buf = Buffer.create (String.length s + String.length s / 2) in
    String.iter begin fun c ->
      let b = Char.code c in
      if b < 0x80
      then Buffer.add_char buf c
      else Buffer.add_utf_8_uchar buf (Uchar.of_int upper_half.(b - 0x80))
    end s;
    Buffer.contents buf
  end

let decode { kind; _ } s =
  let kind, from = Option.value (bom_kind s) ~default:(kind, 0) in
  match kind with
  | UTF8 -> decode_utf8 s ~from
  | UTF16 endianness -> decode_utf16 endianness s ~from
  | Single_byte upper_half -> decode_single_byte upper_half s
