(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(* Fold over the lines of a given file ; don't fail on error *)
let foldFileLines f acc file =
  try
    let ic = open_in file in
    let rec loop acc =
      try loop (f acc (input_line ic))
      with _ -> close_in ic; acc
    in
    loop acc
  with _ -> acc

(* Escape \ and quotes *)
let escape s =
  let b = Buffer.create (String.length s) in
  String.iter (function
      | '\\' -> Buffer.add_string b "\\\\"
      | '"'  -> Buffer.add_string b "\\\""
      | c    -> Buffer.add_char b c) s;
  Buffer.contents b

(* This removes surrounding quotes and escapes quotes inside *)
(* Used to pass values from the VSCode UI to the MI2 backend. *)
let cleanRawValue s =
  let len = String.length s in
  let first = if len > 0 && s.[0] = '"' then 1 else 0 in
  let last = if len > 1 && s.[len-1] = '"' then len - 2 else len - 1 in
  let s =
    if first = 0 && last = len-1 then s
    else String.sub s first (last - first + 1)
  in
  let b = Buffer.create len in
  String.iter (function
      | '"'  -> Buffer.add_string b "\\\\\""
      | c    -> Buffer.add_char b c) s;
  Buffer.contents b

(* Remove the path and shortest extension of a filename, ie
   removePathExtension "/path/file.ext1.ext2" => file.ext1 *)
let removePathExtension f =
  Filename.remove_extension (Filename.basename f)

(* Remove the path and longest extension of a filename, ie
   removePathExtension "/path/file.ext1.ext2" => file *)
let stripPathExtension f =
  let rec aux f =
    let res = Filename.remove_extension f in
    if f == res then res else aux res
  in
  aux (Filename.basename f)

(* Like String.index but looks for a string instead of a char *)
let indexOf str s =
  try
    Str.search_forward (Str.regexp_string s) str 0
  with Not_found ->
    -1

(* Like String.rindex but looks for a string instead of a char *)
let lastIndexOf str s =
  try
    Str.search_backward (Str.regexp_string s) str (String.length str - 1)
  with Not_found ->
    -1

(* Remove everything after and including a given string *)
let removeAfterAndIncluding str s =
  let i = lastIndexOf str s in
  if i < 0 then str
  else String.sub str 0 i

(* Take a substring from a "start" position until the end *)
let stringSubToEnd s start =
  let len = max 0 (String.length s - start) in
  String.sub s start len

(* Take a substring from a "start" position until an
   "end" position (relative to end of string) *)
let stringSub s start end_ =
  let len = max 0 (String.length s - start - end_) in
  String.sub s start len

(* Turns an optional string into a string, mapping None to "" *)
let unoptString str_opt =
  match str_opt with
  | None -> ""
  | Some s -> s

(* Checks whether a string only contains digits *)
let isNumeric s =
  String.length s > 0 &&
  String.for_all (fun c -> c >= '0' && c <= '9') s

(* Returns the same list where the first element may or may
   not have been removed, depending on the given predicate *)
let filterHd f l =
  match l with
  | e :: l when not (f e) -> l
  | l -> l

(* Returns the same list where the first element may or may
   not have been modified by the given function *)
let mapHd f l =
  match l with
  | e :: l -> f e :: l
  | l -> l

(* Checks whether a regexp matches on a string *)
let matches regexp s =
  try ignore @@ Str.search_forward regexp s 0; true
  with _ -> false

(* Checks whether a regexp group was found *)
let hasGroup group =
  try ignore @@ Str.group_beginning group; true
  with _ -> false

(* Return a matched regexp group as string *)
let group s group =
  try Str.matched_group group s
  with _ -> ""

(* Return a matched regexp group as integer *)
let groupInt s group =
  try int_of_string (Str.matched_group group s)
  with _ -> 0
