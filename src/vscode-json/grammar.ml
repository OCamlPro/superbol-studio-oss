(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(* see https://macromates.com/manual/en/language_grammars *)

type capture_pattern = {
  pattern_include : string ; [@key "include"]
}
[@@deriving json_encoding]

type capture = {
  capture_name : string option ;
  capture_patterns : capture_pattern list option ;
}
[@@deriving json_encoding]

type captures = (string * capture) list [@assoc]
[@@deriving json_encoding]

type endCaptures = captures

let endCaptures_enc =
  let open Json_encoding in
  union [
    case string
      (fun _ -> None)
      (fun s -> [ "1", { capture_name = Some s ; capture_patterns = None } ]) ;
    case captures_enc
      (fun s -> Some s)
      (fun s -> s) ;
  ]

type pattern = {
  pat_match : string option ;
  pat_include : string option ;
  pat_name : string option;
  pat_captures : captures option ;
  pat_beginCaptures : captures option ;
  pat_whileCaptures : captures option ;
  pat_endCaptures : endCaptures option ;
  pat_begin : string option ;
  pat_end : string option ;
  pat_patterns : pattern list option ;
  pat_comment : string option ;
  pat_contentName : string option ;
  pat_while : string option ;
}
[@@deriving json_encoding {recursive}]

type patterns = (string * pattern) list [@assoc]
[@@deriving json_encoding]


type grammar = {
  prefix : string Manifest.list_or_one option;
  body : string Manifest.list_or_one option ;
  fileTypes : string list option ;
  name : string option ;
  scope : string option ;
  scopeName : string option ; (* error ? *)
  patterns : pattern list option ;
  repository : patterns option ;
  description : string option ;
  copyright : string option ; [@key "_copyright"]
  schema : string option ; [@key "$schema"]
  injectionSelector : string option ;
}
[@@deriving json_encoding]
