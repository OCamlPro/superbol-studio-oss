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

(* language.configuration file *)

type comments = {
  lineComment : string option ;
}
[@@deriving json_encoding]

type markers = {
  marker_start : string ;
  marker_end : string ;
}
[@@deriving json_encoding]

type folding = ( string * markers ) list [@assoc] (* "markers" *)
[@@deriving json_encoding]

type pair = {
  pair_open : string ;
  pair_close : string ;
  pair_notIn : string list ; [@dft []]
}
[@@deriving json_encoding]

let pair_enc =
  let open Json_encoding in
  union [
    case pair_enc
      (fun p -> Some p )
      (fun p -> p) ;
    case (list string)
      ( fun _p -> None )
      (function [ pair_open ; pair_close ] ->
         { pair_open ; pair_close ; pair_notIn = [] }
              | _ -> failwith "Bad pair encoding")
  ]

type onEnterRule = {
  beforeText : string ;
  endTest : string option ;
  action : ( string * string ) list ;[@assoc] [@dft []]
}
[@@deriving json_encoding]

type language = {
  comments : comments option ;
  brackets : string list list ; [@dft []]
  autoClosingPairs : pair list ; [@dft []]
  surroundingPairs : pair list ; [@dft []]
  wordPattern : string option ;
  folding : folding ; [@dft []]
  onEnterRules : onEnterRule list ; [@dft []]
}
[@@deriving json_encoding]
