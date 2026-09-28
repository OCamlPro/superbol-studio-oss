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

let encode ?(tag = Producer.tag) payload =
  tag ^ payload

let decode ?(tag = Producer.tag) item =
  let n = Producer.tag_length in
  if String.length item < n then Error Truncated else
    let tag' = String.sub item 0 n in
    if tag' <> tag
    then Error (Bad_version_tag { got = tag'; expected = tag })
    else Ok (String.sub item n (String.length item - n))

let marshal v =
  match Marshal.to_string v [] with
  | payload -> Ok payload
  | exception Invalid_argument msg ->            (* typically: holds a closure *)
      Error (Malformed msg)

let unmarshal payload =
  match Marshal.from_string payload 0 with
  | v -> Ok v
  | exception Failure msg -> Error (Malformed msg)
  | exception End_of_file -> Error Truncated

let write_item v =
  Result.map encode (marshal v)

let read_item item =
  Result.bind (decode item) unmarshal
