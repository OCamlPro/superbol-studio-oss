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

type snippet = {
  prefix : string Manifest.list_or_one;
  body : string Manifest.list_or_one ;
  scope : string option ;
  description : string option ;
}
[@@deriving json_encoding]

type snippets =
  ( string * snippet ) list [@assoc]
[@@deriving json_encoding]
