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

type 'a result =
  | Ok of 'a
  | Error of string

val check_project : string ->
  (* warnings *) string list * (* errors *) string list
val read_file : string -> 'a Json_encoding.encoding -> 'a result
val write_file : string -> 'a Json_encoding.encoding -> 'a -> unit
