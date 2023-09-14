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

type 'a result =
  | Ok of 'a
  | Error of string

type errors = (* warnings *) string list * (* errors *) string list


val check_project : string -> errors
val check_file : 'a Json_encoding.encoding -> string -> errors

val read_file : string -> 'a Json_encoding.encoding -> 'a result
val write_file : string -> 'a Json_encoding.encoding -> 'a -> unit
