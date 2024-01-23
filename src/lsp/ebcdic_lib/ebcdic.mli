(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(*
// Copyright 2017 Rohit Joshi <rohit.c.joshi@gmail.com>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.
*)

type translation = {
  to_ascii : int array ;
  of_ascii : int array ;
}

val default : translation
val non_printable : bool array

val ebcdic_to_ascii :
  ?non_printable_to_space:bool ->
  ?nel_to_lf:bool ->
  ?translation:translation ->
  string -> bytes
val ascii_to_ebcdic :
  ?lf_to_nel:bool ->
  ?translation: translation ->
  string -> bytes

val read_gnucobol_collation_file : string -> translation
