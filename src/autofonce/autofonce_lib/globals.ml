(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.0, as described in the LICENSE.md file in the root  *)
(*  directory of this source tree.                                        *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

let verbose = ref 1


let max_file_size = 10_000_000
let overflow_msg = Printf.sprintf "[CUT - EXCEED MAX SIZE OF %d]" max_file_size
let overflow_msg_len = String.length overflow_msg
let read_text_file_partial filename =
  let st = Unix.lstat filename in
  let size = st.Unix.st_size in
  let overflow = size > max_file_size in
  let size = if overflow then max_file_size else size in
  (* binary mode: in text mode on Windows, CRLF conversion makes us read
     fewer bytes than [st_size], raising End_of_file *)
  let ic = open_in_bin filename in
  let bytes = Bytes.create size in
  really_input ic bytes 0 size;
  close_in ic;
  if overflow then
    Bytes.blit_string overflow_msg 0 bytes (size-overflow_msg_len) overflow_msg_len;
  Bytes.unsafe_to_string bytes
