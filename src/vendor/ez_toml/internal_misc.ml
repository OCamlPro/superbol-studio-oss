(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open EzCompat
open Types

let noloc = { file = "";
              line_begin = 0;
              line_end = 0;
              char_begin = 0;
              char_end = 0 }

let error ?(loc=noloc) n error = raise @@ Error ( loc, n, error )

let default_config = {
  debug = false ;
  silent_errors = IntSet.of_list [ 16 ] ;
  newline = "\n";
}

let node_counter = ref 0
let node ?(format=Any) ?(loc=noloc)
    ?(before=[]) ?(name=["???"]) ?after ?pos
    value =
  let node_pos = match pos with
    | None ->
      incr node_counter;
      !node_counter
    | Some pos -> pos
  in
  { node_loc = loc ;
    node_comment_before = before;
    node_comment_after = after;
    node_value = value ;
    node_format = format ;
    node_pos;
    node_name = name;
  }
