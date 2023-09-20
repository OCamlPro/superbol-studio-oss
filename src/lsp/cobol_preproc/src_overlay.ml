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

module TYPES = struct
  type limit = Lexing.position
end
include TYPES

type srcloc = Cobol_common.Srcloc.srcloc  (* alias for shortening definitions *)

module type MANAGER = sig
  type limit := limit
  val id: string
  val limits: srcloc -> limit * limit
  val link_limits: limit -> limit -> unit
  val join_limits: limit * limit -> srcloc
  val dummy_limit: limit
end

(** Overlay limits (internal) *)
module Limit = struct

  type t = limit

  let make_virtual: unit -> t =
    let id = ref (-1) in (* Actually start at -2 (-1 is used in Lexing.dummy) *)
    fun () ->
      decr id;
      Lexing.{ dummy_pos with pos_cnum = !id }

  let is_virtual (l: t) : bool =
    l.pos_cnum < (-1)

  let equal (l1: t) (l2: t) =
    l1.pos_cnum == l2.pos_cnum  &&
    l1.pos_fname = l2.pos_fname

  let hash (l: limit) = l.pos_cnum

  (* l1 = l2, or l1 was emitted before l2 *)
  let surely_predates (l1: limit) (l2: limit) =                  (* or equals *)
    if l1.pos_cnum < (-1)
    then l2.pos_cnum <= l1.pos_cnum
    else l1.pos_cnum > 0 && l1.pos_cnum <= l2.pos_cnum &&
         l1.pos_fname = l2.pos_fname
end

(** Weak hashtable where keys are overlay limits (internal) *)
module Links = Ephemeron.K1.Make (Limit)

(** Managers for sequences of overlay token limits *)
type manager =
  {
    right_of: (srcloc * limit) Links.t; (** associates the left limit of a token
                                            to its location and the
                                            corresponding right limit. *)
    over_right_gap: limit Links.t;  (** associates the right limit of a token to
                                        the left limit of the next *)
    cache: (srcloc * limit) Links.t;
    id: string;                (** manager identifier (for logging/debugging) *)
  }

(** Manager initialization *)
let new_manager: string -> manager =
  let id = ref 0 in
  fun manager_name ->
    incr id;
    {
      right_of = Links.create 42;
      over_right_gap = Links.create 42;
      cache = Links.create 42;
      id = Pretty.to_string "%s-%u" manager_name !id;
    }

(** Returns left and right (potentially fresh) limits for the given source
    location; for any given file, must be called with the leftmost location
    first. *)
let limits: manager -> srcloc -> limit * limit = fun ctx loc ->
  let s, e = match Cobol_common.Srcloc.as_unique_lexloc loc with
    | Some lexloc -> lexloc
    | _ -> Limit.make_virtual (), Limit.make_virtual ()
  in
  Links.replace ctx.right_of s (loc, e);   (* replace to deal with duplicates *)
  s, e

(** Links token limits *)
let link_limits ctx left right =
  (* Replace to deal with overriding of limits during recovery. *)
  Links.replace ctx.over_right_gap left right

(** Returns a source location that spans between two given limits; returns a
    valid pointwise location if the two given limits are physically equal. *)
let join_limits: manager -> limit * limit -> srcloc = fun ctx (s, e) ->
  let pointwise l =          (* pointwise: ensure this is not a virtual limit *)
    let pos =
      if Limit.is_virtual l then
        let s = Links.find ctx.over_right_gap l in
        let loc, _ = Links.find ctx.right_of s in
        Cobol_common.Srcloc.start_pos loc
      else l
    in
    Cobol_common.Srcloc.raw (pos, pos)
  in
  let try_limits (s, e: limit * limit) =

    let rec proceed_from ?loc s =         (* start search from left limit [s] *)
      check ?loc @@ Links.find ctx.right_of s

    and check ?loc (loc', e') =
      (* continue search with ([loc] concatenated with) [loc'] if [e'] is not
         the sought after right limit; raises {!Not_found} when reaching an
         unknown gap or limit *)
      let loc = match loc with
        | None -> loc'
        | Some loc -> Cobol_common.Srcloc.concat loc loc'
      in
      if e.pos_cnum  = e'.pos_cnum &&   (* compare only the fields that matter *)
         e.pos_fname = e'.pos_fname
      then (Links.replace ctx.cache s (loc, e); loc)
      else try_cache_from ~loc @@ Links.find ctx.over_right_gap e'

    and try_cache_from ?loc s =
      (* attempt with cache first; proceed via small-step upon miss or
         failure *)
      match Links.find_opt ctx.cache s with
      | Some ((_, e') as hit) when Limit.surely_predates e' e ->
          (try check ?loc hit with Not_found -> proceed_from ?loc s)
      | Some _ | None ->
          proceed_from ?loc s
    in

    if s == e
    then pointwise s
    else try_cache_from s
  in
  let join_failure (s, e) =
    let loc = Cobol_common.Srcloc.raw (s, e) in
    Pretty.error "@[<2>%a:@ Internal@ warning:@ unable@ to@ join@ locations@ \
                  via@ limits@ in@ `%s.join_limits`@ [ctx=%s]@]@."
      Cobol_common.Srcloc.pp_file_loc loc __MODULE__ ctx.id;
    (* Printexc.(print_raw_backtrace Stdlib.stderr @@ get_callstack 10); *)
    loc
  in
  try   (* first attempt assumes proper token limits: `s` is a left and `e` is a
           right of tokens *)
    try_limits (s, e)
  with Not_found ->
  try                        (* otherwise try assuming `s` is an end of token *)
    try_limits (Links.find ctx.over_right_gap s, e)
  with Not_found ->
    join_failure (s, e)

module New_manager (Id: sig val name: string end) : MANAGER = struct
  let ctx = new_manager Id.name
  let id = ctx.id
  let limits = limits ctx
  let link_limits = link_limits ctx
  let join_limits = join_limits ctx
  let dummy_limit = Lexing.dummy_pos
end
