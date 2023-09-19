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
      id = Pretty.to_string "%s-%u" manager_name !id;
    }

(** Returns left and right (potentially fresh) limits for the given source
    location *)
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

(** [leftmost_limit_in ~filename ctx] finds the leftmost limit from a location
    in [filename] that is registered in [ctx] (internal).  Use with moderation
    as this is quite inefficient. *)
let leftmost_limit_in ~filename ctx =
  Links.fold begin fun l _ -> function
    | None when l.Lexing.pos_fname = filename -> Some l
    | Some l' when l.Lexing.pos_cnum < l'.pos_cnum &&
                   l.Lexing.pos_fname = filename -> Some l
    | res -> res
  end ctx.right_of None

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
  let try_limits (s, e) =
    let rec jump_right loc e' =
      let s' = Links.find ctx.over_right_gap e' in
      let loc', e' = Links.find ctx.right_of s' in
      check (Cobol_common.Srcloc.concat loc loc') e'
    and check loc e' =
      if e == e'                                        (* physical comparison *)
      then loc
      else jump_right loc e'
    in
    if s == e
    then pointwise s
    else let loc, e' = Links.find ctx.right_of s in check loc e'
  in
  let join_failure (s, e) =
    let loc = Cobol_common.Srcloc.raw (s, e) in
    Pretty.error "@[<2>%a:@ Internal@ warning:@ unable@ to@ join@ locations@ \
                  via@ limits@ in@ `%s.join_limits`@ [ctx=%s]@]@."
      Cobol_common.Srcloc.pp_file_loc loc __MODULE__ ctx.id;
    (* Printexc.(print_raw_backtrace Stdlib.stderr @@ get_callstack 10); *)
    loc
  in
  (* first attempt assumes proper token limits: `s` is a left and `e` is a right
     of tokens *)
  try try_limits (s, e) with Not_found ->
  (* try assuming `s` is an end of token *)
  try try_limits (Links.find ctx.over_right_gap s, e) with Not_found ->
    if s.pos_cnum = 0 (* potential special case with left-position forged by the
                         parser: retry with leftmost limit if it differs from
                         s *)
    then match leftmost_limit_in ~filename:s.pos_fname ctx with
      | Some l when l != s -> try_limits (l, e)  (* physical equality is enough *)
      | Some _ | None -> join_failure (s, e)
    else join_failure (s, e)

module New_manager (Id: sig val name: string end) : MANAGER = struct
  let ctx = new_manager Id.name
  let id = ctx.id
  let limits = limits ctx
  let link_limits = link_limits ctx
  let join_limits = join_limits ctx
  let dummy_limit = Lexing.dummy_pos
end
