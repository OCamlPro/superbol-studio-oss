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

open Cobol_common.Srcloc.TYPES

let debug_oc = ref None

(*
let message fmt =
  Format.kasprintf (fun s ->
      match !debug_oc with
      | None -> ()
      | Some oc ->
          Printf.fprintf oc "OVERLAY: %s\n%!" s) fmt
*)

(** Overlay limits (internal) *)
module Limit = struct

  type state = int
  type manager = state ref

  type t = Lexing.position

  let new_manager () =
    ref (-1)             (* Actually start at -2 (-1 is used in Lexing.dummy) *)

  (** [make_virtual manager filename] makes a new vitrual limit that is unique
      to the combination of [manager] and [filename]. In the result, [pos_lnum]
      holds the pre-computed hash for the limit. *)
  let make_virtual: manager -> string -> t = fun id pos_fname ->
    decr id;
    { Lexing.dummy_pos with pos_fname;
                            pos_cnum = !id;
                            pos_lnum = Hashtbl.hash !id }

  let is_virtual (l: t) : bool =
    l.pos_cnum < (-1)

  let equal (l1: t) (l2: t) =
    l1 == l2 ||
    l1.pos_cnum == l2.pos_cnum &&
    l1.pos_fname = l2.pos_fname

  let hash (l: t) =
    l.pos_lnum

  (* l1 = l2, or l1 was emitted before l2 *)
  let surely_predates (l1: t) (l2: t) =                          (* or equals *)
    if l1.pos_cnum < (-1)
    then l2.pos_cnum <= l1.pos_cnum
    else l1.pos_cnum > 0 && l1.pos_cnum <= l2.pos_cnum &&
         l1.pos_fname = l2.pos_fname

end
type limit = Limit.t

(** Weak hashtable where keys are overlay limits (internal) *)
module WLnks = Ephemeron.K1.Make (Limit)
module HLnks = Hashtbl.Make (Limit)

module type MANAGER = sig
  type limit := limit
  val id: string
  val limits: srcloc -> limit * limit
  val link_limits: limit -> limit -> unit
  val join_limits: limit * limit -> srcloc
  val pointwise: limit -> srcloc
  val restart: ?at: limit -> unit -> unit
  val forget: file_of: limit -> unit
  val with_temporary_copy: f: ('a -> 'b) -> 'a -> 'b
end

(** Managers for sequences of overlay token limits *)
type manager =
  {
    right_of: (srcloc * limit) HLnks.t; (** associates the left limit of a token
                                            to its location and the
                                            corresponding right limit. *)
    over_right_gap: limit HLnks.t;  (** associates the right limit of a token to
                                        the left limit of the next *)
    cache: (srcloc * limit) WLnks.t;
    id: string;                (** manager identifier (for logging/debugging) *)
    limit_manager: Limit.manager;
  }

(** Manager initialization *)
let new_manager: string -> manager =
  let id = ref 0 in
  fun manager_name ->
    incr id;
    {
      right_of = HLnks.create 42;
      over_right_gap = HLnks.create 42;
      cache = WLnks.create 42;
      id = Pretty.to_string "%s-%u" manager_name !id;
      limit_manager = Limit.new_manager ();
    }

(** Returns left and right (potentially fresh) limits for the given source
    location; for any given file, must be called with the leftmost location
    first. *)
let limits: manager -> srcloc -> limit * limit = fun ctx loc ->
  match Cobol_common.Srcloc.as_unique_lexloc loc with
  | Some (left, right)  (* filter pointwise locs out to avoid cycles (in case of
                           pointwise link) *)
    when not (Cobol_common.Srcloc.is_pointwise loc) ->
      HLnks.replace ctx.right_of left (loc, right);      (* Replace to deal with
                                                            duplicates. *)
      WLnks.remove ctx.cache left;             (* Manually remove from cache. *)
      WLnks.remove ctx.cache right;            (* same for `right` *)
      left, right
  | _ ->
      let refloc = Cobol_common.Srcloc.start_pos loc in
      let left = Limit.make_virtual ctx.limit_manager refloc.pos_fname in
      let right = Limit.make_virtual ctx.limit_manager refloc.pos_fname in
      HLnks.add ctx.right_of left (loc, right);
      left, right

(** Links token limits *)
let link_limits ctx left right =
  (* Replace to deal with overriding of limits during recovery/rewind. *)
  HLnks.replace ctx.over_right_gap left right

(** [pointwise ctx l] builds a source location that starts and ends at [l].

    May return {!Cobol_common.Srcloc.dummy} in case the given limit is invalid
    (i.e, [l] does not result from a call to [limits ctx loc], for some
    [loc]). *)
let pointwise ctx l =
  try
    let pos =
      if Limit.is_virtual l then
        match HLnks.find_opt ctx.over_right_gap l with
        | Some left ->
            let loc, _ = HLnks.find ctx.right_of left in
            Cobol_common.Srcloc.start_pos loc
        | None ->
            let loc, _ = HLnks.find ctx.right_of l in
            Cobol_common.Srcloc.start_pos loc
      else l
    in
    Cobol_common.Srcloc.raw (pos, pos)
  with Not_found ->
    Pretty.error "@[<2>%a:@ Internal@ warning:@ unable@ to@ build@ a@ proper@ \
                  pointwise@ location@ in@ `%s.pointwise`@ [ctx=%s]@]@."
      Cobol_common.Srcloc.pp_lexpos l
      __MODULE__ ctx.id;
    (* Printexc.(print_raw_backtrace Stdlib.stderr @@ get_callstack 30); *)
    Cobol_common.Srcloc.dummy

(** [join_limits ctx (s, e)] returns a source location that spans between the
    two given limits [s] and [e]; returns a valid pointwise location if [s] and
    [e] are physically equal.

    May print a warning on [stderr] and return {!Cobol_common.Srcloc.dummy} in
    case [s] does not precede [e] (in the sense that there was no chain of
    alternating calls to {!limits} and {!link_limits}) that start from [s] and
    end in [e]) *)
let join_limits: manager -> limit * limit -> srcloc = fun ctx (s, e) ->
  let try_limits (s, e: limit * limit) =

    let rec proceed_from ?loc s =         (* start search from left limit [s] *)
      check ?loc @@ HLnks.find ctx.right_of s

    and check ?loc (loc', e') =
      (* continue search with ([loc] concatenated with) [loc'] if [e'] is not
         the sought after right limit; raises {!Not_found} when reaching an
         unknown gap or limit *)
      let loc = match loc with
        | None -> loc'
        | Some loc -> Cobol_common.Srcloc.concat loc loc'
      in
      if Limit.equal e e'
      then (WLnks.replace ctx.cache s (loc, e); loc)
      else try_cache_from ~loc @@ HLnks.find ctx.over_right_gap e'

    and try_cache_from ?loc s =
      (* attempt with cache first; proceed via small-step upon miss or
         failure *)
      match WLnks.find_opt ctx.cache s with
      | Some ((_, e') as hit) when Limit.surely_predates e' e ->
          (try check ?loc hit with Not_found -> proceed_from ?loc s)
      | Some _ | None ->
          proceed_from ?loc s
    in

    if Limit.equal s e
    then pointwise ctx s
    else try_cache_from s
  in
  let join_failure (s, e) =
    Pretty.error "@[<2>%a:%a:@ Internal@ warning:@ unable@ to@ join@ locations@ \
                  via@ limits@ in@ `%s.join_limits`@ [ctx=%s]@]@."
      Cobol_common.Srcloc.pp_lexpos s
      Cobol_common.Srcloc.pp_lexpos e
      __MODULE__ ctx.id;
    (* Printexc.(print_raw_backtrace Stdlib.stderr @@ get_callstack 30); *)
    Cobol_common.Srcloc.dummy
  in
  try   (* first attempt assumes proper token limits: `s` is a left and `e` is a
           right of tokens *)
    try_limits (s, e)
  with Not_found ->
  try                        (* otherwise try assuming `s` is an end of token *)
    try_limits (HLnks.find ctx.over_right_gap s, e)
  with Not_found ->
    join_failure (s, e)

(* Recursively traverse and empty `right_of` and `over_right_gap` from the given
   left or right limit. *)
let rec clear_right_of ctx left =
  match HLnks.find_opt ctx.right_of left with
  | Some (_, right) ->
      HLnks.remove ctx.right_of left;
      clear_right_gap ctx right
  | None -> ()
and clear_right_gap ctx right =
  match HLnks.find_opt ctx.over_right_gap right with
  | Some left ->
      HLnks.remove ctx.over_right_gap right;
      clear_right_of ctx left
  | None -> ()

let restart ?at ctx =
  WLnks.clear ctx.cache;
  match at with
  | Some right ->
      clear_right_of ctx @@ HLnks.find ctx.over_right_gap right
  | None ->
      HLnks.clear ctx.right_of;
      HLnks.clear ctx.over_right_gap

let forget ~file_of ctx =
  let filename = file_of.Lexing.pos_fname in
  WLnks.clear ctx.cache;
  HLnks.filter_map_inplace begin fun left ((_, right) as next) ->
    if left.pos_fname = filename
    then (clear_right_gap ctx right; None)
    else Some next
  end ctx.right_of;
  HLnks.filter_map_inplace begin fun right left ->
    if right.pos_fname = filename
    then (clear_right_of ctx left; None)
    else Some left
  end ctx.over_right_gap

let with_temporary_copy ~f ctx a =
  WLnks.clear ctx.cache;
  let right_of       = HLnks.copy ctx.right_of
  and over_right_gap = HLnks.copy ctx.over_right_gap in
  let restore () =
    WLnks.clear ctx.cache;
    HLnks.(clear   ctx.right_of;
           add_seq ctx.right_of        @@ to_seq right_of);
    HLnks.(clear   ctx.over_right_gap;
           add_seq ctx.over_right_gap  @@ to_seq over_right_gap);
  in
  let res =
    try f a with e -> restore (); raise e
  in
  restore (); res

module New_manager (Id: sig val name: string end) () : MANAGER = struct
  let ctx = new_manager Id.name
  let id = ctx.id
  let limits = limits ctx
  let link_limits = link_limits ctx
  let join_limits = join_limits ctx
  let pointwise = pointwise ctx
  let restart ?at () = restart ?at ctx
  let forget ~file_of = forget ~file_of ctx
  let with_temporary_copy = with_temporary_copy ctx
end
