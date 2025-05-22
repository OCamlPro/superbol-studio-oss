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

open Ez_file.V1

module TYPES = struct

  (* Tags used to refine the representation of source locations below. *)
  type raw_ = [ `Raw ]
  type cpy_ = [ `Cpy ]
  type rpl_ = [ `Rpl ]
  type cat_ = [ `Cat ]

  (** A location is any composition of simple lexing positions, *)
  type srcloc = any_ slt

  and any_ = [raw_|cpy_|rpl_|cat_]

  (* represented using a source location tree. *)
  and _ slt =
    | Raw: raw -> [>raw_] slt
    | Cpy: { copied: any_ slt; copyloc: copyloc } -> [>cpy_] slt
    | Rpl: replacement -> [>rpl_] slt
    | Cat: { left: left_ slt; right: right_ slt } -> [>cat_] slt
    (* invariant: Cat {left = Cpy _ ; right = Cpy _}: the two copies are not
       from the same file. *)

  (* Raw lexical range *)
  and lexloc = Lexing.position * Lexing.position
  and raw = Lexing.position * Lexing.position * bool           (* area-a flag *)

  (* Representation of a file copy operation *)
  and copyloc = { filename: string; copyloc: srcloc (* option *) }

  and replacement =
    {
      old: any_ slt;      (* should only keep ref. of starting pos if needed for
                             area-a detection... *)
      new_: srcloc;         (* location of replacement spec (e.g, pseudotext) *)
      in_area_a: bool;
      replloc: srcloc;
    }

  (* A (con)cat(enation) allows any sort of source location on its left... *)
  and left_ = any_

  (* ... but forbids other cats directly on its right. *)
  and right_ = [raw_|cpy_|rpl_]

  (** Sets of copied libraries *)
  type copylocs = copyloc list

  (** Values attached with a source location. *)
  type 'a with_loc = { payload: 'a; loc: srcloc [@compare fun _ _ -> 0] }
  [@@ deriving ord]

end
include TYPES

(* For debugging: *)

let pp_lexpos ppf Lexing.{ pos_fname; pos_lnum; pos_cnum; pos_bol } =
  Pretty.print ppf "%s:%d-%d" pos_fname pos_lnum (pos_cnum - pos_bol)

let pp_srcloc_struct: srcloc Pretty.printer =
  let pp_lexpos' ppf Lexing.{ pos_lnum; pos_cnum; pos_bol; _ } =
    Pretty.print ppf "%d-%d" pos_lnum (pos_cnum - pos_bol)
  in
  let rec pp: type t. t slt Pretty.printer = fun ppf -> function
    | Raw (s, e, _) ->
        Pretty.print ppf "<%a|%a>"
          pp_lexpos s pp_lexpos' e
    | Cpy { copied; _ } ->
        Pretty.print ppf "Cpy { copied = %a }"
          pp copied
    | Rpl { old; new_; replloc; _ } ->
        Pretty.print ppf "Rpl { @[matched = %a;\
                          @ replacement = %a;\
                          @ replloc = %a@] }"
          pp old pp new_ pp replloc
    | Cat { left; right } ->
        Pretty.print ppf "Cat { @[left = %a;@ right = %a@] }"
          pp left pp right
  in
  pp

(** {2 Querying source locations} *)

(** copied, but original (replaced) position upon replacing *)
let rec start_pos: type t. t slt -> Lexing.position = function
  | Raw (s, _, _) -> s
  | Cpy { copied; _ } -> start_pos copied
  | Rpl { old; _ } -> start_pos old
  | Cat { left; _ } -> start_pos left

(** [shallow_multiline_lexloc_in ~filename loc] retrieves a lexical location in
    [filename] from [loc], iff [loc] directly originates from [filename] and was
    not subject to any replacement or copy.  Returns [None] otherwise. *)
let shallow_multiline_lexloc_in ~filename loc =
  let rec aux: type t. t slt -> lexloc option = function
    | Raw (s, e, _) when s.pos_fname = filename -> Some (s, e)
    | Raw _ | Cpy _ | Rpl _ -> None
    | Cat { left; right } -> match aux left, aux right with
      | Some (s, _), Some (_, e)
      | Some (s, e), None
      | None, Some (s, e) -> Some (s, e)
      | None, None -> None
  in
  aux loc

(** [shallow_single_line_lexloc_in ~filename loc] is similar to
    {!shallow_multiline_lexloc_in}, except that any returned lexical location is
    guaranteed to span a single line. *)
let shallow_single_line_lexloc_in ~filename = function
  | Raw (s, e, _) when s.pos_fname = filename -> Some (s, e)
  | Raw _ | Cpy _ | Rpl _ | Cat _ -> None

let start_pos_in ~filename ~traverse_replaces =
  let rec aux: type t. t slt -> Lexing.position option = function
    | Raw (s, _, _) when s.pos_fname = filename -> Some s
    | Raw _ -> None
    | Cat { left; right } -> or_else left right
    | Cpy { copied; copyloc = { copyloc; _ } } -> or_else copied copyloc
    | Rpl { old; new_; _ } ->
        aux (if traverse_replaces then new_ else old)
  and or_else: type t u. t slt -> u slt -> _ = fun a b ->
    match aux a with None -> aux b | res -> res
  in
  aux

let end_pos_in ~filename ~traverse_replaces =
  let rec aux: type t. t slt -> Lexing.position option = function
    | Raw (_, e, _) when e.pos_fname = filename -> Some e
    | Raw _ -> None
    | Cat { left; right } -> or_else right left
    | Cpy { copied; copyloc = { copyloc; _ } } -> or_else copied copyloc
    | Rpl { old; new_; _ } ->
        aux (if traverse_replaces then new_ else old)
  and or_else: type t u. t slt -> u slt -> _ = fun a b ->
    match aux a with None -> aux b | res -> res
  in
  aux

(** [forget_preproc ~favor_direction ~traverse_copies ~traverse_replaces loc]
    eliminates any preprocessing operation from the source location [loc], and
    returns a valid lexical range in {b a} file that participated in its
    construction.

    - [favor_direction] indicates whether to favor searching for the range from
    the left or the right bound of the location;

    - if [traverse_copies] is false, the location of {[COPY ...]} preprocessor
    statemenents is considered as a whole and not decomposed.  If
    [traverse_copies] holds, returned ranges may belong to copybooks;

    - if [traverse_replaces] is false, the location of text that is subject to
    replacement is considered as a whole and not decomposed.  This can make the
    resulting range rather coarse in case of {[REPLACE]} or {[COPY
    ... REPLACING]}, as the considered location is then that of the whole text
    that participated in a match.  On the contrary, the opposite case
    ([traverse_replaces] holds) may lead to misleading results (with ranges that
    appear in replacement pseudotext).
*)
let forget_preproc
    ~(favor_direction: [`Left | `Right])
    ~(traverse_copies: bool)
    ~(traverse_replaces: bool) =
  let start_pos_in = start_pos_in ~traverse_replaces
  and end_pos_in = end_pos_in ~traverse_replaces in
  let rec aux: type t. t slt -> lexloc = function
    | Raw (s, e, _) ->
        s, e
    | Cpy { copied; copyloc = { copyloc; _ } } ->
        aux (if traverse_copies then copied else copyloc)
    | Rpl { new_; old; _ } ->
        aux (if traverse_replaces then new_ else old)
    | Cat { left; right } ->
        match favor_direction with
        | `Left ->
            let (Lexing.{ pos_fname = filename; _ } as s), e = aux left in
            s, Option.value (end_pos_in ~filename right) ~default:e
        | `Right ->
            let s, (Lexing.{ pos_fname = filename; _ } as e) = aux right in
            Option.value (start_pos_in ~filename left) ~default:s, e
  in
  aux

(** Default projection.  This is a shorthand for [forget_preproc
    ~favor_direction:`Left ~traverse_copies:true ~traverse_replaces:false]. *)
let as_lexloc: srcloc -> lexloc =
  forget_preproc ~favor_direction:`Left
    ~traverse_copies:true ~traverse_replaces:false

let lookup_ ~lookup ~lookup_name ~filename loc =
  match lookup ~filename loc with
  | None ->
      Pretty.invalid_arg
        "%s.%s: no part of \"%s\" was used to construct the given location (loc \
         = %a)" __MODULE__ lookup_name filename pp_srcloc_struct loc
  | Some s ->
      s

let start_pos_in =
  lookup_ ~lookup:(start_pos_in ~traverse_replaces:false)
    ~lookup_name:"start_pos_in"
let end_pos_in =
  lookup_ ~lookup:(end_pos_in ~traverse_replaces:false)
    ~lookup_name:"end_pos_in"
let shallow_multiline_lexloc_in =
  lookup_ ~lookup:shallow_multiline_lexloc_in
    ~lookup_name:"shallow_multiline_lexloc_in"
let shallow_single_line_lexloc_in =
  lookup_ ~lookup:shallow_single_line_lexloc_in
    ~lookup_name:"shallow_single_line_lexloc_in"
(* let shallow_single_line_lexlocs_in = *)
(*   lookup_ ~lookup:shallow_single_line_lexlocs_in *)
(*     ~lookup_name:"shallow_single_line_lexlocs_in" *)

let shallow_single_line_lexlocs_in
    ?(ignore_invalid_filename = false) ~filename loc =
  let rec aux: type t. t slt -> (lexloc list as 'a) -> 'a = function
    | Raw (s, e, _) when s.pos_fname = filename -> List.cons (s, e)
    | Raw _ | Cpy _ | Rpl _ -> Fun.id
    | Cat { left; right } -> fun acc -> acc |> aux right |> aux left
  in
  match aux loc [] with
  | l when ignore_invalid_filename -> l
  | _ :: _ as l -> l
  | [] ->
      Pretty.invalid_arg
        "%s.%s: no part of \"%s\" was used to construct the given location (loc \
         = %a)" __MODULE__ "shallow_single_line_lexlocs_in"
        filename pp_srcloc_struct loc

(** [lexloc_in ~filename loc] projects the source location [loc] on the file
    [filename] by eliminating relevant preprocessor-related locations.

    Raises {!Invalid_argument} in case no valid projection onto the file
    [filename] can be found; this never happens if at least one lexical range of
    the file [filename] participated in the construction of [loc]. *)
let lexloc_in ~filename loc =
  start_pos_in ~filename loc, end_pos_in ~filename loc

let as_unique_lexloc = function
  | Raw (s, e, _) -> Some (s, e)
  | _ -> None

(** [as_copy loc] returns a copybook filename associated with the location of
    the {[COPY]} directive if [loc] directly results from such a directive, and
    returns [None] otherwise. *)
let as_copy = function
  | Cpy { copyloc = { filename; copyloc }; _ } ->
      Some { payload = filename; loc = copyloc }
  | _ ->    (* CHECKME: COPY ... REPLACING ...: Rpl should be nested below Cpy *)
      None

(* --- *)

(** [in_area_a loc] indicates whether the location [loc] has a left-most raw
    location that was built with the [in_area_a] flag. *)
let rec in_area_a: srcloc -> bool = function
  | Raw (_, _, a) -> a
  | Cpy { copied; _ } -> in_area_a copied
  | Rpl { in_area_a; _ } -> in_area_a
  | Cat { left; _ } -> in_area_a left

let is_pointwise: srcloc -> bool = function
  | Raw (s, e, _) -> s.pos_cnum == e.pos_cnum && s.pos_fname = e.pos_fname
  | _ -> false

let scan ?(kind: [`TopDown | `BottomUp] = `TopDown) ~cpy ~rpl =
  let rec aux: type t. t slt -> 'a -> 'a = fun loc -> match loc, kind with
    | Raw _, _ -> Fun.id
    | (Cpy { copied; copyloc }
      ,             `TopDown ) -> fun acc -> acc |> cpy copyloc |> aux copied
    | (Cpy { copied; copyloc }
      ,             `BottomUp) -> fun acc -> acc |> aux copied |> cpy copyloc
    | (Rpl { old; replloc; _ }
      ,             `TopDown ) -> fun acc -> acc |> rpl replloc |> aux old
    | (Rpl { old; replloc; _ }
      ,             `BottomUp) -> fun acc -> acc |> aux old |> rpl replloc
    | (Cat { left; right }, _) -> fun acc -> acc |> aux left |> aux right
  in
  aux

let compare: srcloc -> srcloc -> int = fun a b ->
  (* TODO: change API to suggest doing the projections beforehand.  Maybe
     parameterize with an ordered list of copybook files. *)
  let project loc =
    forget_preproc ~favor_direction:`Left
      ~traverse_copies:false ~traverse_replaces:false loc
  in
  let a' = project a
  and b' = project b in
  match a', b' with
  | (sa, ea), (sb, eb)
    when sa.pos_fname = sb.pos_fname ->
      if sa.pos_cnum < sb.pos_cnum &&
         ea.pos_cnum > eb.pos_cnum
      then 1                                   (* a totally overlaps b: a > b *)
      else if sa.pos_cnum > sb.pos_cnum &&
              ea.pos_cnum < eb.pos_cnum
      then -1                                              (* converse: a < b *)
      else
        let sc = Int.compare sa.pos_cnum sb.pos_cnum in
        if sc <> 0
        then sc
        else Int.compare eb.pos_cnum ea.pos_cnum         (* larger is greater *)
  | a, b ->
      Stdlib.compare a b                                           (* for now *)


(** {2 Pretty-printing} *)

let retrieve_file_lines, register_file_contents =
  let module Cache =
    Ephemeron.K1.Make (struct
      include String
      let hash = Hashtbl.hash
    end)
  in
  let file_cache = lazy (Cache.create 3) in
  begin fun file ->
    let file_cache = Lazy.force file_cache in
    try Cache.find file_cache file
    with Not_found ->
      let lines = EzFile.read_lines file in
      Cache.add file_cache file lines;
      lines
  end,
  begin fun ~filename contents ->
    let file_cache = Lazy.force file_cache in
    let lines = Array.of_list @@ String.split_on_char '\n' contents in
    Cache.replace file_cache filename lines
  end

type raw_loc = string * (int * int) * (int * int)

let pp_file_loc ppf ((file, pos1, pos2): raw_loc) =
  Pretty.print ppf "%s:%a" file Fmt.text_loc (pos1, pos2)

(** Note this should always end with a newline character *)
let pp_raw_loc: raw_loc Pretty.printer =
  let b = lazy (Buffer.create 1000) in
  let find_source (file, pos1, pos2) =
    let line1 = fst pos1 in
    let line2 = fst pos2 in
    let col1 = snd pos1 in
    let col2 = snd pos2 in
    let col2, pad2 =
      if line1 == line2 && col1 == col2 then succ col2, 1 else col2, 0 in
    let lines = retrieve_file_lines file in
    let b = Lazy.force b in
    Buffer.clear b;
    for l = max 1 (line1 - 3) to min (Array.length lines) (line2 + 2) do
      let line = lines.(l - 1) in
      let len = String.length line in
      Printf.bprintf b "%4d %c %s\n" l
        (if l>=line1 && l<=line2 then '>' else ' ')
        line;
      if l = line1 && l = line2 then
        let str =
          String.mapi
            (fun idx c -> if idx > col1 && idx <= col2 then '^' else c)
            (String.make (min (len + 1 + pad2) (col2 + 1)) ' ')
        in
        Printf.bprintf b "----  %s\n" str;
      else if l = line1 then
        let str =
          String.mapi
            (fun idx c -> if idx > col1 then '^' else c)
            (String.make (len + 1) ' ')
        in
        Printf.bprintf b "----  %s\n" str;
      else if l > line1 && l < line2 then
        let str = String.make (len + 1) '^' in
        Printf.bprintf b "----  %s\n" str;
      else if l = line2 then
        let str =
          String.mapi
            (fun idx c -> if idx <= col2 then '^' else c )
            (String.make (min (len + 1 + pad2) (col2 + 1)) ' ')
        in
        Printf.bprintf b "----  %s\n" str;
    done;
    Buffer.contents b
  in
  fun ppf raw_loc ->
    let text = try find_source raw_loc with _ -> "" in
    Pretty.print ppf "%a:@\n@[@<0>%s@]" pp_file_loc raw_loc text

let same_copyloc { filename = f1; _ } { filename = f2; _ } =
  f1 = f2                                                             (* berk *)

let to_raw_loc
    Lexing.({ pos_lnum = l1; pos_bol = b1; pos_cnum = c1; pos_fname; _ },
            { pos_lnum = l2; pos_bol = b2; pos_cnum = c2; _ }) =
  pos_fname, (l1, c1 - b1), (l2, c2 - b2)

let pp_srcloc: srcloc Pretty.printer =
  let pp_transform_operation ~partial ppf = function
    | `Cpy { filename; copyloc }
      when partial ->
        Pretty.print ppf "%a:@;@[<h>partially@ in@ `%s`,@ copied@ at@ this@ \
                          location@]"
          pp_file_loc (to_raw_loc @@ as_lexloc copyloc) filename
    | `Cpy { copyloc; _ } ->
        Pretty.print ppf "%a:@;@[<h>copied@ at@ this@ location@]"
          pp_file_loc (to_raw_loc @@ as_lexloc copyloc)
    | `Rpl replloc ->
        Pretty.print ppf "%a:@;@[<h>subject@ to@ this@ replacement@]"
          pp_file_loc (to_raw_loc @@ as_lexloc replloc)
  in
  let pp_transform_operations ~partial =
    Pretty.list ~fopen:"@[<2>" ~fsep:"@]@\n@[" ~fclose:"@]@\n" ~fempty:""
      (pp_transform_operation ~partial)
  in
  let toplevel_transform_stack loc =
    let rec aux acc = function
      | Raw _ | Cat _ as loc -> List.rev acc, loc
      | Cpy { copied; copyloc } -> aux (`Cpy copyloc :: acc) copied
      | Rpl { old; replloc; _ } -> aux (`Rpl replloc :: acc) old
    in
    aux [] loc
  and partial_transform_operations loc =
    scan ~kind:`BottomUp loc []
      ~cpy: begin fun copyloc acc ->
        (* TODO: use physical equality instead? *)
        if List.mem (`Cpy copyloc) acc
        then acc
        else List.cons (`Cpy copyloc) acc
      end
      ~rpl: begin fun replloc acc ->
        if List.mem (`Rpl replloc) acc
        then acc
        else List.cons (`Rpl replloc) acc
      end
  in
  fun ppf loc ->
    let toplevel_transforms, loc = toplevel_transform_stack loc in
    let lexloc = as_lexloc loc in
    pp_raw_loc ppf (to_raw_loc lexloc);
    pp_transform_operations ~partial:false ppf toplevel_transforms;
    pp_transform_operations ~partial:true ppf (partial_transform_operations loc)

let pp_file_loc ppf loc =
  pp_file_loc ppf (to_raw_loc @@ as_lexloc loc)

(** {2 Constructors} *)

(** [raw ~in_area_a lexloc] builds a raw source location from a pair of left-
    and right- lexing positions from the same file, optionally setting an
    [in_area_a] flag (that defaults to [false]) to indicate whether the location
    is the first on its line, and starts in Area A of the source format. *)
let raw ?(in_area_a = false) ((s, e): lexloc) : srcloc =
  assert Lexing.(s.pos_cnum <= e.pos_cnum);               (* ensure proper use *)
  let loc = Raw (s, e, in_area_a) in
  if Lexing.(s.pos_fname <> e.pos_fname) then
    Pretty.error
      "%a@\n>> Internal warning in `%s.raw`: file names mismatch (`%s` != `%s`)\
      " pp_srcloc loc __MODULE__ s.pos_fname e.pos_fname;
  loc

let copy ~filename ~copyloc copied : srcloc =
  Cpy { copied; copyloc = { filename; copyloc } }

let replacement ~old ~new_ ~in_area_a ~replloc : srcloc =
  Rpl { old; new_; in_area_a; replloc }

let dummy =
  raw Lexing.(dummy_pos, dummy_pos)

(** {2 Composition & truncation} *)

(** [may_join_as_single_raw a b] checks whether a lexloc {i l{_ a}} with a a
    left-hand lexing position [a] and a lexloc {i l{_ b}} with a right-hand
    position [b], may be joined to form a single raw source location
    (internal). *)
let may_join_as_single_raw (a: Lexing.position) (b: Lexing.position) =
  a.pos_fname = b.pos_fname &&
  a.pos_lnum  == b.pos_lnum &&           (* ensure we are stay on a single line *)
  a.pos_cnum  >= b.pos_cnum - 1

(** [concat l1 l2] concatenates two adjacent source locations [l1] and [l2]. *)
let rec concat: srcloc -> srcloc -> srcloc = fun l1 l2 -> match l1, l2 with
  | Raw (s1, e1, in_area_a),
    Raw (s2, e2, _)
    when may_join_as_single_raw e1 s2 ->
      Raw (s1, e2, in_area_a)

  | Cat { left; right = Raw (s1, e1, in_area_a) },
    Raw (s2, e2, _)
    when may_join_as_single_raw e1 s2 ->
      Cat { left; right = Raw (s1, e2, in_area_a) }

  | Cpy { copied = l1; copyloc = c1 },
    Cpy { copied = l2; copyloc = c2 }
    when same_copyloc c1 c2 ->
      Cpy { copied = concat l1 l2; copyloc = c1 }

  | (Cat { left; right = Cpy { copied = l1; copyloc = c1 } }),
    (Cpy { copied = l2; copyloc = c2 })
    when same_copyloc c1 c2 ->
      Cat { left; right = Cpy { copied = concat l1 l2; copyloc = c1 } }

  | Rpl { new_ = s1; old = l1; replloc; in_area_a },              (* unlikely *)
    Rpl { new_ = s2; old = l2; replloc = replloc'; _ }
    when l1 == l2 && replloc == replloc' ->            (* note: physical equality *)
      Rpl { new_ = concat s1 s2; old = l1; replloc; in_area_a }

  | (Cat { left; right = Rpl { new_ = s1; old = l1; replloc; in_area_a }}),
    (Rpl { new_ = s2; old = l2; replloc = replloc'; _ })
    when l1 == l2 && replloc == replloc' ->            (* note: physical equality *)
      Cat { left; right = Rpl { new_ = concat s1 s2; old = l1;
                                replloc; in_area_a }}

  | (Raw _ | Cpy _ | Rpl _ | Cat _ as left),
    (Cpy _ | Rpl _ | Raw _ as right) ->
      Cat { left; right }

  | (Raw _ | Cpy _ | Rpl _ | Cat _ as l1),
    (Cat { left = l2; right }) ->
      Cat { left = concat l1 l2; right }

let concat_srclocs: srcloc list -> srcloc option = fun l ->
  List.fold_left begin fun acc loc -> match acc with
    | None -> Some loc
    | Some acc -> Some (concat acc loc)
  end None l

(** Direction for {!take} and {!trunc} below (internal) *)
type direction = Prefix | Suffix

let show_direction = function
  | Prefix -> "prefix"
  | Suffix -> "suffix"

(** [take direction length l] computes a source location for the prefix or
    suffix of length [length] of [l].  Shows some warnings on [stderr] in case
    [length] exceeds the length of [l].  *)
let take direction length loc =
  (* Assumes raw lexlocs do not span over several lines *)
  let open Lexing in
  let rec take: type k. _ -> k slt -> _ = fun length -> function
    | Raw (s, e, a) as loc ->
        let len = e.pos_cnum - s.pos_cnum in
        if len > length
        then match direction with
          | Prefix ->
              Raw (s, { e with pos_cnum = s.pos_cnum + length }, a), 0
          | Suffix ->
              Raw ({ s with pos_cnum = e.pos_cnum - length }, e, false), 0
        else loc, length - len
    | Cpy ({ copied; _ } as cpy) ->
        let copied, rem = take length copied in
        Cpy { cpy with copied }, rem
    | Rpl ({ new_; _ } as rpl) ->
        let new_, rem = take length new_ in
        Rpl { rpl with new_ }, rem
    | Cat { left; right } -> match direction with
      | Prefix ->
          let left, rem = take length left in
          if rem <= 0 then loc, rem else
            let right, rem = take rem right in
            concat left right, rem
      | Suffix ->
          let right, rem = take length right in
          if rem <= 0 then loc, rem else
            let left, rem = take rem left in
            concat left right, rem
  in
  let loc', rem = take length loc in
  if rem < 0 then
    Pretty.error
      "%a@\n>> Internal warning in `%s.take`: requested %s (%d) is longer than \
       source location (by %d)@.\
      " pp_srcloc loc __MODULE__ (show_direction direction) length (- rem);
  loc'

(** [trunc direction length l] truncates a prefix or suffix of length [length]
    from a source location [l].  Shows some warnings on [stderr] in case
    [length] exceeds the length of [l].  *)
let trunc direction length loc =
  (* Assumes raw lexlocs do not span over several lines *)
  let open Lexing in
  let rec cut: type k. _ -> k slt -> _ = fun length -> function
    | (Raw _ | Cpy _ | Rpl _ | Cat _) as loc
      when length == 0 ->
        Some loc, 0
    | Raw (s, e, a) ->
        let len = e.pos_cnum - s.pos_cnum in
        if len > length
        then match direction with
          | Prefix ->
              Some (Raw ({ s with pos_cnum = s.pos_cnum + length }, e, false)), 0
          | Suffix ->
              Some (Raw (s, { e with pos_cnum = e.pos_cnum - length }, a)), 0
        else None, length - len
    | Cpy ({ copied; _ } as cpy) ->
        begin match cut length copied with
          | Some copied, rem -> Some (Cpy { cpy with copied }), rem
          | res -> res
        end
    | Rpl ({ new_; _ } as rpl) ->
        begin match cut length new_ with
          | Some new_, rem -> Some (Rpl { rpl with new_ }), rem
          | res -> res
        end
    | Cat { left; right } -> match direction with
      | Prefix ->
          begin match cut length left with
            | Some left, rem -> Some (Cat { left; right }), rem
            | None, rem -> cut rem right
          end
      | Suffix ->
          begin match cut length right with
            | Some right, rem -> Some (concat left right), rem
            | None, rem -> cut rem left
          end
  in
  match cut length loc with
  | Some loc', rem when rem == 0 ->
      loc'
  | loc', rem ->
      if rem < 0 then
        Pretty.error
          "%a@\n>> Internal warning in `%s.trunc`: taken out %s (%d) is longer \
           than source location (by %d)@.\
          " pp_srcloc loc __MODULE__ (show_direction direction) length (- rem);
      Option.value loc' ~default:loc

(** [prefix prefix_length l] computes a source location for the prefix of length
    [prefix_length] of [l].  Shows some warnings on [stderr] in case
    [prefix_length] exceeds the length of [l].  *)
let prefix prefix_length loc = take Prefix prefix_length loc

(** [suffix suffix_length l] computes a source location for the suffix of length
    [suffix_length] of [l].  Shows some warnings on [stderr] in case
    [suffix_length] exceeds the length of [l].  *)
let suffix suffix_length loc = take Suffix suffix_length loc

(** [trunc_prefix prefix_length l] truncates a prefix of length [prefix_length]
    from a source location [l].  Shows some warnings on [stderr] in case
    [prefix_length] exceeds the length of [l].  *)
let trunc_prefix prefix_length loc = trunc Prefix prefix_length loc
let trunc_suffix suffix_length loc = trunc Suffix suffix_length loc

let sub loc ~pos ~len =
  let loc = if pos > 0 then trunc_prefix pos loc else loc in
  prefix len loc

(** {2 Manipulating localized values} *)

let pp pe ppf e = pe ppf e.payload              (* ignore source localization *)
let pp_with_loc = pp
let flagit payload loc = { payload; loc }
let payload: 'a with_loc -> 'a = fun e -> e.payload
let loc: 'a with_loc -> srcloc = fun e -> e.loc
let as_pair e = e.payload, e.loc
let locfrom: 'a -> 'b with_loc -> 'a with_loc = fun payload b ->
  flagit payload (loc b)
let locmap: ('a -> 'b) -> 'a with_loc -> 'b with_loc = fun f a ->
  flagit (f (payload a)) (loc a)

module INFIX : sig
  (* Meaning of letters:
     * '~' means projection
     * '&' means payload
     * '@' means location
     * '?' means map-option
  *)
  val ( &@ ): 'a -> srcloc -> 'a with_loc
  val ( &@<- ): 'a -> 'b with_loc -> 'a with_loc

  val ( ~& ): 'a with_loc -> 'a
  val ( ~@ ): 'a with_loc -> srcloc
  val ( ~&? ): 'a with_loc option -> 'a option
  val ( ~@? ): 'a with_loc option -> srcloc option
  val ( ~&@ ): 'a with_loc -> 'a * srcloc
end = struct
  (* Fabrice: the use of non-standard infix operators is a nightmare for
     external reviewers and newcomers. *)

  let ( &@ ) = flagit
  let ( &@<- ) = locfrom
  let ( ~& ) = payload
  let ( ~&? ) e = Option.map (~&) e
  let ( ~@ ) = loc
  let ( ~@? ) e = Option.map (~@) e
  let ( ~&@ ) = as_pair

end
open INFIX

let lift_option: 'a option with_loc -> 'a with_loc option = fun a ->
  Option.map (fun x -> x &@<- a) ~&a

let lift_result: ('a, 'e) result with_loc -> ('a with_loc, 'e with_loc) result =
  fun rs ->
  Result.(fold ~&rs
            ~ok:(fun a -> Ok (a &@<- rs))
            ~error:(fun e -> Error (e &@<- rs)))

let concat_locs: _ with_loc list -> srcloc option = fun l ->
  List.fold_left begin fun acc { loc; _ } -> match acc with
    | None -> Some loc
    | Some acc -> Some (concat acc loc)
  end None l

let concat_strings_with_loc v w = (~&v ^ ~&w) &@ (concat ~@v ~@w)

let copy_from ~filename ~copyloc { payload; loc } =
  { payload; loc = copy ~filename ~copyloc loc }

(* --- *)

module TESTING = struct
  let register_file_contents = register_file_contents
end

(* --- *)

let no_copy: copylocs = []
let new_copy ~copyloc filename = List.cons { filename; copyloc }
let mem_copy f  = List.exists (fun { filename; _ } -> filename = f)
