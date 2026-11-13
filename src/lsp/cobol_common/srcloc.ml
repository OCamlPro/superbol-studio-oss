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

open Platform.TYPES

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
    | Raw:
        {
          fname: string;
          lnum: int;          (* line of the start position *)
          cnum: int;          (* offset of the start position *)
          col: int;           (* column of the start position *)
          len: int;           (* offset of the end position - [cnum] *)
          lines_n_a: int;     (* (line of the end position - [lnum]) * 2 +
                                 area-a flag (OCaml int trick ;)) *)
          ecol: int;          (* column of the end position *)
        } -> [>raw_] slt
    | Cpy:                                                            (* COPY *)
        {
          copied: any_ slt;
          copyloc: copyloc;
        } -> [>cpy_] slt
    | Rpl:                                                   (* REPLACE(MENT) *)
        {
          old: any_ slt;  (* should only keep ref. of starting pos if needed for
                             area-a detection... *)
          new_: srcloc;     (* location of replacement spec (e.g, pseudotext) *)
          in_area_a: bool;
          replloc: srcloc;
        } -> [>rpl_] slt
    | Cat:                                                   (* concatenation *)
        {
          left: left_ slt;
          right: right_ slt;
        } -> [>cat_] slt
    (* invariant: Cat {left = Cpy _ ; right = Cpy _}: the two copies are not
       from the same file. *)

  (* Raw lexical range *)
  and lexloc = Lexing.position * Lexing.position

  (* Compact raw locations *)
  and raw = raw_ slt

  (* Representation of a file copy operation *)
  and copyloc = { filename: string; copyloc: srcloc (* option *) }

  (* A (con)cat(enation) allows any sort of source location on its left... *)
  and left_ = any_

  (* ... but forbids other cats directly on its right. *)
  and right_ = [raw_|cpy_|rpl_]

  (** Sets of copied libraries *)
  type copylocs = copyloc list

  (** Values attached with a source location. *)
  type 'a with_loc = { payload: 'a; loc: srcloc [@compare fun _ _ -> 0] }
  [@@deriving ord]

  type src =
    | Source_location of (srcloc [@compare fun _ _ -> 0])
    | Process_parameter
    | Process_environment
  [@@deriving ord]

  type 'a with_src =
    {
      src_payload: 'a;
      src: src [@compare fun _ _ -> 0];
    }
  [@@deriving ord]

  let pp pe ppf e = pe ppf e.payload            (* ignore source localization *)
  let pp_with_loc = pp
  let show_with_loc pe l = Pretty.to_string "%a" (pp pe) l
  let pp_src pe ppf e = pe ppf e.src_payload                          (* same *)
  let pp_with_src = pp_src
  let show_with_src pe l = Pretty.to_string "%a" (pp_src pe) l
end
include TYPES

(* Raw locations *)

(* [mk_raw lexloc ~in_area_a] builds a raw location from [lexloc].  Both
   positions are expected to belong to the same file: filename of start position
   is kept. *)
let mk_raw ((s, e): lexloc) ~in_area_a : _ slt =
  Raw { fname = s.pos_fname;
        lnum = s.pos_lnum;
        cnum = s.pos_cnum;
        col = s.pos_cnum - s.pos_bol;
        len = e.pos_cnum - s.pos_cnum;
        lines_n_a = (e.pos_lnum - s.pos_lnum) * 2 + Bool.to_int in_area_a;
        ecol = e.pos_cnum - e.pos_bol }

let lexpos ~fname ~lnum ~cnum ~col =
  Lexing.{ pos_fname = fname; pos_lnum = lnum; pos_cnum = cnum;
           pos_bol = cnum - col }

let unraw: raw -> Lexing.position * Lexing.position * bool = function
  | Raw { fname; lnum; cnum; col; len; lines_n_a; ecol } ->
      lexpos ~fname ~lnum ~cnum ~col,
      lexpos ~fname ~lnum:(lnum + lines_n_a asr 1) ~cnum:(cnum + len) ~col:ecol,
      lines_n_a land 1 = 1

let raw_start: raw -> Lexing.position = function
  | Raw { fname; lnum; cnum; col; _ } -> lexpos ~fname ~lnum ~cnum ~col

let raw_end: raw -> Lexing.position = function
  | Raw { fname; lnum; cnum; len; lines_n_a; ecol; _ } ->
      lexpos ~fname ~lnum:(lnum + lines_n_a asr 1) ~cnum:(cnum + len) ~col:ecol

let raw_lexloc loc =
  let s, e, _ = unraw loc in
  s, e

(* For debugging: *)

let pp_lexpos ppf Lexing.{ pos_fname; pos_lnum; pos_cnum; pos_bol } =
  Pretty.print ppf "%s:%d-%d" pos_fname pos_lnum (pos_cnum - pos_bol)

let pp_srcloc_struct: srcloc Pretty.printer =
  let pp_lexpos' ppf Lexing.{ pos_lnum; pos_cnum; pos_bol; _ } =
    Pretty.print ppf "%d-%d" pos_lnum (pos_cnum - pos_bol)
  in
  let rec pp: type t. t slt Pretty.printer = fun ppf -> function
    | Raw _ as loc ->
        let s, e, _ = unraw loc in
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
  | Raw _ as loc -> raw_start loc
  | Cpy { copied; _ } -> start_pos copied
  | Rpl { old; _ } -> start_pos old
  | Cat { left; _ } -> start_pos left

(** [shallow_start_pos loc] retrieves the starting position of [loc] if it was
    not subject to any copy, and without traversal of replacements (i.e. returns
    the start position of replaced text, not the position of its
    replacement). Returns [None] otherwise. *)
let rec shallow_start_pos = function
  | Raw _ as loc -> Some (raw_start loc)
  | Cpy _ -> None
  | Rpl { old; _ } -> shallow_start_pos old
  | Cat { left; _ } -> shallow_start_pos left

(** [shallow_multiline_lexloc_in ~filename loc] retrieves a lexical location in
    [filename] from [loc], iff [loc] directly originates from [filename] and was
    not subject to any replacement or copy.  Returns [None] otherwise. *)
let shallow_multiline_lexloc_in ~filename loc =
  let rec aux: type t. t slt -> lexloc option = function
    | Raw { fname; _ } as loc when fname = filename -> Some (raw_lexloc loc)
    | Raw _ | Cpy _ | Rpl _ -> None
    | Cat { left; right } ->
        (* Note: [Cat] chains may be very deep, so we iterate over their left
           spine.  Fast path: when the left-most leaf belongs to [filename],
           the end is that of the right-most leaf that also does. *)
        match leftmost left with
        | Some (s, e) -> Some (s, last_end ~default:e right left)
        | None -> cat [right] left
  and leftmost: left_ slt -> _ = function
    | Cat { left; _ } -> leftmost left
    | l -> aux l
  and last_end ~default right left =
    match aux right, left with
    | Some (_, e), _ -> e
    | None, Cat { left; right } -> last_end ~default right left
    | None, _ -> default
  and cat: right_ slt list -> left_ slt -> lexloc option = fun rights ->
    function
    | Cat { left; right } -> cat (right :: rights) left
    | l -> List.fold_left (fun acc r -> join acc (aux r)) (aux l) rights
  and join a b = match a, b with
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
  | Raw { fname; _ } as loc when fname = filename -> Some (raw_lexloc loc)
  | Raw _ | Cpy _ | Rpl _ | Cat _ -> None

let start_pos_in ~filename ~traverse_replaces =
  let rec aux: type t. t slt -> Lexing.position option = function
    | Raw { fname; _ } as loc when fname = filename -> Some (raw_start loc)
    | Raw _ -> None
    | Cat { left; right } ->
        (* Note: [Cat] chains may be very deep, so we iterate over their left
           spine.  Fast path: the left-most leaf belongs to [filename]. *)
        (match leftmost left with None -> cat [right] left | res -> res)
    | Cpy { copied; copyloc = { copyloc; _ } } -> or_else copied copyloc
    | Rpl { old; new_; _ } ->
        aux (if traverse_replaces then new_ else old)
  and or_else: type t u. t slt -> u slt -> _ = fun a b ->
    match aux a with None -> aux b | res -> res
  and leftmost: left_ slt -> _ = function
    | Cat { left; _ } -> leftmost left
    | l -> aux l
  and cat: right_ slt list -> left_ slt -> _ = fun rights -> function
    | Cat { left; right } -> cat (right :: rights) left
    | l -> match aux l with None -> first_in rights | res -> res
  and first_in = function
    | [] -> None
    | r :: rights -> match aux r with None -> first_in rights | res -> res
  in
  aux

let end_pos_in ~filename ~traverse_replaces =
  let rec aux: type t. t slt -> Lexing.position option = function
    | Raw { fname; _ } as loc when fname = filename -> Some (raw_end loc)
    | Raw _ -> None
    | Cat { left; right } ->                   (* note: tail call on [left] *)
        (match aux right with None -> aux left | res -> res)
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
    | Raw _ as loc ->
        raw_lexloc loc
    | Cpy { copied; copyloc = { copyloc; _ } } ->
        aux (if traverse_copies then copied else copyloc)
    | Rpl { new_; old; _ } ->
        aux (if traverse_replaces then new_ else old)
    | Cat { left; right } ->
        match favor_direction with
        | `Left ->
            (* Note: [Cat] chains may be very deep, so we iterate over their
               left spine: first to find the start in the left-most leaf, then
               to find the right-most end in the same file. *)
            let (Lexing.{ pos_fname = filename; _ } as s), e = leftmost left in
            s, last_end ~filename ~default:e right left
        | `Right ->
            let s, (Lexing.{ pos_fname = filename; _ } as e) = aux right in
            Option.value (start_pos_in ~filename left) ~default:s, e
  and leftmost: left_ slt -> lexloc = function
    | Cat { left; _ } -> leftmost left
    | l -> aux l
  and last_end ~filename ~default right left =
    match end_pos_in ~filename right, left with
    | Some e, _ -> e
    | None, Cat { left; right } -> last_end ~filename ~default right left
    | None, _ -> default
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
  let rec aux: type t. t slt -> (lexloc list as 'a) -> 'a = fun loc acc ->
    match loc with
    | Raw { fname; _ } as loc when fname = filename -> raw_lexloc loc :: acc
    | Raw _ | Cpy _ | Rpl _ -> acc
    | Cat { left; right } -> aux left (aux right acc)   (* tail call on [left] *)
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
  | Raw _ as loc -> Some (raw_lexloc loc)
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
  | Raw { lines_n_a; _ } -> lines_n_a land 1 = 1
  | Cpy { copied; _ } -> in_area_a copied
  | Rpl { in_area_a; _ } -> in_area_a
  | Cat { left; _ } -> in_area_a left

let is_pointwise: srcloc -> bool = function
  | Raw { len; _ } -> len == 0
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
    | (Cat { left; right }, _) -> cat [right] left
  and cat: right_ slt list -> left_ slt -> 'a -> 'a = fun rights ->
    function            (* iterate over the left spine to avoid deep recursions *)
    | Cat { left; right } -> cat (right :: rights) left
    | l -> fun acc -> List.fold_left (fun acc r -> aux r acc) (aux l acc) rights
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

module TABLE =
  Ephemeron.K1.Make (struct
    include String                                     (* structural equality *)
    let hash = Hashtbl.hash
  end)

let given_filename, register_given_filename =
  (* [given_filename] is what we expect to be kept at its place in memory,
     whereas [filename] may be re-computed.  We use two tables to keep the
     latter alive for as long as the former does. *)
  let filename_of_given_filename = TABLE.create 3 in (* Note phys eq may be enough here  *)
  let given_filename_of_filename = TABLE.create 3 in
  begin fun filename ->
    match TABLE.find_opt given_filename_of_filename filename with
    | Some given_filename ->
        given_filename
    | None ->
        filename
  end,
  begin fun ~filename ~given_filename ->
    TABLE.replace filename_of_given_filename given_filename filename;
    TABLE.replace given_filename_of_filename filename given_filename;
  end

let retrieve_file_lines, register_file_contents =
  let file_cache = lazy (TABLE.create 3) in
  begin fun ~platform file ->
    let file_cache = Lazy.force file_cache in
    match TABLE.find_opt file_cache file with
    | Some lines ->
        lines
    | None ->
        let contents = platform.read_text_file file in
        let lines = String.split_on_char '\n' contents in
        let lines = Array.of_list lines in
        let lines =
          (* Retain the behavior of `EzFile.read_lines` that do not automatically
             append an empty line at the end *)
          if lines.(Array.length lines - 1) = ""
          then Array.sub lines 0 (Array.length lines - 1)
          else lines
        in
        TABLE.replace file_cache file lines;
        lines
  end,
  begin fun ~filename contents ->
    let file_cache = Lazy.force file_cache in
    let lines = Array.of_list @@ String.split_on_char '\n' contents in
    TABLE.replace file_cache filename lines
  end

type raw_loc = string * (int * int) * (int * int)

let pp_file_loc ppf ((file, pos1, pos2): raw_loc) =
  Pretty.print ppf "%s:%a" (given_filename file) Fmt.text_loc (pos1, pos2)

(** Note this should always end with a newline character *)
let pp_raw_loc: ?platform:platform -> raw_loc Pretty.printer =
  let b = lazy (Buffer.create 1000) in
  let find_source ~platform (file, pos1, pos2) =
    let line1 = fst pos1 in
    let line2 = fst pos2 in
    let col1 = snd pos1 in
    let col2 = snd pos2 in
    let col2, pad2 =
      if line1 == line2 && col1 == col2 then succ col2, 1 else col2, 0 in
    let lines = retrieve_file_lines ~platform file in
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
  fun ?platform ppf raw_loc ->
    let text =
      match platform with
      | None -> ""
      | Some platform ->
          try find_source ~platform raw_loc
          with Sys_error _ | Failure _ | Unix.Unix_error _ -> ""
    in
    Pretty.print ppf "%a:@\n@[@<0>%s@]" pp_file_loc raw_loc text

let pp_raw_loc_without_caret =
  pp_raw_loc ?platform:None

let same_copyloc { filename = f1; _ } { filename = f2; _ } =
  f1 = f2                                                             (* berk *)

let to_raw_loc
    Lexing.({ pos_lnum = l1; pos_bol = b1; pos_cnum = c1; pos_fname; _ },
            { pos_lnum = l2; pos_bol = b2; pos_cnum = c2; _ }) =
  pos_fname, (l1, c1 - b1), (l2, c2 - b2)

let pp_srcloc_with_optional_caret: ?platform:platform -> srcloc Pretty.printer =
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
  fun ?platform ppf loc ->
    let toplevel_transforms, loc = toplevel_transform_stack loc in
    let lexloc = as_lexloc loc in
    pp_raw_loc ?platform ppf (to_raw_loc lexloc);
    pp_transform_operations ~partial:false ppf toplevel_transforms;
    pp_transform_operations ~partial:true ppf (partial_transform_operations loc)

let pp_srcloc_without_caret = pp_srcloc_with_optional_caret ?platform:None
let pp_srcloc ~platform = pp_srcloc_with_optional_caret ~platform

let pp_file_loc ppf loc =
  pp_file_loc ppf (to_raw_loc @@ as_lexloc loc)

let start_filename loc =
  given_filename (start_pos loc).pos_fname

let pp_src ppf = function
  | Source_location l ->
      pp_file_loc ppf l
  | Process_parameter ->
      Pretty.print ppf "process@ parameters"
  | Process_environment ->
      Pretty.print ppf "process@ environment"

(** {2 Constructors} *)

(** [raw ~in_area_a lexloc] builds a raw source location from a pair of left-
    and right- lexing positions from the same file, optionally setting an
    [in_area_a] flag (that defaults to [false]) to indicate whether the location
    is the first on its line, and starts in Area A of the source format.  Only
    the filename of the start position is kept. *)
let raw ?(in_area_a = false) ((s, e): lexloc) : srcloc =
  assert Lexing.(s.pos_cnum <= e.pos_cnum);               (* ensure proper use *)
  let loc = mk_raw (s, e) ~in_area_a in
  if Lexing.(s.pos_fname <> e.pos_fname) then
    Pretty.error
      "%a@\n>> Internal warning in `%s.raw`: file names mismatch (`%s` != `%s`)\
      " pp_srcloc_without_caret loc __MODULE__ s.pos_fname e.pos_fname;
  loc

let copy ~filename ~copyloc copied : srcloc =
  Cpy { copied; copyloc = { filename; copyloc } }

let replacement ~old ~new_ ~in_area_a ~replloc : srcloc =
  Rpl { old; new_; in_area_a; replloc }

let dummy =
  raw Lexing.(dummy_pos, dummy_pos)

(** {2 Composition & truncation} *)

(** [may_join_as_single_raw l1 l2] checks whether two raw locations [l1] and
    [l2] may be joined to form a single raw source location. *)
let may_join_as_single_raw: raw -> raw -> bool = fun (Raw r1) (Raw r2) ->
  r1.fname = r2.fname &&
  r1.lnum + (r1.lines_n_a asr 1) == r2.lnum &&   (* ensure we are stay on a single
                                                  line *)
  r1.cnum + r1.len >= r2.cnum - 1

let join_raws: raw -> raw -> [>raw_] slt = fun (Raw r1) (Raw r2) ->
  let end_lnum = r2.lnum + (r2.lines_n_a asr 1) in
  Raw { r1 with
        len = r2.cnum + r2.len - r1.cnum;
        lines_n_a = (end_lnum - r1.lnum) * 2 + (r1.lines_n_a land 1);
        ecol = r2.ecol }

(** [concat l1 l2] concatenates two adjacent source locations [l1] and [l2]. *)
let rec concat: srcloc -> srcloc -> srcloc = fun l1 l2 -> match l1, l2 with
  | (Raw _ as l1), (Raw _ as l2)
    when may_join_as_single_raw l1 l2 ->
      join_raws l1 l2

  | Cat { left; right = Raw _ as l1 }, (Raw _ as l2)
    when may_join_as_single_raw l1 l2 ->
      Cat { left; right = join_raws l1 l2 }

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
    (Cat _ as l2) ->
      (* iterate over the left spine of [l2] to avoid deep recursions *)
      let rec spine (acc: right_ slt list) : srcloc -> srcloc * _ = function
        | Cat { left; right } -> spine (right :: acc) left
        | l -> l, acc
      in
      let first, rights = spine [] l2 in
      List.fold_left (fun left right -> Cat { left; right })
        (concat l1 first) rights

let concat_srclocs: srcloc list -> srcloc option = fun l ->
  List.fold_left begin fun acc loc -> match acc with
    | None -> Some loc
    | Some acc -> Some (concat acc loc)
  end None l

let append_srclocs: srcloc -> srcloc list -> srcloc = fun loc l ->
  List.fold_left concat loc l

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
  let rec take: type k. _ -> k slt -> _ = fun length -> function
    | Raw _ as loc ->
        take_in_raw length loc
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
  and take_in_raw length (Raw r as loc: raw) =
    if r.len > length
    then
      let delta = r.len - length in
      match direction with
      | Prefix ->                             (* the end moves back by [delta] *)
          Raw { r with len = length; ecol = r.ecol - delta }, 0
      | Suffix ->    (* the start moves by [delta], and is no longer in area A *)
          Raw { r with cnum = r.cnum + delta; col = r.col + delta;
                       len = length; lines_n_a = r.lines_n_a land (lnot 1) }, 0
    else loc, length - r.len
  in
  let loc', rem = take length loc in
  if rem < 0 then
    Pretty.error
      "%a@\n>> Internal warning in `%s.take`: requested %s (%d) is longer than \
       source location (by %d)@."
      pp_srcloc_without_caret loc __MODULE__ (show_direction direction) length
      (- rem);
  loc'

(** [trunc direction length l] truncates a prefix or suffix of length [length]
    from a source location [l].  Shows some warnings on [stderr] in case
    [length] exceeds the length of [l].  *)
let trunc direction length loc =
  (* Assumes raw lexlocs do not span over several lines *)
  let rec cut: type k. _ -> k slt -> _ = fun length -> function
    | (Raw _ | Cpy _ | Rpl _ | Cat _) as loc
      when length == 0 ->
        Some loc, 0
    | Raw _ as loc ->
        cut_in_raw length loc
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
  and cut_in_raw length (Raw r: raw) =
    if r.len > length
    then match direction with
      | Prefix ->   (* the start moves by [length], and is no longer in area A *)
          Some (Raw { r with cnum = r.cnum + length; col = r.col + length;
                             len = r.len - length;
                             lines_n_a = r.lines_n_a land (lnot 1) }), 0
      | Suffix ->                            (* the end moves back by [length] *)
          Some (Raw { r with len = r.len - length;
                             ecol = r.ecol - length }), 0
    else None, length - r.len
  in
  match cut length loc with
  | Some loc', rem when rem == 0 ->
      loc'
  | loc', rem ->
      if rem < 0 then
        Pretty.error
          "%a@\n>> Internal warning in `%s.trunc`: taken out %s (%d) is longer \
           than source location (by %d)@."
          pp_srcloc_without_caret loc __MODULE__ (show_direction direction)
          length (- rem);
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

let flagit payload loc = { payload; loc }
let payload: 'a with_loc -> 'a = fun e -> e.payload
let loc: 'a with_loc -> srcloc = fun e -> e.loc
let as_pair e = e.payload, e.loc
let locfrom: 'a -> 'b with_loc -> 'a with_loc = fun payload b ->
  flagit payload (loc b)
let map_payload: ('a -> 'b) -> 'a with_loc -> 'b with_loc = fun f a ->
  flagit (f (payload a)) (loc a)
let map_loc: (srcloc -> srcloc) -> 'a with_loc -> 'a with_loc = fun f a ->
  flagit (payload a) (f (loc a))

let with_src ~src src_payload =
  { src; src_payload }

let with_loc_as_src ~loc x =
  with_src ~src:(Source_location loc) x

let lift_loc_as_src x =
  with_src ~src:(Source_location x.loc) x.payload

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

module NAMING = struct
  let register_given_filename = register_given_filename
  let given_filename = given_filename
end

(* --- *)

let no_copy: copylocs = []
let new_copy ~copyloc filename = List.cons { filename; copyloc }
let mem_copy f  = List.exists (fun { filename; _ } -> filename = f)
