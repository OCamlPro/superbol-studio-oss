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
open Cobol_common.Srcloc.INFIX

(* --- *)

module TYPES = Text_types
include TYPES

let prefix_of_literal_kind: literal_kind -> string = function
  | Basic -> ""
  | Bool -> "B"
  | BoolX ->  "BX"
  | Hex ->  "X"
  | NullTerm ->  "Z"
  | National ->  "N"
  | NationalX -> "NX"

let char_of_quotation: quotation -> char = function
  | Apostrophe -> '\''
  | Quote -> '"'

module FMT = struct

  let pp_literal_kind ppf kind =
    Pretty.string ppf (prefix_of_literal_kind kind)

  let pp_quote ppf quote =
    Pretty.char ppf (char_of_quotation quote)

  let pp_alphanum ppf { knd; qte; str } =
    Pretty.print ppf "%a%a%s%a"
      pp_literal_kind knd pp_quote qte str pp_quote qte

  let pp_pseudoword ppf w = match ~&w with
    | PseudoWord strs ->
        Pretty.list ~fopen:"" ~fclose:"" ~fsep:"" ~fempty:""
          begin fun ppf { payload = PwText s | PwDelim (s, _); _ } ->
            Pretty.string ppf s
          end ppf strs
    | PseudoAlphanum a ->
        pp_alphanum ppf a

  let pp_pseudotext =
    Pretty.list ~fopen:"==" ~fsep:" "~fclose:"=="  pp_pseudoword

  let pp_word ppf word = match word with
    | TextWord str
    | CDirWord str ->
        Pretty.string ppf str
    | Alphanum a ->
        pp_alphanum ppf a
    | AlphanumPrefix { knd; qte; str } ->
        Pretty.print ppf "%a%a%s" pp_literal_kind knd pp_quote qte str
    | Pseudo pl ->
        pp_pseudotext ppf pl
    | Eof ->
        Pretty.string ppf "EOF"

  let pp_text =
    Pretty.list ~fopen:"@[" ~fsep:"@;" ~fclose:"@]" (fun ppf w -> pp_word ppf ~&w)

  let pp_text' ?fsep  =
    Pretty.list ~fopen:"@[" ?fsep ~fclose:"@]" begin fun ppf word ->
      Pretty.print ppf "%a@@%a"
        pp_word ~&word
        Cobol_common.Srcloc.pp_srcloc_struct ~@word
    end

end
include FMT

(* Various predicates on source-flagged character strings *)

let textwordp t = match ~&t with TextWord _ -> true | _ -> false
let textword_eqp ~eq:w t = match ~&t with
  | TextWord t -> String.uppercase_ascii t = String.uppercase_ascii w
  | _ -> false
let cdirp t = match ~&t with CDirWord _ -> true | _ -> false
let cdir_eqp ~eq:w t = match ~&t with
  | CDirWord t -> String.uppercase_ascii t = String.uppercase_ascii w
  | _ -> false

(* Manipulating pseudo-words and text *)

let pseudosep_regexp =
  Str.regexp "[:()]"

let split_pseudo_string w =
  let split_loc loc str =
    let len = String.length str in
    Cobol_common.Srcloc.prefix len loc,
    Cobol_common.Srcloc.trunc_prefix len loc
  in
  let pseudoword_items, _ =
    Str.full_split pseudosep_regexp ~&w |>
    List.fold_left begin fun (acc, wloc) w -> match w with
      | Str.Text t ->
          let tloc, wloc = split_loc wloc t in
          (PwText t &@ tloc) :: acc, wloc
      | Str.Delim d ->
          let dloc, wloc = split_loc wloc d in
          (PwDelim (d, Str.regexp_case_fold (Str.quote d)) &@ dloc) :: acc, wloc
    end ([], ~@w)
  in
  List.rev pseudoword_items

let join_pseudo_string ~string =
  List.fold_left begin fun pw { payload = PwText w | PwDelim (w, _); loc } ->
    match pw with
    | None -> Some (string (w &@ loc))
    | Some pw -> Some (Cobol_common.Srcloc.concat_strings_with_loc pw (w &@ loc))
  end None

let pseudo_string w =
  PseudoWord (split_pseudo_string w) &@<- w
let pseudo_alphanum a =
  PseudoAlphanum { str = fst ~&a; qte = snd ~&a; knd = Basic } &@<- a
let pseudo_integer i =
  PseudoWord [PwText (Int64.to_string ~&i) &@<- i] &@<- i
let alphanum_as_pseudo a =
  PseudoWord [PwText (fst ~&a) &@<- a] &@<- a

let pseudoword_of_string: _ -> pseudoword = pseudo_string
let pseudoword_of_alphanum: _ -> pseudoword = pseudo_alphanum
let pseudoword_of_integer: _ -> pseudoword = pseudo_integer
let alphanum_as_pseudoword a = alphanum_as_pseudo a

let pseudotext_ f x = [f x] &@ ~@x
let pseudotext_of_string = pseudotext_ pseudo_string
let pseudotext_of_alphanum = pseudotext_ pseudo_alphanum
let pseudotext_of_integer = pseudotext_ pseudo_integer
let alphanum_as_pseudotext a = pseudotext_ alphanum_as_pseudo a

(** Strips any end-of-input/file item from the beginning of the given text. *)
let rec strip_eof = function
  | { payload = Eof; _ } :: text -> strip_eof text
  | text -> text

let map_pseudowords ~f : pseudotext -> pseudotext =
  let map_pseudoword_items: (pseudoword_item with_loc list as 'a) -> 'a =
    EzList.tail_map begin function
      | { payload = PwText w; loc } -> PwText (f w) &@ loc
      | x -> x
    end
  in
  EzList.tail_map begin function
    | { payload = PseudoWord w; loc } ->
        PseudoWord (map_pseudoword_items w) &@ loc
    | x -> x
  end
