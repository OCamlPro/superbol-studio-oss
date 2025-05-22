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

type lexloc = Cobol_common.Srcloc.lexloc
type srcloc = Cobol_common.Srcloc.srcloc

type 'a with_loc = 'a Cobol_common.Srcloc.with_loc =
  {
    payload: 'a;
    loc: srcloc [@compare fun _ _ -> 0];
  }
[@@deriving ord]

let pp_with_loc = Cobol_common.Srcloc.pp_with_loc

(* A type for non-empty lists *)

module NEL = Cobol_common.Basics.NEL
type 'a nel = 'a NEL.t
[@@deriving ord]

let pp_nel ppe = NEL.pp ~fsep:"@ " ~fopen:"" ~fclose:"" ppe

type permutation_duo = AB | BA

and trio_head = A3 | B3 | C3

and permutation_trio = trio_head * permutation_duo

and quartet_head = A4 | B4 | C4 | D4

and permutation_quartet = quartet_head * permutation_trio
[@@deriving ord]

let prepend_as_trio: _ -> _ * _ * _ * permutation_trio = function
  | `A (a, (b, c, duo)) -> (a, b, c, (A3, duo))
  | `B (b, (a, c, duo)) -> (a, b, c, (B3, duo))
  | `C (c, (a, b, duo)) -> (a, b, c, (C3, duo))

let prepend_as_quartet: _ -> _ * _ * _ * _ * permutation_quartet = function
  | `A (a, (b, c, d, trio)) -> (a, b, c, d, (A4, trio))
  | `B (b, (a, c, d, trio)) -> (a, b, c, d, (B4, trio))
  | `C (c, (a, b, d, trio)) -> (a, b, c, d, (C4, trio))
  | `D (d, (a, b, c, trio)) -> (a, b, c, d, (D4, trio))

let fold_duo duo (a, b) (fa, fb) acc =
  match duo with
  | AB -> acc |> fa a |> fb b
  | BA -> acc |> fb b |> fa a

let fold_trio trio (a,b,c) (fa, fb, fc) acc =
  match trio with
  | A3, duo -> acc |> fa a |> fold_duo duo (b,c) (fb, fc)
  | B3, duo -> acc |> fb b |> fold_duo duo (a,c) (fa, fc)
  | C3, duo -> acc |> fc c |> fold_duo duo (a,b) (fa, fb)

let fold_quartet quartet (a,b,c,d) (fa, fb, fc, fd) acc =
  match quartet with
  | A4, trio -> acc |> fa a |> fold_trio trio (b,c,d) (fb, fc, fd)
  | B4, trio -> acc |> fb b |> fold_trio trio (a,c,d) (fa, fc, fd)
  | C4, trio -> acc |> fc c |> fold_trio trio (a,b,d) (fa, fb, fd)
  | D4, trio -> acc |> fd d |> fold_trio trio (a,b,c) (fa, fb, fc)

let unit_acc (f: 'a -> unit) = fun x () -> f x

let iter_duo duo tuple (fa, fb) =
  fold_duo duo tuple (unit_acc fa, unit_acc fb) ()

let iter_trio trio tuple (fa, fb, fc) =
  fold_trio trio tuple
    (unit_acc fa, unit_acc fb, unit_acc fc) ()

let iter_quartet quartet tuple (fa, fb, fc, fd) =
  fold_quartet quartet tuple
    (unit_acc fa, unit_acc fb, unit_acc fc, unit_acc fd) ()

