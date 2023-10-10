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

open Text.TYPES

(** {1 Preprocessor state}

    This state is used to track some preprocessing-related divisions, like the
    `CONTROL DIVISION` in the GCOS dialect. *)

type state
type t = state

type preproc_phrase =
  | Copy of phrase
  | Replace of phrase
  | Header of tracked_header * phrase
and phrase =
  {
    prefix: text;
    phrase: text;
    suffix: text;
  }
and tracked_header =
  | ControlDivision
  | SubstitutionSection
  | IdentificationDivision

val initial: state
val find_preproc_phrase
  : ?prefix:[ `Rev | `Same ]
  -> state
  -> text
  -> (preproc_phrase * state,
      [> `MissingPeriod | `MissingText | `NoneFound ]) result
