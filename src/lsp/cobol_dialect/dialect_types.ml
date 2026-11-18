(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

type id =
  | Default
  | GnuCOBOL
  | COBOL85
  | COBOL2002
  | COBOL2014
  | ACU        of strictness
  | BS2000     of strictness
  | GCOS       of strictness
  | IBM        of strictness
  | MicroFocus of strictness
  | MVS        of strictness
  | Realia     of strictness
  | RM         of strictness
  | XOpen
  | From_file  of string
and strictness = { strict: bool }

(** Description of a dialect configuration. *)
type origin =
  {
    id: id;
    display_name: string;                 (** [name] in configuration files *)
    file: string option;
    ancestry: id list;
    (** dialects of included configurations, then [id]; each after those it
        includes *)
  }

(** Support levels of dialect features; those that accept a feature may carry
    a feature-specific value.  [unit support] are the levels found in
    configuration files. *)
type 'a support =
  | Ok of 'a
  | Warning of 'a
  | Archaic of 'a
  | Obsolete of 'a
  | Skip
  | Ignore
  | Error
  | Unconformable

type feature_info =
  {
    name: string;
    short: Pretty.simple option;
    doc: Pretty.simple;
  }

type 'a feature =
  {
    info: feature_info;
    of_level: unit support -> 'a support;
  }

type 'a binding =
  {
    feature: feature_info;
    level: 'a support;
    origin: origin;
  }

type reason =
  [ `Used
  | `Ignored
  | `Archaic of origin
  | `Obsolete of origin
  | `Unconformable of origin ]

type usage =
  {
    loc: Cobol_common.srcloc;
    feature: feature_info;
    reason: reason;
  }

type 'a verification = ('a * usage option, usage option) result
