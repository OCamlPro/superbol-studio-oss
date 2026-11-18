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
