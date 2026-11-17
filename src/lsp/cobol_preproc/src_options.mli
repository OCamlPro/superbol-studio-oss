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


(** Options that drive how source text is read (before preprocessing). *)

type t =
  {
    tab_stops: int list;
    (** Tab stops used to compute visual columns (see
        {!Src_format.next_tab_stop}). *)
    mf_comments: bool;
    (** Recognize lines with an asterisk in the first column as comments in
        FIXED, COBOL85, and VARIABLE source formats (cf GnuCOBOL's
        [-fmfcomment]). *)
  }

(** Default tab stops, and no MF-style comment lines. *)
val default: t
