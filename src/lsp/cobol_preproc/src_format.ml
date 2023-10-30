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

(* Paging *)

type free = UnlimitedLines
type fixed = LimitedLines
type _ paging =
  | FreePaging: free paging
  | FixedWidth: fixed_paging_params -> fixed paging
and fixed_paging_params =
  {
    cut_at_col: int;
    alphanum_padding: char option;
  }

let fixed_paging    = { cut_at_col = 72; alphanum_padding = Some ' ' }
let variable_paging = { fixed_paging with cut_at_col = 250 }
let xcard_paging    = { fixed_paging with cut_at_col = 255 }
let xopen_paging    = { fixed_paging with cut_at_col = 80 }
let crt_paging      = { fixed_paging with cut_at_col = 320 }
let terminal_paging = crt_paging
let cobolx_paging   = { cut_at_col = 255; alphanum_padding = None }

(* Actual format and indicator positioning *)

type 'k source_format = 'k indicator_position * 'k paging
and _ indicator_position =
  |    NoIndic:  free indicator_position
  | FixedIndic: fixed indicator_position
  | XOpenIndic: fixed indicator_position
  |   CRTIndic: fixed indicator_position
  |   TrmIndic: fixed indicator_position
  |  CBLXIndic: fixed indicator_position
and any =
  | SF: 'k source_format -> any                                        [@@unboxed]

let equal
    (type k r) : k source_format -> r source_format -> bool =
  fun (i1, p1) (i2, p2) -> match i1, i2 with
    |    NoIndic,    NoIndic -> true
    | FixedIndic, FixedIndic -> p1 = p2
    | XOpenIndic, XOpenIndic -> p1 = p2
    |   CRTIndic,   CRTIndic -> p1 = p2
    |   TrmIndic,   TrmIndic -> p1 = p2
    |  CBLXIndic,  CBLXIndic -> p1 = p2
    | _ -> false

(* let to_config *)
(*     (type k) : k source_format -> Cobol_config.source_format = function *)
(*   |    NoIndic, _ -> SFFree *)
(*   | FixedIndic, FixedWidth p when p == fixed_paging -> SFFixed *)
(*   | FixedIndic, FixedWidth p when p == variable_paging -> SFVariable *)
(*   | FixedIndic, FixedWidth _ (\* when p == xcard_paging *\) -> SFxCard *)
(*   | XOpenIndic, FixedWidth _ (\* when p == xopen_paging *\) -> SFXOpen *)
(*   |   CRTIndic, FixedWidth _ (\* when p == crt_paging *\) -> SFCRT *)
(*   |   TrmIndic, FixedWidth _ (\* when p == terminal_paging *\) -> SFTrm *)
(*   |  CBLXIndic, FixedWidth _ (\* when p == cobolx_paging *\) -> SFCOBOLX *)

let from_config
  : Cobol_config.source_format -> any = function
  | SFFree     -> SF (   NoIndic, FreePaging)
  | SFFixed    -> SF (FixedIndic, FixedWidth    fixed_paging)
  | SFVariable -> SF (FixedIndic, FixedWidth variable_paging)
  | SFXOpen    -> SF (XOpenIndic, FixedWidth    xopen_paging)
  | SFxCard    -> SF (FixedIndic, FixedWidth    xcard_paging)
  | SFCRT      -> SF (  CRTIndic, FixedWidth      crt_paging)
  | SFTrm      -> SF (  TrmIndic, FixedWidth terminal_paging)
  | SFCOBOLX   -> SF ( CBLXIndic, FixedWidth   cobolx_paging)

let decypher ~dialect format =
  Result.map from_config @@
  match String.uppercase_ascii @@format, dialect with
  (* SOURCEFORMAT"FREE" on MF means: X/Open free format *)
  (* cf https://www.microfocus.com/documentation/visual-\
       cobol/vc50pu7/VS2019/HRLHLHINTR01U008.html *)
  | "FREE", Cobol_config.DIALECT.MicroFocus _ -> Ok Cobol_config.SFXOpen
  | "FREE",                      _            -> Ok SFFree
  | "FIXED",                     _            -> Ok SFFixed
  | "VARIABLE",                  _            -> Ok SFVariable
  | "XOPEN",                     _            -> Ok SFXOpen
  | "XCARD",                     _            -> Ok SFxCard
  | "CRT",                       _            -> Ok SFCRT
  | "TERMINAL",                  _            -> Ok SFTrm
  | "COBOLX",                    _            -> Ok SFCOBOLX
  | _                                         -> Error (`SFUnknown format)

(* --- *)

let first_area_b_column
    (type k) : k source_format -> int option = function
  | _,                     FreePaging
  | XOpenIndic,            _            -> None
  | FixedIndic,            FixedWidth _ -> Some (7 + 4)
  |(CRTIndic | TrmIndic |
    CBLXIndic),            FixedWidth _ -> Some (1 + 4)

(* --- *)

type comment_entry_termination =                  (* skip until... *)
  | Newline                                       (* ... newline *)
  | Period                                        (* ... next period (unused) *)
  | AreaB of { first_area_b_column: int }         (* ... next word in area A *)

(* let comment_entry_termination *)
(*   : type k. k source_format -> comment_entry_termination = fun sf -> *)
(*   match sf, first_area_b_column sf with *)
(*   |(_         , FreePaging   ), _ *)
(*   |(XOpenIndic, _            ), _ *)
(*   |(_         , _            ), None   -> Newline *)
(*   | (FixedIndic, FixedWidth _), Some c -> AreaB { first_area_b_column = c } *)
(*   |((CRTIndic | *)
(*      TrmIndic | *)
(*      CBLXIndic), FixedWidth _), Some c -> AreaB { first_area_b_column = c } *)

let comment_entry_termination
    (type k) : k source_format -> comment_entry_termination = fun sf ->
  match sf, first_area_b_column sf with
  |(_         , FreePaging   ), _
  |(XOpenIndic, _            ), _
  |(_         , _            ), None   -> Newline
  | (FixedIndic, FixedWidth _), Some c -> AreaB { first_area_b_column = c }
  |((CRTIndic |
     TrmIndic |
     CBLXIndic), FixedWidth _), Some c -> AreaB { first_area_b_column = c }

(* --- *)

let looks_like_fixed_format ?(tab_stop = 8) contents_prefix =
  let rec sna ap vp =
    match contents_prefix.[ap] with
    | '\n' -> sna (succ ap) 1
    | '\t' -> sna (succ ap) (vp + (tab_stop - (vp + tab_stop) mod tab_stop))
    | '\r' -> sna (succ ap) vp
    | _ when vp <> 7 -> sna (succ ap) (succ vp)
    | ' ' | '-' | 'd' | 'D' | '*' | '/' | '\\' | '$' when vp = 7 -> true
    | _ -> false
  in
  try sna 0 1 with Invalid_argument _ -> false

let guess_from ~contents_prefix =
  if looks_like_fixed_format contents_prefix
  then from_config SFFixed
  else from_config SFFree
