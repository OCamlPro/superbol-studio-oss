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

open Types

let default       = Default
let gnucobol      = GnuCOBOL
let cobol85       = COBOL85
let cobol2002     = COBOL2002
let cobol2014     = COBOL2014
let acu           = ACU        { strict = false }
let acu_strict    = ACU        { strict = true }
let bs2000        = BS2000     { strict = false }
let bs2000_strict = BS2000     { strict = true }
let gcos          = GCOS       { strict = false }
let gcos_strict   = GCOS       { strict = true }
let ibm           = IBM        { strict = false }
let ibm_strict    = IBM        { strict = true }
let mf            = MicroFocus { strict = false }
let mf_strict     = MicroFocus { strict = true }
let mvs           = MVS        { strict = false }
let mvs_strict    = MVS        { strict = true }
let realia        = Realia     { strict = false }
let realia_strict = Realia     { strict = true }
let rm            = RM         { strict = false }
let rm_strict     = RM         { strict = true }
let xopen         = XOpen

let to_string: dialect -> string = function
  | Default                       -> "default"
  | GnuCOBOL                      -> "gnucobol"
  | COBOL85                       -> "cobol85"
  | COBOL2002                     -> "cobol2002"
  | COBOL2014                     -> "cobol2014"
  | ACU        { strict = false } -> "acu"
  | ACU        { strict = true  } -> "acu-strict"
  | BS2000     { strict = false } -> "bs2000"
  | BS2000     { strict = true  } -> "bs2000-strict"
  | GCOS       { strict = false } -> "gcos"
  | GCOS       { strict = true  } -> "gcos-strict"
  | IBM        { strict = false } -> "ibm"
  | IBM        { strict = true  } -> "ibm-strict"
  | MicroFocus { strict = false } -> "mf"
  | MicroFocus { strict = true  } -> "mf-strict"
  | MVS        { strict = false } -> "mvs"
  | MVS        { strict = true  } -> "mvs-strict"
  | Realia     { strict = false } -> "realia"
  | Realia     { strict = true  } -> "realia-strict"
  | RM         { strict = false } -> "rm"
  | RM         { strict = true  } -> "rm-strict"
  | XOpen                         -> "xopen"

let of_string: string -> dialect = fun s ->
  match String.lowercase_ascii s with
  | "default"   -> Default
  | "gnucobol"  -> GnuCOBOL
  | "cobol85"   -> COBOL85
  | "cobol2002" -> COBOL2002
  | "cobol2014" -> COBOL2014
  | "xopen"     -> XOpen
  | l ->
    let prefix, strict = match EzString.chop_suffix l ~suffix:"-strict" with
      | Some prefix -> prefix, true
      | None -> l, false
    in
    match prefix with
    | "acu"               -> ACU { strict }
    | "bs2000"            -> BS2000 { strict }
    | "gcos"              -> GCOS { strict }
    | "ibm"               -> IBM { strict }
    | "mf" | "microfocus" -> MicroFocus { strict }
    | "mvs"               -> MVS { strict }
    | "realia"            -> Realia { strict }
    | "rm"                -> RM { strict }
    | _ -> invalid_arg s

let all_canonical_names =
  [
    "default";
    "gnucobol";
    "cobol85";
    "cobol2002";
    "cobol2014";
  ] @ List.concat_map (fun d -> [d; d ^ "-strict"]) [
    "acu";
    "bs2000";
    "gcos";
    "ibm";
    "mf";
    "mvs";
    "realia";
    "rm";
  ] @ [
    "xopen";
  ]

let of_gnucobol_config_name: string -> dialect = function
  | "COBOL 85"                -> COBOL85
  | "COBOL 2002"              -> COBOL2002
  | "COBOL 2014"              -> COBOL2014
  | "GnuCOBOL"                -> GnuCOBOL
  | "ACUCOBOL-GT"             -> ACU        { strict = true  }
  | "ACUCOBOL-GT (lax)"       -> ACU        { strict = false }
  | "BS2000 COBOL"            -> BS2000     { strict = true  }
  | "BS2000 COBOL (lax)"      -> BS2000     { strict = false }
  | "GCOS"                    -> GCOS       { strict = true  }
  | "GCOS (lax)"              -> GCOS       { strict = false }
  | "IBM COBOL"               -> IBM        { strict = true  }
  | "IBM COBOL (lax)"         -> IBM        { strict = false }
  | "Micro Focus COBOL"       -> MicroFocus { strict = true  }
  | "Micro Focus COBOL (lax)" -> MicroFocus { strict = false }
  | "IBM COBOL for MVS & VM"  -> MVS        { strict = true  }
  | "MVS/VM COBOL (lax)"      -> MVS        { strict = false }
  | "CA Realia II"            -> Realia     { strict = true  }
  | "CA Realia II (lax)"      -> Realia     { strict = false }
  | "RM-COBOL"                -> RM         { strict = true  }
  | "RM-COBOL (lax)"          -> RM         { strict = false }
  | "X/Open COBOL"            -> XOpen
  | s                         -> of_string s
