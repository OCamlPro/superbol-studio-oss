(******************************************************************************)
(*                                                                            *)
(*     Copyright (c) 2021-2023 OCamlPro SAS                                   *)
(*                                                                            *)
(*     All rights reserved.                                                   *)
(*     This file is distributed under the terms of the                        *)
(*     OCAMLPRO-NON-COMMERCIAL license.                                       *)
(*                                                                            *)
(******************************************************************************)

type t = {
  graph_name: string option;
  hide_unreachable: bool;
  collapse_fallthru: bool;
  shatter_hubs: int option;
}

let create
    ?(graph_name=None)
    ?(hide_unreachable=false)
    ?(collapse_fallthru=true)
    ?(shatter_hubs=None)
    ()
  = { hide_unreachable; collapse_fallthru; graph_name; shatter_hubs }

