(******************************************************************************)
(*                                                                            *)
(*     Copyright (c) 2021-2023 OCamlPro SAS                                   *)
(*                                                                            *)
(*     All rights reserved.                                                   *)
(*     This file is distributed under the terms of the                        *)
(*     OCAMLPRO-NON-COMMERCIAL license.                                       *)
(*                                                                            *)
(******************************************************************************)

open Cfg_types

val make
  : options:Cfg_options.t
  -> name:string
  -> Cobol_typeck.Outputs.t
  -> Cfg.t * Cfg.t

val possible_cfgs_of_doc
  : Cobol_typeck.Outputs.t
  -> string list
