(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

open MenhirSdk.Cmly_api
module type S = sig
  module G : GRAMMAR

  type item = G.lr1 * G.production * int
  type recovery = G.lr1 -> int * (G.lr1 option * item list) list

  val recover : recovery
  val report : Format.formatter -> unit
end

module Make (G : GRAMMAR) (S : Synthesis.S with module G = G) : S with module G = G
