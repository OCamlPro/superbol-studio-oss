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

module MAKE
    (Set: sig
       type t
       val none: t
       val union: t -> t -> t
       val translate: t -> Diagnostics.diagnostics                (* temporary *)
     end) : sig

  module TYPES: sig
    type 'a with_diags = { result: 'a; diags: Set.t }
  end
  include module type of TYPES with type 'a with_diags = 'a TYPES.with_diags
  open Set

  val result: ?diags:t -> 'a -> 'a with_diags
  val simple_result: 'a -> 'a with_diags
  val some_result: ?diags:t -> 'a -> 'a option with_diags
  val no_result: diags:t -> _ option with_diags

  val with_diags: 'a -> t -> 'a with_diags
  val with_more_diags: diags:t -> 'a with_diags -> 'a with_diags

  val result_only: 'a with_diags -> 'a
  val forget_result: _ with_diags -> t

  val more_result
    : f:('a -> 'b with_diags) -> 'a with_diags -> 'b with_diags
  val map_result
    : f:('a -> 'b) -> 'a with_diags -> 'b with_diags
  val map2_results
    : f:('a -> 'b -> ('c with_diags as 'x)) -> 'a with_diags -> 'b with_diags -> 'x
  val map_some_result
    : f:('a -> 'b) -> 'a option with_diags -> 'b option with_diags
  val merge_results
    : f:('a -> 'b -> 'c) -> 'a with_diags -> 'b with_diags -> 'c with_diags

  val cons_option_result
    : 'a option with_diags -> 'a list with_diags -> 'a list with_diags

  val translate_diags
    : 'a with_diags -> 'a Diagnostics.with_diags
  val show_n_forget
    : ?set_status:bool -> ?min_level:Diagnostics.severity
    -> ?ppf:Format.formatter -> 'a with_diags -> 'a
  val sink_result
    : ?set_status:bool -> ?ppf:Format.formatter -> _ with_diags -> unit
end
