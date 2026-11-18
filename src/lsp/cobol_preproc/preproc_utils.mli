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

open Cobol_common.Srcloc.TYPES
open Preproc_outputs.TYPES

module type PARAMS = sig
  val safe_partial_replacing_when_src_literal:
    [`Safe | `Unsafe] Cobol_dialect.Types.binding
end

module Make (Params: PARAMS) : sig

  val replacing'
    : ?repl_dir:Preproc_directives.replacing_direction
    -> [< `Alphanum of Text.pseudotext
       | `PseudoText of Text.pseudotext ] Cobol_common.Srcloc.with_loc
    -> Text.pseudotext Cobol_common.Srcloc.with_loc
    -> Preproc_directives.replacing option with_diags

  val filter_map_4_list_with_diags'
    : 'a option with_diags with_loc list -> 'a with_loc list with_diags

end
