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
open Cobol_common.Diagnostics.TYPES

module Make (Config: Cobol_config.Types.T) : sig

  val replacing'
    : ?repl_dir:Directives.replacing_direction
    -> [< `Alphanum of Text.pseudotext
       | `PseudoText of Text.pseudotext ] Cobol_common.Srcloc.with_loc
    -> Text.pseudotext Cobol_common.Srcloc.with_loc
    -> Directives.replacing option Cobol_common.Diagnostics.with_diags

  val filter_map_4_list_with_diags'
    : 'a option with_diags with_loc list -> 'a with_loc list with_diags

end
