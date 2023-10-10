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
open Cobol_common.Srcloc.INFIX
open Cobol_common.Diagnostics.TYPES
module DIAGS = Cobol_common.Diagnostics

module Make (Config: Cobol_config.T) = struct

  let source_format_lexdir ~dialect format =
    match Src_format.decypher ~dialect ~&format with
    | Ok (SF sf) ->
        DIAGS.some_result @@ Preproc_directives.LexDirSource (sf &@<- format)
    | Error (`SFUnknown f) ->
        DIAGS.error_result None ~loc:~@format "Unknown@ source@ format@ `%s'" f

  let safe_partial_replacing_when_src_literal ~loc =
    Config.safe_partial_replacing_when_src_literal#verify' ~loc:(Some loc) |>
    DIAGS.map_result (function Some s -> s = `Safe | None -> false)

  let replacing' ?repl_dir repl_from repl_to =
    match repl_dir, ~&repl_from with
    | None, (`PseudoText src | `Alphanum src) ->
        Text_processor.replacing (src &@<- repl_from) repl_to
    | Some repl_dir, `PseudoText src ->
        Text_processor.replacing ~partial:{ repl_dir; repl_strict = false }
          (src &@<- repl_from) repl_to
    | Some repl_dir, `Alphanum src ->
        let { result = repl_strict; diags } =
          let loc = ~@repl_to in
          match ~&repl_to with
          | [{ payload = PseudoWord [{ payload = PwText str; _ }]; _ }]
            when str = "" ->
              safe_partial_replacing_when_src_literal ~loc
          | [{ payload = PseudoWord [{ payload = PwText str; _ }]; _ }]
            when String.contains str ' ' ||     (* TODO: properly check spaces *)
                 String.contains str '\t' ->                         (* reject *)
              DIAGS.error_result false ~loc "Forbidden@ operand@ with@ spaces"
          | [{ payload = PseudoWord (_::_::_); _ }] | _::_::_ ->
              DIAGS.error_result false ~loc "Forbidden@ multi-word@ operand"
          | _ ->
              DIAGS.result false
        in
        DIAGS.with_more_diags ~diags @@
        Text_processor.replacing ~partial:{ repl_dir; repl_strict }
          (src &@<- repl_from) repl_to

  let filter_map_4_list_with_diags'
    : 'a option with_diags with_loc list -> 'a with_loc list with_diags = fun l ->
    List.fold_left
      begin fun (result, diags) { payload = { result = r; diags = d }; loc } ->
        (match r with None -> result | Some r -> (r &@ loc) :: result),
        DIAGS.Set.union diags d
      end ([], DIAGS.Set.none) l |>
    fun (result, diags) -> { result = List.rev result; diags }

end
