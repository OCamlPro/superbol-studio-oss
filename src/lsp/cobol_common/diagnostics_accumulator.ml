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
     end) =
struct

  module TYPES = struct
    type 'a with_diags = { result: 'a; diags: Set.t }
  end
  include TYPES

  let result ?(diags = Set.none) result = { result; diags }
  let simple_result r = result r
  let some_result ?diags r = result ?diags (Some r)
  let no_result ~diags = { result = None; diags }

  let with_diags r diags =
    result ~diags r
  let with_more_diags ~diags { result; diags = diags' } =
    { result; diags = Set.union diags' diags }

  let result_only { result; _ } = result
  let forget_result { diags; _ } = diags

  let more_result ~f { result; diags } =
    with_more_diags ~diags (f result)
  let map_result ~f { result; diags } =
    { result = f result; diags }
  let map2_results ~f r1 r2 =
    more_result ~f:(f r1.result) (with_more_diags ~diags:r1.diags r2)
  let map_some_result ~f =
    map_result ~f:(Option.map f)
  let merge_results ~f r1 r2 =
    result (f r1.result r2.result) ~diags:(Set.union r1.diags r2.diags)

  let cons_option_result = function
    | { result = None; diags } -> with_more_diags ~diags
    | { result = Some r; diags } -> more_result ~f:(fun tl -> result ~diags @@ r :: tl)

  let translate_diags { result; diags } =                        (* temporary *)
    Diagnostics.result result ~diags:(Set.translate diags)

  let show_n_forget
      ?(set_status = true) ?(min_level = Diagnostics.Hint)
      ?(ppf = Fmt.stderr) { result; diags } =
    let diags = Set.translate diags in
    Diagnostics.Set.pp_above ~level:min_level ppf diags;
    if set_status && Diagnostics.Set.has_errors diags then Exit_status.raise ();
    result

  let sink_result ?set_status ?ppf r =
    ignore @@ show_n_forget ?set_status ?ppf r
end
