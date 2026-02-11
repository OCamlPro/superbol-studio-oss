(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2024 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open EzCompat

module TYPES = struct

  type dialect =
    | Dialect_GnuCOBOL
    | Dialect_MicroFocus
    | Dialect_IBM
    | Dialect_MVS
    | Dialect_BS2000
    | Dialect_ACU
    | Dialect_RM
    | Dialect_STD85
    | Dialect_STD2002
    | Dialect_STD2014
    | Dialect_Other of string

  type source_format =
    | SFFree
    | SFFixed
    | SFVariable
    | SFXOpen
    | SFxCard
    | SFCRT
    | SFTrm
    | SFCOBOLX

  type source_format_spec =
    | Auto
    | SF of source_format

  type support_level =
    | Level_ok
    | Level_warning
    | Level_archaic
    | Level_obsolete
    | Level_skip
    | Level_ignore
    | Level_error
    | Level_unconformable

  (* feature_name *)
  type configuration =
    {
      name: string;
    }

  type feature = {
    feature_name : configuration ;
    feature_safe : bool ;
    feature_level : support_level ;
  }

  type features = {
    safe_partial_replacing_when_src_literal : feature ;
    free_redefines_position : feature ;
  }

  type cobol_config = {
    dialect : dialect ;
    pic_length : int ;
    features : features ;
    intrinsic_functions : StringSet.t ;
    words : Reserved_words.TYPES.words_spec;
  }

  module type CONFIG = sig
    val c: cobol_config
  end

end

open TYPES

module DIAG = struct
  type 'a used_feature =
      {
        loc: Srcloc.TYPES.srcloc;
        feature: feature;
        reason: 'a;
      }

  type error =
    | Invalid_use_of_feature of error_reason used_feature
  and error_reason =
    [ `Unconformable of configuration
    | `Used ]

  let error_loc = function
    | Invalid_use_of_feature { loc; _ } ->
        loc

  let pp_error ppf = function
    | Invalid_use_of_feature { feature = f; reason = `Used; _ } ->
        Pretty.print ppf "%s@ used" f.feature_name.name
    | Invalid_use_of_feature { feature = f; reason = `Unconformable c; _ } ->
        Pretty.print ppf "%s@ does@ not@ conform@ to@ %s" f.feature_name.name c.name

  type warning =
    | Feature_used of warning_reason used_feature
  and warning_reason =
    [ `Archaic of configuration
    | `Obsolete of configuration
    | `Ignored
    | `Used ]

  let warning_loc = function
    | Feature_used { loc; _ } ->
        loc

  let pp_warning ppf = function
    | Feature_used { feature = f; reason = `Used; _ } ->
        Pretty.print ppf "%s@ used" f.feature_name.name
    | Feature_used { feature = f; reason = `Archaic c; _ } ->
      Pretty.print ppf "%s@ is@ archaic@ in@ %s"
        f.feature_name.name c.name
    | Feature_used { feature = f; reason = `Obsolete c; _ } ->
      Pretty.print ppf "%s@ is@ obsolete@ in@ %s"
        f.feature_name.name c.name
    | Feature_used { feature = f; reason = `Ignored; _ } ->
      Pretty.print ppf "%s@ used" f.feature_name.name

  type diagnostic =
    | Error of error
    | Warning of warning

  let decompose_verification_result = function
    | Ok (result, None) ->
        Some result,
        None
    | Ok (result, Some (loc, feature, reason)) ->
        Some result,
        Some (Warning (Feature_used { loc; feature = feature;
                                      reason = (reason :> warning_reason) }))
    | Error (Some (loc, feature, `Ignored)) ->
        None,
        Some (Warning (Feature_used { loc; feature = feature;
                                      reason = `Ignored }))
    | Error None ->
        None,
        None
    | Error (Some (loc, feature, (`Used | `Unconformable _ as reason))) ->
        None,
        Some (Error (Invalid_use_of_feature { loc; feature = feature;
                                              reason }))

end

let  verify feature ~loc =
  match feature.feature_level with
  | Level_ok ->
    Ok (feature.feature_safe, None)
  | Level_warning ->
    Ok (feature.feature_safe, Some (loc, feature, `Used))
  | Level_archaic ->
    Ok (feature.feature_safe,
        Some (loc, feature, `Archaic feature.feature_name))
  | Level_obsolete ->
    Ok (feature.feature_safe,
        Some (loc, feature, `Obsolete feature.feature_name))
  | Level_skip ->
    Error None
  | Level_ignore ->
    Error (Some (loc, feature, `Ignored))
  | Level_error ->
    Error (Some (loc, feature, `Used))
  | Level_unconformable ->
    Error (Some (loc, feature, `Unconformable feature.feature_name))

let safe_partial_replacing_when_src_literal = {
  feature_name =
    { name = "safe_partial_replacing_when_src_literal" };
  feature_safe = false ;
  feature_level = Level_obsolete ;
}

let free_redefines_position = {
  feature_name =
    { name = "free_redefines_position" };
  feature_safe = true ;
  feature_level = Level_warning ;
}

open Reserved_words.TYPES

let words =
  let alias_for b = ReserveAlias { alias_for = b;
                                   preserve_context_sensitivity = false } in
  Reserved_words.words @
  List.map (fun w -> w, NotReserved)
    Reserved_words.not_reserved @
  List.map (fun (a, b) -> a, alias_for b)
    Reserved_words.default_aliases

let not_reserved = StringSet.of_list
    Reserved_words.not_reserved

let intrinsic_functions = StringSet.diff
    Reserved_words.default_intrinsics not_reserved

let default = {
  pic_length = 255 ;
  dialect = Dialect_MicroFocus ;
  features = {
    safe_partial_replacing_when_src_literal ;
    free_redefines_position ;
  };
  intrinsic_functions ;
  words ;
}
