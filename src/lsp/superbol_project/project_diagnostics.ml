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

open Ez_toml.V1

type error =
  | Invalid_toml of { loc: TOML.Types.location; error: TOML.Types.error }
  | Unknown_dialect of string
  | Cobol_config_error of Cobol_config.Diagnostics.error

let pp_error ppf = function
  | Invalid_toml { loc; error } ->
      Pretty.print ppf "%s: %s"
        (TOML.string_of_location loc) (TOML.string_of_error error)
  | Unknown_dialect name ->
      Pretty.print ppf "Unknown@ dialect: `%s'" name
  | Cobol_config_error e ->
      Cobol_config.Diagnostics.pp_error ppf e
