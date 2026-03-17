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

module TYPES: sig

  type fileloc = [ `Word of string | `Alphanum of string ]

  type lookup_config =
    {
      lookup_path: string list;
      lookup_exts: string list;
    }

  type lookup_error =
    {
      lookup_libname: string;
      lookup_config: lookup_config;
    }

end

open TYPES

val copybook_extensions: string list
val copybookonly_extensions: string list

val lookup_config: ?libexts: string list -> string list -> lookup_config

val pp_lookup_config: lookup_config Pretty.printer
val pp_lookup_error: lookup_error Pretty.printer
