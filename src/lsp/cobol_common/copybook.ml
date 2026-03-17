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

module TYPES = struct

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

(** Filename extensions that are searched when looking up copybooks. *)
let copybook_extensions =
  ["cpy"; "cbl"; "cob"]

(** Filename extensions that we should not treat as main programs, but as
    copybooks instead. *)
let copybookonly_extensions =
  ["cpy"; "copy"; "copybook"]

let lookup_config ?(libexts = copybook_extensions) libpath =
  {
    lookup_path = libpath;
    lookup_exts = List.map String.lowercase_ascii libexts;
  }

let pp_lookup_config ppf { lookup_path; lookup_exts } =
  let pp_path ppf = function
    | [] -> Pretty.string ppf "<empty>"
    | path -> Pretty.path ppf path
  in
  Pretty.print ppf
    "@[- search@ path:@ %a@]@;\
     @[- filename@ extensions:@ %a@]"
    pp_path lookup_path Fmt.(list ~sep:sp @@ fmt "%S") lookup_exts

let pp_lookup_error ppf { lookup_libname; lookup_config } =
  Pretty.print ppf
    "@[<v>@[Library@ `%s'@ not@ found@]@;%a@]"
    lookup_libname pp_lookup_config lookup_config
