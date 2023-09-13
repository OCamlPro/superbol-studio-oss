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

open Ppxlib
open Ast_builder.Default

let expand_flag on ~loc ~path:_ flag_name flag_args =
  match flag_args with
  | [{pexp_desc = Pexp_constant (Pconst_string (n, nloc, _)); _};
     {pexp_desc = Pexp_constant (Pconst_string (_, docsloc, _) as docs_cst); _}] ->
      let flag_pat = ppat_var ~loc flag_name in
      let flag_val = Located.lident ~loc:flag_name.loc flag_name.txt |> pexp_ident ~loc in
      let flag_str = pexp_constant ~loc:nloc (Pconst_string ("f"^n, nloc, None)) in
      let flag_no_str = pexp_constant ~loc:nloc (Pconst_string ("fno-"^n, nloc, None)) in
      let docs_str = pexp_constant ~loc:docsloc docs_cst in
      let arg_set = [%expr Arg.Set [%e flag_val]] in
      let arg_unset = [%expr Arg.Clear [%e flag_val]] in
      let default_value = if on then [%expr true] else [%expr false] in
      let set_help = if on then [%expr "see " ^ [%e flag_no_str]] else docs_str in
      let unset_help = if not @@ on then [%expr "see " ^ [%e flag_str]] else docs_str in
      [%stri
        let [%p flag_pat] =
          let [%p flag_pat] = ref [%e default_value] in
          let args = [
            [[%e flag_str]],
            [%e arg_set],
            EZCMD.info
              [%e set_help];
            [[%e flag_no_str]],
            [%e arg_unset],
            EZCMD.info [%e unset_help]
          ] in
          all_gnucobol_args := args @ !all_gnucobol_args;
          [%e flag_val], args]
  | _ ->
      [] |>
      pstr_extension ~loc @@
      Location.error_extensionf ~loc
        "Expecting a tuple of format (string, string)"

let extension_flag =
  Extension.declare "flag"
    Extension.Context.structure_item
    Ast_pattern.(pstr
                   (pstr_value
                      nonrecursive
                      (value_binding
                         ~pat:(ppat_var __')
                         ~expr:(pexp_tuple __)
                      ^::nil)
                    ^:: nil))
    (expand_flag false)

let extension_flag_on =
  Extension.declare "flag_on"
    Extension.Context.structure_item
    Ast_pattern.(pstr
                   (pstr_value
                      nonrecursive
                      (value_binding
                         ~pat:(ppat_var __')
                         ~expr:(pexp_tuple __)
                      ^::nil)
                    ^:: nil))
    (expand_flag true)
