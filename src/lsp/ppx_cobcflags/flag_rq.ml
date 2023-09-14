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

let get_symbols loc flag_name fields =
  let ctors, strs =
    List.fold_left (fun (ctors, strs) field ->
        match field.prf_desc with
        | Rtag ({txt = n; loc} , true, []) ->
            let name = Located.mk ~loc n in
            let args = Pcstr_tuple [] in
            let ctor = constructor_declaration ~loc ~name ~args ~res:None in
            let str_cst = Pconst_string (n, loc, None) in
            (ctor::ctors, str_cst::strs)
        | _ -> failwith "Invalid variant")
      ([], [])
      fields
  in
  let variant = Ptype_variant (ctors) in
  let type_decl =
    type_declaration ~loc ~name:flag_name ~params:[] ~cstrs:[] ~kind:variant ~private_:Public
      ~manifest:None
  in
  let cases =
    List.rev @@ case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:[%expr failwith "Invalid case"]
    :: List.fold_left2 (fun cases ctor cst ->
        let loc = ctor.pcd_loc in
        let lhs = ppat_constant ~loc cst in
        let lident = Located.lident ~loc ctor.pcd_name.txt in
        let rhs = pexp_construct ~loc lident None in
        case ~lhs ~guard:None ~rhs :: cases)
      []
      ctors
      strs
  in
  let match_expr = pexp_match ~loc [%expr arg] cases in
  let flag_val = Located.lident ~loc:flag_name.loc flag_name.txt |> pexp_ident ~loc in
  let symbols = List.fold_left (fun list cst ->
      let cst_expr = pexp_constant ~loc cst in
      [%expr [%e cst_expr]::[%e list]])
      [%expr []]
      strs
  in
  let arg_set = [%expr
    Arg.Symbol ([%e symbols], fun arg -> [%e flag_val] := [%e match_expr])
  ]
  in
  pstr_type ~loc Recursive [type_decl], arg_set

let get_arg_type flag_name {txt = flag_type; loc} flag_val =
  match flag_type with
  | [%type: int] ->
      None, [%expr Arg.Set_int [%e flag_val]]
  | [%type: string] ->
      None, [%expr Arg.Set_string [%e flag_val]]
  | {ptyp_desc = Ptyp_variant (fields, Closed, None); ptyp_loc = loc; _ } ->
      let type_decl, arg_set = get_symbols loc flag_name fields in
      Some (type_decl),
      arg_set
  | _ -> None,
         pexp_extension ~loc @@
         Location.error_extensionf ~loc "Type not handled yet"

let expand_flag_rq ~loc ~path:_ flag_name flag_type params =
  match params with
  | [{pexp_desc = Pexp_constant (Pconst_string (n, nloc, _)); _};
     default_exp;
     {pexp_desc = Pexp_constant ((Pconst_string (_, docv_loc, _)) as docv_cst); _};
     {pexp_desc = Pexp_constant ((Pconst_string (_, docs_loc, _)) as docs_cst); _};] ->
      let flag_pat = ppat_var ~loc flag_name in
      let flag_val = Located.lident ~loc:flag_name.loc flag_name.txt |> pexp_ident ~loc in
      let flag_str = pexp_constant ~loc:nloc (Pconst_string ("f"^n, nloc, None)) in
      let docv_str = pexp_constant ~loc:docv_loc docv_cst in
      let docs_str = pexp_constant ~loc:docs_loc docs_cst in
      let _arg_type, arg_set = get_arg_type flag_name flag_type flag_val in
      let arg_type = None in
      begin match arg_type with
      | None ->
          [%stri
            let [%p flag_pat] =
              let [%p flag_pat] = ref [%e default_exp] in
              let args = [
                [[%e flag_str]],
                [%e arg_set],
                EZCMD.info ~docv:[%e docv_str]
                  [%e docs_str]
              ] in
              all_gnucobol_args := args @ !all_gnucobol_args;
              [%e flag_val], args]
      | Some type_decl ->
          [%stri include struct
            [%%i type_decl]
            let [%p flag_pat] =
              let [%p flag_pat] = ref [%e default_exp] in
              let args = [
                [[%e flag_str]],
                [%e arg_set],
                EZCMD.info ~docv:[%e docv_str]
                  [%e docs_str]
              ] in
              all_gnucobol_args := args @ !all_gnucobol_args;
              [%e flag_val], args
          end]
      end
  | _ ->
      [] |>
      pstr_extension ~loc @@
      Location.error_extensionf ~loc
        "Expecting a tuple of format (string, <default_value>, string, string)"

let extension_flag_rq =
  Extension.declare "flag_rq"
    Extension.Context.structure_item
    Ast_pattern.(pstr
                   (pstr_value
                      nonrecursive
                      (value_binding
                         ~pat:(ppat_constraint (ppat_var __') __')
                         ~expr:(pexp_tuple __)
                      ^::nil)
                    ^:: nil))
    expand_flag_rq
