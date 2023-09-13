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

(* open Cobol_common.Basics *)
(* open Cobol_typing.CobolEnv *)
(* open Cobol_ast.INFIX *)

(*FIXME: Rework on the entire module *)

module Mangler = struct
  type t = {
    mangled_name: string;
    original_name: string;
  }

  type env =
    | Elem of t
    | Grp of t * env list

  let _count = ref 0
  let _last_buf = ref 0
  let new_buf_name () =
    incr _count;
    _last_buf := !_count;
    Format.sprintf "b_%i" (!_count)

  let last_buf_name () =
    Format.sprintf "b_%i" (!_last_buf)

  let new_field_name () =
    incr _count;
    Format.sprintf "f_%i" (!_count)

  exception Found of t

  let rec find name env =
    match env with
    | Elem ({original_name; _} as m) ->
        if original_name = name then
          m
        else
          raise Not_found
    | Grp ({original_name; _} as m, subs) ->
        if original_name = name then
          raise @@ Found m
        else
          let rec aux = function
            | [] -> raise Not_found
            | hd::tl -> try
                  find name hd
                with Not_found ->
                  aux tl
          in
          aux subs

end

(*
module GAst_trav = Grouped_ast.Grouped_ast_traversal

(* let mangling_traversal_funs =
  let open Cobol_typing.DataGroup in
  let open Cobol_typing.CobolEnv.PROG_ENV in
  let open Cobol_typing.CobolEnv.DATA_ITEM in
  let rec sub_items_mangling buf (data_items: DATA_ITEM.t StringMap.t) = function
    | Elementary {name; data_desc} ->
        let data_item = StringMap.find ~&name data_items in
        let new_name = Mangler.new_field_name () in
        (* Format.printf "Buf: %s Field: %s For: %s\n" buf new_name name; *)
        let data_items = StringMap.add
            ~&(data_item.name)
            {data_item with c_buf = buf; c_field = new_name}
            data_items
        in
        data_items, Elementary (name, dde)
    | Group (name, dgl, dde) ->
        let data_item = StringMap.find name data_items in
        let new_name = Mangler.new_field_name () in
        (* Format.printf "Buf: %s Field: %s For: %s\n" buf new_name name; *)
        let sub_items, dgl = List.fold_left_map (sub_items_mangling buf) data_item.sub_items dgl in
        let data_item = {data_item with c_buf = buf; c_field = new_name; sub_items;} in
        let data_items =
          StringMap.replace (Option.get data_item.data_name) data_item data_items
        in
        data_items, Group (name, dgl, dde)
  in
  let data_div_traversal_funs stage =
    GAst_trav.{Data_div_traversal.fm_default with
     working_storage_section = (fun _funs prog_env wss ->
       let prog_env, wss =
         List.fold_left_map (fun (prog_env: PROG_ENV.t) dg ->
           let dg, prog_env = match dg with
             | Elementary (name, dde) ->
                 (* Format.printf "Buf: %s Field: %s For: %s\n" new_name new_field name; *)
                 let data_item = if stage = `Working then
                     ENV.find name prog_env.data_items
                   else
                     ENV.find name prog_env.linkage_items
                 in
                 let new_name = if Option.is_none data_item.redefines then
                     Mangler.new_buf_name ()
                   else
                     Mangler.last_buf_name ()
                 in
                 let new_field = if Option.is_none data_item.redefines then
                     "f_"^(String.sub new_name 2 ((String.length new_name) - 2))
                   else
                     Mangler.new_field_name ()
                 in
                 begin
                   match stage with
                   | `Working ->
                       let prog_env =
                         {prog_env with
                          data_items =
                            ENV.replace
                              (Option.get data_item.data_name)
                              {data_item with c_buf = new_name; c_field = new_field}
                              prog_env.data_items
                         }
                       in
                       Elementary (name, dde), prog_env
                   | `Linkage ->
                       let prog_env =
                         {prog_env with
                          linkage_items =
                            ENV.replace
                              (Option.get data_item.data_name)
                              {data_item with c_buf = new_name; c_field = new_field}
                              prog_env.linkage_items
                         }
                       in
                       Elementary (name, dde), prog_env
                 end
             | Group (name, dgl, dde) ->
                 (* Format.printf "Buf: %s Field: %s For: %s\n" new_name new_field name; *)
                 let data_item = if stage = `Working then
                     ENV.find name prog_env.data_items
                   else
                     ENV.find name prog_env.linkage_items
                 in
                 let new_name = if Option.is_none data_item.redefines then
                     Mangler.new_buf_name ()
                   else
                     Mangler.last_buf_name ()
                 in
                 let new_field = if Option.is_none data_item.redefines then
                     "f_"^(String.sub new_name 2 ((String.length new_name) - 2))
                   else
                     Mangler.new_field_name ()
                 in
                 match stage with
                 | `Working ->
                     let sub_items, dgl = List.fold_left_map (sub_items_mangling new_name) data_item.sub_items dgl in
                     let data_item = {data_item with c_buf = new_name; c_field = new_field; sub_items} in
                     let prog_env =
                       {prog_env with
                        data_items =
                          ENV.replace
                            (Option.get data_item.data_name)
                            data_item
                            prog_env.data_items
                       }
                     in
                     Group (name, dgl, dde), prog_env
                 | `Linkage ->
                     let sub_items, dgl = List.fold_left_map (sub_items_mangling new_name) data_item.sub_items dgl in
                     let data_item = {data_item with c_buf = new_name; c_field = new_field; sub_items} in
                     let prog_env =
                       {prog_env with
                        linkage_items =
                          ENV.replace
                            (Option.get data_item.data_name)
                            data_item
                            prog_env.linkage_items
                       }
                     in
                     Group (name, dgl, dde), prog_env
           in
           prog_env, dg)
           prog_env
           wss
       in
       wss, prog_env)
    }
  in
  GAst_trav.{fm_default with
   program_definition = (fun funs env pd ->
     let name = ~&(pd.program_id_paragraph.program_name) in
     let prog_env = ENV.find name env in
     let data_division, prog_env =
       (pd.data_division, prog_env)
       >>= GAst_trav.Data_div_traversal.fm_data_division
         {(data_div_traversal_funs `Working) with
          linkage_section = (data_div_traversal_funs `Linkage).working_storage_section}
     in
     let nested_env = prog_env.nested_progs in
     let nested_env, nested_programs =
       List.fold_left_map (fun nested_env nested_prog ->
       fm_program_definition funs nested_env ~&nested_prog
       |> Pair.map_fst ~f:(fun np -> np &@<- nested_prog)
       |> Pair.swap)
         nested_env
         pd.nested_programs
     in
     let env = ENV.replace name {prog_env with nested_progs = nested_env} env in
     {pd with data_division; nested_programs}, env
   );
  } *)
*)
