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

let cmlyname = ref None

let usage_msg = Fmt.str "%s [OPTIONS] file.cmly" Sys.argv.(0)
let anon str = match !cmlyname with
  | None -> cmlyname := Some str
  | Some _ -> raise @@ Arg.Bad "Only one anonymous argument may be given"

let () = Arg.parse [] anon usage_msg

let cmlyname = match !cmlyname with
  | None | Some "" -> Fmt.epr "%s@." usage_msg; exit 1
  | Some s -> s

(* --- *)

let menhir = "MenhirInterpreter"

include MenhirSdk.Cmly_read.Read (struct let filename = cmlyname end)

(* --- *)

let tidyup =
  let spaces = Str.regexp "[ \t\n\r]+" in
  Str.global_replace spaces " "

(* --- *)

module StrMap = Map.Make (Stdlib.String)

type post_action_repr =
  {
    tag: string;
    fun_type: string;
  }

let post_actions =
  let post_action_types =
    List.filter_map begin fun a ->
      if Attribute.has_label "post.tag" a then
        let payload = tidyup (Attribute.payload a) in
        let first_space = String.index payload ' ' in
        let typename = String.sub payload 0 first_space
        and typespec = String.(sub payload (succ first_space)
                                 (length payload - first_space - 1)) in
        Some (typename, typespec)
      else
        None
    end Grammar.attributes
  in
  List.to_seq post_action_types |>
  Seq.map (fun (k, fun_type) -> "post." ^ k, { tag = "Post_" ^ k; fun_type }) |>
  StrMap.of_seq

let post_action attrs =
  let open Attribute in
  List.find_opt (fun a -> StrMap.mem (label a) post_actions) attrs |>
  Option.map (fun a -> StrMap.find (label a) post_actions,
                       try payload a with Not_found -> "Fun.id")

(* --- *)

let pp_functor_parameters =
  Fmt.(list (fmt "(%s)"))

let pp_extension_module ppf pp_struct =
  let all_parameters =
    let parameters =
      List.filter (Attribute.has_label "post.parameter") Grammar.attributes |>
      List.map Attribute.payload
    in
    Grammar.parameters @ parameters
  in
  if all_parameters <> [] then
    Fmt.pf ppf
      "@\n@[<2>module@ Make@ %a =@]\
       @\n@[<2>struct@;%t@]\
       @\nend"
      pp_functor_parameters all_parameters
      pp_struct
  else
    pp_struct ppf

let pp_grammar_open ppf =
  let grammar_module =
    String.capitalize_ascii (Filename.basename Grammar.basename)
  and grammar_params =
    List.map (fun p -> List.hd (String.split_on_char ':' p)) Grammar.parameters
  in
  if grammar_params = [] then
    Fmt.pf ppf "@\n@[<2>open@ %s@]" grammar_module
  else
    Fmt.pf ppf "@\n@[<2>open@ %s.Make@ %a@]" grammar_module
      pp_functor_parameters grammar_params;
  Fmt.pf ppf "@\nopen MenhirInterpreter"

let pp_header ppf =
  List.iter begin fun a ->
    if Attribute.has_label "header" a ||
       Attribute.has_label "post.header" a then
      Fmt.pf ppf "@\n%s" (Attribute.payload a)
  end Grammar.attributes

let pp_post_type ppf =
  Fmt.pf ppf "@[<2>type post_action =";
  StrMap.iter begin fun _ { tag; fun_type } ->
    Fmt.pf ppf "@\n|@[<2> %s: (@[%a@]) -> post_action@]" tag Fmt.text fun_type
  end post_actions;
  Fmt.pf ppf "@\n| NoPost: post_action@]"

let pp_production_posts ppf =
  let last_item_attrs p =
    let rhs = Production.rhs p in
    try
      let _, _, last_item_attrs = rhs.(Array.length rhs - 1) in
      last_item_attrs
    with Invalid_argument _ -> []
  in
  Fmt.pf ppf
    "@\n@[<2>let post_production_num\
     @\n: type k. int -> k env -> post_action = fun prod_num env ->\
     @\nmatch top env with\
     @\n| None -> NoPost\
     @\n@[<4>| Some (Element (state, value, _, _)) ->\
     @\nmatch incoming_symbol state, prod_num with";
  Production.iter begin fun p ->
    let pp_post_action ppf ({ tag; _ }, func) = match tidyup func with
      | "" -> Fmt.pf ppf "%s value" tag
      | func -> Fmt.pf ppf "%s (@[<2>(%a)@ value@])" tag Fmt.text func
    in
    match post_action (last_item_attrs p),
          post_action (Production.attributes p) with
    | Some a, _ | _, Some a ->
        Fmt.pf ppf "@\n|@[<4> N N_%a, %d ->@;%a@]"
          Print.mangled_nonterminal (Production.lhs p) (Production.to_int p)
          pp_post_action a
    | None, None -> ()
  end;
  Fmt.pf ppf
    "@\n| _ -> NoPost@]@]\
     @\n\
     @\n@[<2>let post_production\
     @\n: type k. production -> k env -> post_action = fun p ->\
     @\npost_production_num (production_index p)@]"

let emit ppf =
  Fmt.pf ppf
    "(* Caution: this file was automatically generated from %s; do not edit *)\
     @\n[@@@@@@warning \"-33\"] (* <- do not warn on unused opens *)\
     @\n[@@@@@@warning \"-27\"] (* <- do not warn on unused variabes *)\
     @\n" cmlyname;
  Fmt.pf ppf "%a@\n" pp_extension_module begin fun ppf ->
    Fmt.pf ppf "%t@\n" pp_grammar_open;
    Fmt.pf ppf "%t@\n" pp_header;
    Fmt.pf ppf "%t@\n" pp_post_type;
    Fmt.pf ppf "%t@\n" pp_production_posts;
  end

let () =
  emit Fmt.stdout
