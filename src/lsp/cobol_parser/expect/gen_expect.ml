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
let external_tokens = ref ""

let usage_msg = Fmt.str "%s [OPTIONS] file.cmly" Sys.argv.(0)
let anon str = match !cmlyname with
  | None -> cmlyname := Some str
  | Some _ -> raise @@ Arg.Bad "Only one anonymous argument may be given"

let () =
  Arg.parse
    Arg.[
      ("--external-tokens", Set_string external_tokens,
       "<module> Import token type definition from <module>");
    ]
    anon usage_msg

let cmlyname = match !cmlyname with
  | None | Some "" -> Fmt.epr "%s@." usage_msg; exit 1
  | Some s -> s

(* --- *)

include MenhirSdk.Cmly_read.Read (struct let filename = cmlyname end)

type completion_entry =
  | K of terminal
  | Custom of string

let completion_entry_equal entry1 entry2 =
  match entry1, entry2 with
  | K t1, K t2 -> t1 == t2
  | Custom c1, Custom c2 -> c1 == c2
  | _ -> false

let completion_entry_compare entry1 entry2 =
  match entry1, entry2 with
    | K t1, K t2 -> Terminal.to_int t1 - Terminal.to_int t2
    | K _, Custom _ -> 1
    | Custom _, K _ -> -1
    | Custom s1, Custom s2 -> String.compare s2 s1

let pp_completion_entry: completion_entry Fmt.t = fun ppf -> function
  | K term -> Fmt.pf ppf "K %a" Print.terminal term
  | Custom custom_type -> Fmt.pf ppf "%s" custom_type

let terminal_filter_map: terminal -> completion_entry option = fun term ->
  if Terminal.kind term == `REGULAR &&
  Terminal.attributes term |>
  List.exists (fun attrib ->
    Attribute.has_label "keyword" attrib ||
    Attribute.has_label "keyword.combined" attrib)
  then
    Some (K term)
  else None

let nonterminal_filter_map: nonterminal -> completion_entry option = fun nonterm ->
  Nonterminal.attributes nonterm |>
  List.find_opt (Attribute.has_label "completion") |>
  Option.map Attribute.payload |>
  fun s -> Option.bind s (fun s -> Some (Custom s))

let completion_type_name = "completion_entry"

let emit_pp_completion_entry ppf custom_types = (* For debug *)
  Fmt.pf ppf "\nlet pp_%s: %s Fmt.t = fun ppf -> function\n"
  completion_type_name completion_type_name;
  Fmt.pf ppf "  | K token -> Fmt.pf ppf \"%%s\" @@@@ Grammar_printer.print_token token\n";
  List.iter
  (fun s -> Fmt.pf ppf "  | %s -> Fmt.pf ppf \"%s\"\n" s s)
  custom_types;
  Fmt.pf ppf "\n"

let emit_completion_entry ppf =
  Fmt.pf ppf "type %s =\n" completion_type_name;
  Fmt.pf ppf "  | K of Grammar_tokens.token\n";
  let custom_types = Nonterminal.fold (fun nonterm acc ->
    match nonterminal_filter_map nonterm with
    | Some (Custom s) -> (Fmt.pf ppf "  | %s\n" s; s::acc)
    | _ -> acc
  ) [] in
  Fmt.pf ppf "\n";
  emit_pp_completion_entry ppf custom_types

(* SHOULD BE REMOVED BEFORE MERGE
let emit_productions ppf =
  Fmt.pf ppf "(*";
  Lr1.iter (fun lr1 -> begin
    let keywords = Lr1.reductions lr1 in
    let keywords2 = List.map fst (Lr1.transitions lr1) in
    Fmt.pf ppf "  | %d\t ->red [%a]\n"
    (Lr1.to_int lr1)
    (Fmt.list ~sep:(Fmt.any ";") (fun ppf (t, pl) ->
      Fmt.pf ppf "{%a:%a}"
      Print.terminal t
      (Fmt.list ~sep:(Fmt.any "--") Print.production)
      pl)) keywords;
    Fmt.pf ppf "  | %d\t ->tra [%a]\n"
    (Lr1.to_int lr1)
    (Fmt.list ~sep:(Fmt.any ";") Print.symbol) keywords2
      end);
      Fmt.pf ppf "*)\n"

let emit_next_symbol_of_state ppf =
  Fmt.pf ppf "\nlet next_symbol_of_state: int -> %s list = Grammar_tokens.(function\n" completion_type_name;
  Lr1.iter (fun lr1 -> begin
    let keywords = List.map fst (Lr1.transitions lr1) in
      match keywords with
        | [] -> Fmt.pf ppf "  | %d\t -> []\n" (Lr1.to_int lr1)
        | lr1transition ->
            Fmt.pf ppf "  | %d\t -> [%a]\n"
            (Lr1.to_int lr1)
            (Fmt.list ~sep:(Fmt.any ";") Print.symbol) lr1transition
      end);
      Fmt.pf ppf "  | _ -> [])\n"

let emit_next_symbol_of_state_test ppf =
  Fmt.pf ppf "\nlet next_symbol_of_state: int -> %s list = Grammar_tokens.(function\n" completion_type_name;
  Lr1.iter (fun lr1 -> begin
    let keywords = List.flatten @@ List.map (fun (s, _) ->
        match s with
          (* | T term -> if terminal_keyword_filter term then [term] else [] *)
          | T term -> [term]
          | N nonterm -> Nonterminal.first nonterm
    ) (Lr1.transitions lr1) in
      match keywords with
        | [] -> ()
        | lr1transition ->
            Fmt.pf ppf "  | %d\t -> [%a]\n"
            (Lr1.to_int lr1)
            (Fmt.list ~sep:(Fmt.any ";") Print.terminal) lr1transition
      end);
      Fmt.pf ppf "  | _ -> [])\n"
*)

let emit_next_symbol_of_state_sorted ppf =
  Fmt.pf ppf "\nlet next_symbol_of_state: int -> _ list \
    = Grammar_tokens.(function\n";
  Lr1.fold (fun lr1 acc -> begin
    let comp_entries = List.filter_map (fun (s, _) ->
      match s with
        | T term -> terminal_filter_map term
        | N nonterm -> nonterminal_filter_map nonterm) (Lr1.transitions lr1) in
    let comp_entries =
      List.fold_left (fun acc (term, _) ->
        (Option.to_list @@ terminal_filter_map term) @ acc)
      comp_entries (Lr1.reductions lr1)
    in
    if comp_entries == []
    then acc
    else let sorted_comp_entries =
      List.sort_uniq completion_entry_compare comp_entries in
    (lr1, sorted_comp_entries)::acc
  end) []
        |> (* Finds all state with equal token list and merge them *)
  List.sort (fun (_, terms1) (_, terms2) ->
    List.compare completion_entry_compare terms1 terms2)
        |>
  List.fold_left (fun acc (lr1, comp_entries) -> begin
    match acc with
      | (prev_lr1s, prev_comp_entries)::t
        when List.equal completion_entry_equal comp_entries prev_comp_entries ->
          (lr1::prev_lr1s, prev_comp_entries)::t
      | _ -> ([lr1], comp_entries)::acc
  end) []
        |>
  List.iter (fun (states, tokens) ->
    Fmt.(pf ppf "  | %a ->\n    [%a]\n"
            (list ~sep:(any " | ") (using Lr1.to_int int)) states
            (list ~sep:(any ";") pp_completion_entry) tokens));
  Fmt.pf ppf "  | _ -> [])\n"

let emit ppf =
  Fmt.pf ppf
    "(* Caution: this file was automatically generated from %s; do not edit *)\
     @\n"
    cmlyname;
    emit_completion_entry ppf;
    emit_next_symbol_of_state_sorted ppf

let () =
  emit Fmt.stdout
