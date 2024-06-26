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
(* [@@deriving ord] *)

let completion_entry_equal entry1 entry2 =
  match entry1, entry2 with
  | K t1, K t2 -> t1 == t2
  | Custom c1, Custom c2 -> c1 == c2
  | _ -> false

let completion_entry_compare entry1 entry2 =
  match entry1, entry2 with
    | K t1, K t2 -> Terminal.compare t2 t1
    | Custom s1, Custom s2 -> String.compare s2 s1
    | Custom _, K _ -> -1
    | K _, Custom _ -> 1

let pp_completion_entry: completion_entry Fmt.t = fun ppf -> function
  | K term -> Fmt.pf ppf "K %a" Print.terminal term
  | Custom custom_type -> Fmt.pf ppf "%s" custom_type

let terminal_filter_map: terminal -> completion_entry option = fun term ->
  if Terminal.kind term == `REGULAR &&
  Terminal.attributes term |>
  List.exists (fun attrib ->
    Attribute.has_label "keyword" attrib ||
    Attribute.has_label "completion" attrib ||
    Attribute.has_label "keyword.combined" attrib)
  then
    Some (K term)
  else None

let nonterminal_filter_map: nonterminal -> completion_entry option = fun nonterm ->
  Nonterminal.attributes nonterm |>
  List.find_opt (Attribute.has_label "completion") |>
  Option.map Attribute.payload |>
  fun s -> Option.bind s (fun s -> Some (Custom s))

let completion_entry_t = "completion_entry"
let state_t = "state"
let nonterminal_t = "nonterminal"

let emit_pp_completion_entry ppf custom_types = (* For debug *)
  Fmt.pf ppf "\nlet pp_%s: %s Fmt.t = fun ppf -> function\n"
  completion_entry_t completion_entry_t;
  Fmt.pf ppf "  | K token -> Fmt.pf ppf \"%%s\" @@@@ Grammar_printer.print_token token\n";
  List.iter
  (fun s -> Fmt.pf ppf "  | %s -> Fmt.pf ppf \"%s\"\n" s s)
  custom_types

let emit_completion_entry ppf =
  Fmt.pf ppf "type %s =\n" completion_entry_t;
  Fmt.pf ppf "  | K of token\n";
  let custom_types = Nonterminal.fold (fun nonterm acc ->
    match nonterminal_filter_map nonterm with
    | Some (Custom s) -> (Fmt.pf ppf "  | %s\n" s; s::acc)
    | _ -> acc
  ) [] in
  emit_pp_completion_entry ppf custom_types

let emit_types ppf =
  Fmt.pf ppf {|
module type TYPES = sig
  type %s = private int
  val state_to_int: state -> int
  val state_of_int: int -> state
  type %s = private int
  val nonterminal_to_int: nonterminal -> int
  val nonterminal_of_int: int -> nonterminal
end

module TYPES: TYPES = struct
  type state = int
  let state_to_int state = state
  let state_of_int state = state
  type nonterminal = int
  let nonterminal_to_int nt = nt
  let nonterminal_of_int nt = nt
end

include TYPES|}
  state_t nonterminal_t;
  Fmt.cut ppf ();
  Fmt.cut ppf ();
  (* Fmt.pf ppf "let %s_of_int (state:int): %s = state \n\n" state_type_name state_type_name; *)
  ()

(** Find all keys that have the same value and merge them into a single key_list entry *)
let sort_and_merge compare equal l = l |>
  List.sort (fun (_, value1) (_, value2) ->
    compare value1 value2)
        |>
  List.fold_left (fun acc (key, value) -> begin
    match acc with
      | (prev_keys, prev_value)::t
        when equal value prev_value ->
          (key::prev_keys, prev_value)::t
      | _ -> ([key], value)::acc
  end) []

(* SHOULD BE REMOVED BEFORE MERGE *)
let emit_firsts ppf =
  Nonterminal.iter (fun nonterm ->
    let kind = if Nonterminal.kind nonterm  == `START then "start" else "" in
    let nullable = if Nonterminal.nullable nonterm then "nullable" else "" in
    Fmt.(
      pf ppf "-%a- %s %s\n\t[%a]\n\n"
      Print.nonterminal nonterm
      nullable kind
      (list ~sep:(any ";") Print.terminal) (Nonterminal.first nonterm)))

  (* SHOULD BE REMOVED BEFORE MERGE *)
let emit_lr0 ppf =
  Lr1.iter (fun lr1 -> begin
    let lr0 = Lr1.lr0 lr1 in
    let items = Lr0.items lr0 in
    Fmt.pf Fmt.stdout "\nState %d -%d-:\n" (Lr0.to_int lr0) (Lr1.to_int lr1);
    Print.itemset Fmt.stdout items;
    let items = List.filter_map (fun (prod,idx) ->
      let rhs = Production.rhs prod in
      try let (symbol, _,_) = rhs.(idx) in
      Some symbol
      with _ -> None
      ) items in
    let sorted = List.sort_uniq Symbol.compare items in
    (Fmt.list ~sep:(Fmt.any ";") Print.symbol) ppf sorted;
    if snd @@ List.fold_left (fun acc s ->
    match acc,s with
    | (true,_), N _ -> (true,true)
    | (false,_), N _ -> (true,false)
    | _ -> acc
    ) (false,false) sorted then begin
    (* if List.length sorted < List.length items then begin *)
      Fmt.string ppf "<-OO->" end;
  end)

let default_nonterminals lr1 =
  List.sort_uniq Stdlib.compare @@
  match Lr1.default_reduction lr1 with
  | Some prod -> [(Array.length @@ Production.rhs prod, Nonterminal.to_int @@ Production.lhs prod)]
  | None -> begin
    let lr0 = Lr1.lr0 lr1 in
    List.fold_left (fun acc (prod, idx) ->
      let rhs = Production.rhs prod in
      if Array.length rhs == idx
      then (Array.length @@ Production.rhs prod, Nonterminal.to_int @@ Production.lhs prod)::acc
      else match rhs.(idx) with
      | N nt, _, _ when Nonterminal.nullable nt -> (0, Nonterminal.to_int nt)::acc
      | _ -> acc
      | exception _ -> acc)
    [] (Lr0.items lr0) end

let has_reducable_productions lr1: bool =
  (* SHOULD BE REMOVED BEFORE MERGE *)
  match default_nonterminals lr1 with
  | [] -> false
  | _ -> true

  (* SHOULD BE REMOVED BEFORE MERGE *)
let emit_productions ppf =
  Fmt.pf ppf "(*";
  Lr1.iter (fun lr1 -> begin
    if not @@ has_reducable_productions lr1 then () else
    let lr0 = Lr1.lr0 lr1 in
    let items = Lr0.items lr0 in
    if not @@ List.fold_left (fun acc (prod,idx) ->
      let rhs = Production.rhs prod in
      match rhs.(idx) with
      | T _,_,_ -> acc
      | N nt,_,_ -> Nonterminal.nullable nt || acc
      | exception _ -> acc
    ) false items then () else (
    Fmt.pf Fmt.stdout "State %d -%d-: %a" (Lr0.to_int lr0) (Lr1.to_int lr1)
    (Fmt.option ~none:Fmt.cut Print.production) (Lr1.default_reduction lr1);
    Print.itemset Fmt.stdout items;
    Fmt.string ppf "\n")
    (* let items = List.filter_map (fun (prod,idx) -> *)
    (*   try let rhs = Production.rhs prod in *)
    (*   let (symbol, _,_) = rhs.(idx) in *)
    (*   Some symbol *)
    (* with _ -> None *)
    (*   ) items in *)
    (* let from_red = Lr1.reductions lr1 in *)
    (* let from_tra = Lr1.transitions lr1 in *)
    (* Fmt.pf ppf "  | %d\t ->tra [%a]\n" *)
    (*   (Lr1.to_int lr1) *)
    (*   (Fmt.list ~sep:(Fmt.any ";") (fun ppf (s,i) -> *)
    (*     Fmt.pf ppf "%a(%d)" Print.symbol s (Lr1.to_int i))) from_tra; *)
    (* Fmt.pf ppf "==> %a\n" (Fmt.list ~sep:(Fmt.any ";") Print.symbol) (List.sort_uniq Symbol.compare items); *)
    (*   Fmt.pf ppf "  | %d\t ->red [%a]\n" *)
    (* (Lr1.to_int lr1) *)
    (* (Fmt.list ~sep:(Fmt.any ";") (fun ppf (t,pl) -> *)
    (*   Fmt.pf ppf "%a <> %a" Print.terminal t (Fmt.list Print.production) pl)) from_red; *)
end);
      Fmt.pf ppf "*)\n"

let emit_get_default_nonterminal_produced ppf =
  Fmt.pf ppf {|
(** For a given state, compute the list of every nonterminal that can be produced
  without consuming the input.
  @return (length of production, nonterminal produced) list
  @raise Invalid_argument if the state has no default *)
let get_default_nonterminal_produced: %s -> (int * %s) list = fun state ->
  List.map (fun (rhslen, lhs) -> (rhslen, nonterminal_of_int lhs))
  begin match state_to_int state with
|} state_t nonterminal_t;
  Lr1.fold (fun lr1 acc -> begin
    match default_nonterminals lr1 with
    | [] -> acc
    | defaults -> (Lr1.to_int lr1, defaults)::acc
  end) [] |>
  sort_and_merge (List.compare Stdlib.compare) (List.equal Stdlib.(=)) |>
  List.iter (fun (states,lhs) ->
    Fmt.(pf ppf "  | %a -> [%a]\n"
          (list ~sep:(any " | ") Fmt.int) states
          (list ~sep:(any ";") (fun ppf (len,lhs) -> pf ppf "(%d,%d)" len lhs))
          lhs));
  Fmt.pf ppf "  | _ -> raise (Invalid_argument \"This state doesn't have any default production\") end\n"

let emit_follow_transition ppf =
  Fmt.pf ppf {|
(** For a given state and nonterminal,
  returns the state the automaton will be in after taking the transition *)
let follow_transition: %s -> %s -> %s = fun state nt ->
  let state = state_to_int state in
  let nt = nonterminal_to_int nt in
  state_of_int @@@@ match state, nt with
|} state_t nonterminal_t state_t;
  Lr1.fold (fun lr1 acc -> begin
    List.fold_left (fun acc (symbol, next_state) ->
      match symbol with
      | T _ -> acc
      | N nt -> ((Lr1.to_int lr1, Nonterminal.to_int nt),
          Lr1.to_int next_state)::acc
      ) acc (Lr1.transitions lr1)
  end) [] |>
  sort_and_merge Int.compare Int.equal
      |>
  List.iter (fun (state_nt, next_state) ->
    Fmt.(pf ppf "  | %a -> %d\n"
          (list ~sep:(any " | ") (fun ppf (s, nt) -> Fmt.pf ppf "(%d,%d)" s nt)) state_nt
          next_state));
  Fmt.pf ppf "  | _ -> raise (Invalid_argument \"This state and nonterminal don't lead to any transition\")\n"

let emit_transition_tokens ppf = (* taking transitions *)
  Fmt.pf ppf {|
(** A list of the possible tokens accepted by the
  automaton in the given state *)
let transition_tokens: %s -> %s list = fun state ->
  match state_to_int state with
|} state_t completion_entry_t;
  Lr1.fold (fun lr1 acc -> begin
    let custom_comp_entries = List.filter_map (function
      | N nonterm, _ -> nonterminal_filter_map nonterm
      | _ -> None
    ) (Lr1.transitions lr1) in
    let token_comp_entries = List.fold_left (fun acc (prod, idx) ->
      match (Production.rhs prod).(idx) with
      | N nt, _, _ -> (List.filter_map terminal_filter_map (Nonterminal.first nt)) @ acc
      | T t, _, _ -> (Option.to_list @@ terminal_filter_map t) @ acc
      | exception _ -> acc
    ) [] (Lr0.items @@ Lr1.lr0 lr1) in
    let comp_entries = custom_comp_entries @ token_comp_entries in
    if comp_entries == [] then acc
    else (lr1, List.sort_uniq completion_entry_compare comp_entries)::acc
  end) [] |>
  sort_and_merge
    (List.compare completion_entry_compare)
    (List.equal completion_entry_equal)
          |>
  List.iter (fun (states, comp_entries) ->
    Fmt.(pf ppf "  | %a ->\n    [%a]\n"
            (list ~sep:(any " | ") (using Lr1.to_int int)) states
            (list ~sep:(any ";") pp_completion_entry) comp_entries));
  Fmt.pf ppf "  | _ -> []\n"

let emit ppf =
  Fmt.pf ppf
    "(* Caution: this file was automatically generated from %s; do not edit *)\
     @\nopen Grammar_tokens@\n"
    cmlyname;
    emit_types ppf;
    emit_completion_entry ppf;
    emit_get_default_nonterminal_produced ppf;
    emit_follow_transition ppf;
    emit_transition_tokens ppf;
    (* emit_productions ppf; *)
    (* emit_firsts ppf; *)
    (* emit_lr0 ppf; *)
    (* Terminal.iter (fun term -> Print.terminal ppf term; Fmt.string ppf ";") *)
    ()

let () =
  emit Fmt.stdout
