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

module NEL = Cobol_common.Basics.NEL

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

(* --- UTILS --- *)

let inv f a b = f b a

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

let pp_brackets pp =
  Fmt.(any "[" ++ pp ++ any "]")

let pp_pair pp_a pp_b = fun ppf (a, b) ->
    Fmt.pf ppf "(%a,%a)" pp_a a pp_b b

let pp_list pp = Fmt.list ~sep:(Fmt.any ";") pp

let rec pp_nel pp ppf = function
  | NEL.One v -> Fmt.pf ppf "One %a" pp v
  | v::nel -> Fmt.pf ppf "%a::" pp v; pp_nel pp ppf nel

let pp_match_cases ppf pp_key pp_value default l =
  List.iter (fun (keys, value) ->
    Fmt.pf ppf "  | %a -> %a\n"
      Fmt.(list ~sep:(any " | ") pp_key) keys
      pp_value value) l;
  Fmt.pf ppf "  | _ -> %s\n" default

(* --- *)

include MenhirSdk.Cmly_read.Read (struct let filename = cmlyname end)

type completion_entry =
  | K of terminal NEL.t
  | Custom of string
(* [@@deriving ord] *)

let completion_entry_equal entry1 entry2 =
  match entry1, entry2 with
  | Custom c1, Custom c2 -> c1 == c2
  | K nel1, K nel2 -> NEL.equal Terminal.equal nel1 nel2
  | _ -> false

let completion_entry_compare entry1 entry2 =
  match entry1, entry2 with
    | Custom s1, Custom s2 -> String.compare s2 s1
    | K nel1, K nel2 -> NEL.compare Terminal.compare nel1 nel2
    | Custom _, K _ -> -1
    | K _, Custom _ -> 1

let pp_completion_entry: completion_entry Fmt.t = fun ppf -> function
  | K list -> Fmt.pf ppf "K (%a)" (pp_nel Print.terminal) list
  | Custom custom_type -> Fmt.pf ppf "%s" custom_type


let terminal_condition: terminal -> bool = fun term ->
  Terminal.kind term == `REGULAR &&
  Terminal.attributes term |>
  List.exists (fun attrib ->
    Attribute.has_label "keyword" attrib ||
    Attribute.has_label "completion" attrib ||
    Attribute.has_label "keyword.combined" attrib)

let terminal_filter_map: terminal -> completion_entry option = fun term ->
  if terminal_condition term then Some (K (NEL.One term)) else None

let nonterminal_filter_map: nonterminal -> completion_entry option = fun nonterm ->
  Nonterminal.attributes nonterm |>
  List.find_opt (Attribute.has_label "completion") |>
  Option.map Attribute.payload |>
  fun s -> Option.bind s (fun s -> Some (Custom s))

(* --- *)

module SymbolSet = Set.Make(struct
  type t = symbol
  let compare = Symbol.compare
end)

module CompEntrySet = struct
  include Set.Make(struct
    type t = completion_entry
    let compare = completion_entry_compare
  end)
  let pp _ppf = ()
end

module Map5 (OrderedType : Map.OrderedType) = struct
  include Map.Make(OrderedType)

  let add_to_list x data m =
    let add = function None -> Some [data] | Some l -> Some (data :: l) in
    update x add m
end

module CompEntryMap = Map5(struct
  type t = CompEntrySet.t
  let compare = CompEntrySet.compare
end)

module DefaultNTMap = Map5(struct
  type t = (int * nonterminal) list
  let compare = List.compare (fun (i, nt) (i2, nt2) ->
    match i-i2 with 0 -> Nonterminal.compare nt nt2 | diff -> diff)
end)

(* --- *)

let completion_entry_t = "completion_entry"
let state_t = "state"
let nonterminal_t = "nonterminal"

let emit_pp_completion_entry ppf custom_types = (* For debug *)
  Fmt.pf ppf "\nlet pp_%s: %s Fmt.t = fun ppf -> function\n"
  completion_entry_t completion_entry_t;
  Fmt.pf ppf "  | K tokens -> Fmt.pf ppf \"%%a\" (Fmt.list ~sep:Fmt.sp Fmt.string) (List.map Grammar_printer.print_token @@@@ NEL.to_list tokens)\n";
  List.iter
  (fun s -> Fmt.pf ppf "  | %s -> Fmt.pf ppf \"%s\"\n" s s)
  custom_types

let emit_complentryset ppf custom_types =
  Fmt.(pf ppf {|
module CompEntrySet = Set.Make(struct
  type t = completion_entry
  let compare ce1 ce2 =
    let to_int = function
      %a| K _ -> %d in
    match ce1, ce2 with
      | K nel1, K nel2 -> NEL.compare Stdlib.compare nel2 nel1
      | _ -> to_int ce1 - to_int ce2
end)
|}
  (list ~sep:nop (fun ppf (i,t) ->
    pf ppf "| %s -> %d\n    " t i)) (List.mapi (fun i v -> (i,v)) custom_types)
  (List.length custom_types))

let emit_completion_entry ppf =
  Fmt.pf ppf "type %s =\n" completion_entry_t;
  Fmt.pf ppf "  | K of token NEL.t\n";
  let custom_types = Nonterminal.fold (fun nonterm acc ->
    match nonterminal_filter_map nonterm with
    | Some (Custom s) -> (Fmt.pf ppf "  | %s\n" s; s::acc)
    | _ -> acc) [] in
  emit_pp_completion_entry ppf custom_types;
  emit_complentryset ppf custom_types

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

include TYPES
|}
  state_t nonterminal_t;
  Fmt.cut ppf ();
  ()

let default_nonterminals lr1 =
  List.sort_uniq Stdlib.compare @@
  match Lr1.default_reduction lr1 with
  | Some prod -> [(Array.length @@ Production.rhs prod, Production.lhs prod)]
  | None -> begin
    let lr0 = Lr1.lr0 lr1 in
    List.fold_left (fun acc (prod, idx) ->
      let rhs = Production.rhs prod in
      if Array.length rhs == idx
      then (Array.length @@ Production.rhs prod, Production.lhs prod)::acc
      else match rhs.(idx) with
      | N nt, _, _ when Nonterminal.nullable nt -> (0, nt)::acc
      | _ -> acc)
    [] (Lr0.items lr0) end

let emit_default_nonterminals ppf =
  Fmt.pf ppf {|
(** For a given state, compute the list of every nonterminal that can be produced
  without consuming the input.
  @return (length of production, nonterminal produced) list *)
let default_nonterminals: %s -> (int * %s) list = fun state ->
  List.rev_map (fun (rhslen, lhs) -> (rhslen, nonterminal_of_int lhs)) @@@@
  match state_to_int state with
|} state_t nonterminal_t;
  Lr1.fold (fun lr1 acc ->
    match default_nonterminals lr1 with
    | [] -> acc
    | defaults -> DefaultNTMap.add_to_list defaults lr1 acc)
  DefaultNTMap.empty
  |> inv (DefaultNTMap.fold (fun s l acc -> (l, s)::acc)) []
  |> pp_match_cases ppf
    Fmt.(using Lr1.to_int int)
    Fmt.(pp_brackets @@ pp_list @@ pp_pair int (using Nonterminal.to_int int))
    "[]"

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
  end) []
  |> sort_and_merge Int.compare Int.equal
  |> pp_match_cases ppf
    Fmt.(pp_pair int int)
    Fmt.int
    "raise (Invalid_argument \"This state and nonterminal don't lead to any transition\")"


let completion_entries_of ~lr1 =
  let next_tokens_from ~item =
    let (prod, idx) = item in
    let rec eagerly_get_terminals rhs i l =
      match rhs.(i) with
        | T t, _, _ when terminal_condition t -> eagerly_get_terminals rhs (i+1) (t::l)
        | _ | exception Invalid_argument _ -> List.rev l
    in
      match (Production.rhs prod).(idx) with
        | N nt, _, _ -> List.filter_map terminal_filter_map (Nonterminal.first nt)
        | T _, _, _ -> begin
          match eagerly_get_terminals (Production.rhs prod) idx [] with
            | [] -> []
            | l -> [K (NEL.of_list l)] end
        | exception Invalid_argument _ -> []
  in
  Lr1.tabulate begin fun lr1 ->
    let custom_comp_entries = List.fold_left (fun acc -> function
      | N nonterm, _ -> Option.fold ~none:acc
        ~some:(inv CompEntrySet.add acc) (nonterminal_filter_map nonterm)
      | _ -> acc)
    CompEntrySet.empty (Lr1.transitions lr1) in
    List.fold_left (fun acc item ->
      CompEntrySet.(union acc (of_list @@ next_tokens_from ~item)))
    custom_comp_entries (Lr0.items @@ Lr1.lr0 lr1)
  end lr1

let emit_transition_tokens ppf = (* taking transitions *)
  Fmt.pf ppf {|
(** A list of the possible tokens accepted by the
  automaton in the given state *)
let transition_tokens: %s -> %s list = fun state ->
  NEL.(match state_to_int state with
|} state_t completion_entry_t;
  Lr1.fold (fun lr1 acc ->
    match completion_entries_of ~lr1 with
      | s when CompEntrySet.is_empty s -> acc
      | s -> CompEntryMap.add_to_list s lr1 acc)
  CompEntryMap.empty
  |> inv (CompEntryMap.fold (fun s l acc -> (l, CompEntrySet.elements s)::acc)) []
  |> pp_match_cases ppf
    Fmt.(using Lr1.to_int int)
    (pp_brackets @@ pp_list pp_completion_entry)
    "[])"

module DEBUG = struct
let emit_firsts ppf =
  Nonterminal.iter (fun nonterm ->
    let kind = if Nonterminal.kind nonterm  == `START then "start" else "" in
    let nullable = if Nonterminal.nullable nonterm then "nullable" else "" in
    Fmt.(
      pf ppf "-%a- %s %s\n\t[%a]\n\n"
      Print.nonterminal nonterm
      nullable kind
      (pp_list Print.terminal) (Nonterminal.first nonterm)))

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
    (pp_list Print.symbol) ppf sorted;
    if snd @@ List.fold_left (fun acc s ->
    match acc,s with
    | (true,_), N _ -> (true,true)
    | (false,_), N _ -> (true,false)
    | _ -> acc
    ) (false,false) sorted then begin
    (* if List.length sorted < List.length items then begin *)
      Fmt.string ppf "<-OO->" end;
  end)

let emit_productions ppf =
  let has_reducable_productions lr1: bool =
    match default_nonterminals lr1 with
      | [] -> false
      | _ -> true
  in
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
end

let () =
  let ppf = Fmt.stdout in
  Fmt.pf ppf
    "(* Caution: this file was automatically generated from %s; do not edit *)\
     \nmodule NEL = Cobol_common.Basics.NEL
     open Grammar_tokens@\n\n"
    cmlyname;
  emit_types ppf;
  emit_completion_entry ppf;
  emit_default_nonterminals ppf;
  emit_follow_transition ppf;
  emit_transition_tokens ppf;
  (* DEBUG.emit_productions ppf; *)
  (* DEBUG.emit_firsts ppf; *)
  (* DEBUG.emit_lr0 ppf; *)
  ()
