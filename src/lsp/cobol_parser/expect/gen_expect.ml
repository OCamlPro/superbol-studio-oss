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

let pp_completion_entry: completion_entry Fmt.t = fun ppf -> function
  | K list -> Fmt.pf ppf "K (%a)" (pp_nel Print.terminal) list
  | Custom custom_type -> Fmt.pf ppf "%s" custom_type

let is_valid_for_comp: terminal -> bool = fun term ->
  Terminal.kind term == `REGULAR &&
  Terminal.attributes term |>
  List.exists (fun attrib ->
    Attribute.has_label "keyword" attrib ||
    Attribute.has_label "completion" attrib ||
    Attribute.has_label "keyword.combined" attrib)

let terminal_filter_map: terminal -> completion_entry option = fun term ->
  if is_valid_for_comp term then Some (K (NEL.One term)) else None

let nonterminal_filter_map: nonterminal -> completion_entry option = fun nonterm ->
  Nonterminal.attributes nonterm |>
  List.find_opt (Attribute.has_label "completion") |>
  Option.map Attribute.payload |>
  inv Option.bind (fun s -> Some (Custom s))

let pp_xsymbol_of_nt ppf nonterminal =
  Fmt.pf ppf "X(N N_%s)" (Nonterminal.mangled_name nonterminal)

(* --- *)

module SuperSet (OrderedType : Set.OrderedType) = struct
  include Set.Make(OrderedType)

  let add_option elt t =
    match elt with
    | None -> t
    | Some elt -> add elt t

  let add_list l t = add_seq (List.to_seq l) t

  let from_list_map ?(init=empty) ~add f l =
    List.fold_left (fun acc value -> add (f value) acc) init l
end

module SymbolSet = SuperSet(struct
  type t = symbol
  let compare = Symbol.compare
end)

module CompEntrySet = struct
  include SuperSet(struct
    type t = completion_entry
    let compare entry1 entry2 =
      match entry1, entry2 with
        | Custom s1, Custom s2 -> String.compare s2 s1
        | K nel1, K nel2 -> NEL.compare Terminal.compare nel1 nel2
        | Custom _, K _ -> -1
        | K _, Custom _ -> 1
  end)
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

module ProdMap = Map5(struct
  type t = Production.t list
  let compare = List.compare (fun p1 p2 ->
    Production.to_int p1 - Production.to_int p2)
end)

module SymbolMap = Map5(struct
  type t = SymbolSet.t
  let compare = SymbolSet.compare
end)

(* --- *)

let emit_pp_completion_entry ppf custom_types = (* For debug *)
  Fmt.pf ppf "\nlet pp_completion_entry: completion_entry Fmt.t = fun ppf -> function\n";
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
  Fmt.pf ppf "type completion_entry =\n";
  Fmt.pf ppf "  | K of token NEL.t\n";
  let custom_types = Nonterminal.fold (fun nonterm acc ->
    match nonterminal_filter_map nonterm with
    | Some (Custom s) -> (Fmt.pf ppf "  | %s\n" s; s::acc)
    | _ -> acc) [] in
  emit_pp_completion_entry ppf custom_types;
  emit_complentryset ppf custom_types

let nullable_nonterminals = Lr1.tabulate begin fun lr1 ->
  SymbolSet.from_list_map ~add:SymbolSet.add_option
  (fun (prod, idx) ->
    match (Production.rhs prod).(idx) with
      | N nt, _, _ when Nonterminal.nullable nt -> Some (N nt)
      | _ | exception Invalid_argument _ -> None)
    (Lr0.items @@ Lr1.lr0 lr1)
  end

let reducable_productions = Lr1.tabulate begin fun lr1 ->
  List.filter_map (function
    | (prod, idx) when Array.length (Production.rhs prod) == idx ->
        Some prod
    | _ -> None)
  (Lr0.items @@ Lr1.lr0 lr1)
  |> List.sort_uniq Production.compare
end

let emit_accaptable_nullable_nonterminals_in_env ppf =
  Fmt.pf ppf {|
let accaptable_nullable_nonterminals_in ~env : Menhir.xsymbol list =
  Menhir.(match current_state_number env with
|};
  Lr1.fold (fun lr1 acc ->
    match nullable_nonterminals lr1 with
    | emtpy when SymbolSet.is_empty emtpy -> acc
    | symbols -> SymbolMap.add_to_list symbols lr1 acc)
  SymbolMap.empty
  |> inv (SymbolMap.fold (fun s l acc -> (l,
      List.map (function  N nt -> nt | T _ -> failwith "impossible" ) @@ SymbolSet.elements s)::acc)) []
  |> pp_match_cases ppf
    Fmt.(using Lr1.to_int int)
    (pp_brackets @@ pp_list @@ pp_xsymbol_of_nt)
    "[])"

let emit_reducable_productions_in_env ppf =
  Fmt.pf ppf {|
let reducable_productions_in ~env : Menhir.production list =
  List.rev_map Menhir.find_production @@@@
  match Menhir.current_state_number env with
|};
  Lr1.fold (fun lr1 acc ->
    match reducable_productions lr1 with
    | [] -> acc
    | defaults -> ProdMap.add_to_list defaults lr1 acc)
  ProdMap.empty
  |> inv (ProdMap.fold (fun s l acc -> (l, s)::acc)) []
  |> pp_match_cases ppf
    Fmt.(using Lr1.to_int int)
    Fmt.(pp_brackets @@ pp_list @@ (using Production.to_int int))
    "[]"

let completion_entries_of =
  let rec continue_while_terminal rhs i l =
    match rhs.(i) with
        | T t, _, _ when is_valid_for_comp t ->
            continue_while_terminal rhs (i+1) (t::l)
        | _ | exception Invalid_argument _ -> List.rev l
  in
  let next_tokens_from_item item =
    let (prod, idx) = item in
    let rhs = Production.rhs prod in
    match rhs.(idx) with
      | N nt, _, _ -> List.filter_map terminal_filter_map (Nonterminal.first nt)
      | T _, _, _ -> begin
        match continue_while_terminal rhs idx [] with
          | [] -> []
          | l -> [K (NEL.of_list l)] end
      | exception Invalid_argument _ -> []
  in
  Lr1.tabulate begin fun lr1 ->
    let custom_comp_entries = CompEntrySet.from_list_map
      ~add:CompEntrySet.add_option
      begin function
        | N nonterm, _ -> nonterminal_filter_map nonterm
        | _ -> None end
      (Lr1.transitions lr1) in
    CompEntrySet.from_list_map
      ~init:custom_comp_entries ~add:CompEntrySet.add_list
      next_tokens_from_item (Lr0.items @@ Lr1.lr0 lr1)
  end

let emit_acceptable_terminals_in_env ppf = (* taking transitions *)
  Fmt.pf ppf {|
let acceptable_terminals_in ~env : completion_entry list =
  NEL.(match Menhir.current_state_number env with
|};
  Lr1.fold (fun lr1 acc ->
    match completion_entries_of lr1 with
      | s when CompEntrySet.is_empty s -> acc
      | s -> CompEntryMap.add_to_list s lr1 acc)
  CompEntryMap.empty
  |> inv (CompEntryMap.fold (fun s l acc -> (l, CompEntrySet.elements s)::acc)) []
  |> pp_match_cases ppf
    Fmt.(using Lr1.to_int int)
    (pp_brackets @@ pp_list pp_completion_entry)
    "[])"

let guess_default_value ~mangled_name ~typ =
  match typ, mangled_name with
  | _ when EzString.ends_with ~suffix:"option" typ ||
          EzString.starts_with ~prefix:"option_" mangled_name
  -> Some "None"
  | _ when EzString.ends_with ~suffix:"list" typ ||
  EzString.starts_with ~prefix:"loption_" mangled_name ||
  EzString.starts_with ~prefix:"list_" mangled_name
  -> Some "[]"
  | _ when EzString.ends_with ~suffix:"unit" typ
  -> Some "()"
  | _ when EzString.ends_with ~suffix:"bool" typ ||
  EzString.starts_with ~prefix:"boption_" mangled_name
  -> Some "false"
  | _ -> None

let is_nullable_without_recovery = Nonterminal.tabulate begin fun nt ->
  Nonterminal.nullable nt &&
    Nonterminal.attributes nt
    |> List.find_opt (Attribute.has_label "recovery")
    |> Option.is_none
  end

let best_guess_default_value = Nonterminal.tabulate begin fun nt ->
  if not (is_nullable_without_recovery nt)
  then None
  else
  let mangled_name = Nonterminal.mangled_name nt in
  let guessed_default = Option.bind
    (Nonterminal.typ nt)
    (fun typ -> guess_default_value ~mangled_name ~typ) in
  let given_default = Nonterminal.attributes nt
    |> List.find_opt (Attribute.has_label "default")
    |> Option.map Attribute.payload in
  match given_default with
    | Some _ -> given_default
    | None -> guessed_default
  end

let emit_default_value_of_nullables ppf =
  Fmt.pf ppf {|
let guessed_default_value_of_nullables (type a): a Menhir.symbol -> a = function
  (* If this function does not compile it probably means you're missing a
     [@default] attribute on a nullable nonterminal in the grammar *)
  | T _ -> raise Not_found
  | N nt -> begin match nt with
|};
  Nonterminal.fold (fun nt acc -> begin
    match best_guess_default_value nt with
      | None -> acc
      | Some default -> ([nt], default)::acc
  end) []
  |> pp_match_cases ppf
    Fmt.(using Nonterminal.mangled_name (any "N_" ++ string))
    Fmt.string
    "raise Not_found end"

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

let emit_state_productions ppf =
  Lr1.iter (fun lr1 -> begin
    let lr0 = Lr1.lr0 lr1 in
    let items = Lr0.items lr0 in
    Fmt.pf ppf "\nState lr1-%d- lr0(%d):\n%a"
      (Lr1.to_int lr1) (Lr0.to_int lr0)
      Fmt.(option ~none:nop (any "DEFAULT-PROD: " ++ Print.production))
      (Lr1.default_reduction lr1);
    Print.itemset ppf items;
    Fmt.pf ppf "TRANSITIONS [%a]\n"
      (Fmt.list ~sep:(Fmt.any "; ") (fun ppf (s, i) ->
        Fmt.pf ppf "%a(%d)" Print.symbol s (Lr1.to_int i))) (Lr1.transitions lr1);
    Fmt.pf ppf "REDUCTIONS [%a]\n"
      Fmt.(list ~sep:(any "; ") (using fst Print.terminal)) (Lr1.get_reductions lr1);
  end)

let emit_temp ppf =
  Lr1.iter (fun lr1 -> begin
    let lr0 = Lr1.lr0 lr1 in
    let items = Lr0.items lr0 in
    let r1 = reducable_productions lr1 in
    let r2 = reducable_productions lr1 in
    if List.equal Production.equal r1 r2 then () else begin
    Fmt.pf ppf "\nState lr1-%d- lr0(%d):\n%a"
      (Lr1.to_int lr1) (Lr0.to_int lr0)
      Fmt.(option ~none:nop (any "DEFAULT-PROD: " ++ Print.production))
      (Lr1.default_reduction lr1);
    Print.itemset ppf items;
    Fmt.string ppf "R1:\n";
    Print.itemset ppf (List.map (fun p -> (p, 0)) r1);
    Fmt.string ppf "R2:\n";
    Print.itemset ppf (List.map (fun p -> (p, 0)) r2) end
  end)

let emit_nullable_unrecoverable ppf =
  let nt_without_default = Nonterminal.fold (fun nt acc -> begin
    if is_nullable_without_recovery nt
    && best_guess_default_value nt == None
    then nt::acc else acc
  end) [] in
  if nt_without_default == []
  then Fmt.string ppf "All nullable nonterminals have a recovery/default/guessed-default"
  else List.iter (fun nt ->
    Fmt.pf ppf "%s  (%s)\n"
      (Nonterminal.mangled_name nt)
      (Option.value ~default:"no typ" @@ Nonterminal.typ nt))
  nt_without_default

end


let () =
  let ppf = Fmt.stdout in
  Fmt.pf ppf
    "(* Caution: this file was automatically generated from %s; do not edit *)\
     \nmodule NEL = Cobol_common.Basics.NEL\
     \nmodule Menhir = Grammar.MenhirInterpreter\
     \nopen Grammar_tokens@\n\n"
    cmlyname;

  emit_completion_entry ppf;
  emit_reducable_productions_in_env ppf;
  emit_accaptable_nullable_nonterminals_in_env ppf;
  emit_acceptable_terminals_in_env ppf;
  emit_default_value_of_nullables ppf;

  (* emit_follow_transition ppf; *)
  (* DEBUG.emit_firsts ppf; *)
  (* DEBUG.emit_state_productions ppf; *)
  (* DEBUG.emit_nullable_unrecoverable ppf; *)
  ()
