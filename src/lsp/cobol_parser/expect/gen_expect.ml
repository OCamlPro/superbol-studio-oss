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
let nel_module = ref ""
let extra_default = ref ""

let usage_msg = Fmt.str "%s [OPTIONS] file.cmly" Sys.argv.(0)
let anon str = match !cmlyname with
  | None -> cmlyname := Some str
  | Some _ -> raise @@ Arg.Bad "Only one anonymous argument may be given"

let () =
  Arg.parse
    Arg.[
      ("--external-tokens", Set_string external_tokens,
       "<module> Import token type definition from <module>");
      ("--nel-module", Set_string nel_module,
       "<module> Import NEL (non-empty-list) type definition from <module>");
      ("--extra-default-attribute-name", Set_string extra_default,
       "<string> The annotation representing extra default values");
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

let rec pp_nel_constr pp ppf = function
  | NEL.One v -> Fmt.pf ppf "One %a" pp v
  | v::nel -> Fmt.pf ppf "%a::" pp v; pp_nel_constr pp ppf nel

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
  | K list -> Fmt.pf ppf "K (%a)" (pp_nel_constr Print.terminal) list
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
  Option.map (fun attr -> Custom (Attribute.payload attr))

type dry_action =
  | Nt of nonterminal
  | Prod of production

let pp_action ppf = function
  | Nt nt -> Fmt.pf ppf "X N_%s" (Nonterminal.mangled_name nt)
  | Prod p  -> Fmt.pf ppf "P %a" Fmt.(using Production.to_int int) p

(* --- *)

module StringSet = Set.Make(String)

module ActionSet = Set.Make(struct
    type t = dry_action
    let compare = fun a1 a2 ->
      match a1, a2 with
      | Prod p1, Prod p2 -> Production.compare p1 p2
      | Nt nt1, Nt nt2 -> Nonterminal.compare nt1 nt2
      | Nt _, Prod _ -> -1
      | Prod _, Nt _ -> 1
  end)

module CompEntrySet = struct
  include Set.Make(struct
      type t = completion_entry
      let compare entry1 entry2 =
        match entry1, entry2 with
        | Custom s1, Custom s2 -> String.compare s2 s1
        | K nel1, K nel2 -> NEL.compare Terminal.compare nel1 nel2
        | Custom _, K _ -> -1
        | K _, Custom _ -> 1
    end)
end

module Make_map (OrderedType : Map.OrderedType) = struct
  include Map.Make(OrderedType)

  let add_to_list x data m =
    let add = function None -> Some [data] | Some l -> Some (data :: l) in
    update x add m
end

module Compentries_mapping = Make_map(struct
    type t = CompEntrySet.t
    let compare = CompEntrySet.compare
  end)

module Action_mapping = Make_map(struct
    type t = ActionSet.t
    let compare = ActionSet.compare
  end)

(* --- *)

let emit_completion_entry ppf =
  Fmt.string ppf {|
module Completion_entry = struct
  type t =
    | K of token NEL.t
|};
  let custom_types = StringSet.elements @@
    Nonterminal.fold (fun nonterm acc ->
        match nonterminal_filter_map nonterm with
        | Some (Custom s) -> ( StringSet.add s acc)
        | _ -> acc) StringSet.empty in
  Fmt.pf ppf {|%a

  let compare ce1 ce2 =
    let to_int = function
      %a| K _ -> %d in
    match ce1, ce2 with
      | K nel1, K nel2 -> NEL.compare Stdlib.compare nel2 nel1
      | _ -> to_int ce1 - to_int ce2
|}
    Fmt.(list ~sep:nop (fun ppf t -> pf ppf "    | %s\n" t))
    custom_types
    Fmt.(list ~sep:nop (fun ppf (i, t) ->
        pf ppf "| %s -> %d\n      " t i))
    (List.mapi (fun i t -> (i, t)) custom_types)
    (List.length custom_types);
  Fmt.pf ppf {|
  let pp: t Fmt.t = fun ppf -> function
     | K tokens -> Fmt.(pf ppf "%%a"
         (NEL.pp ~fopen:"" ~fclose:"" ~fsep:" "
         (using Grammar_printer.print_token string)) tokens)
|};
  List.iter
    (fun s -> Fmt.pf ppf "    | %s -> Fmt.string ppf \"%s\"\n" s s)
    custom_types;
  Fmt.string ppf "end\n"

let action_in: lr1 -> ActionSet.t =
  Lr1.tabulate begin fun lr1 ->
    List.to_seq (Lr0.items @@ Lr1.lr0 lr1)
    |> Seq.filter_map (fun (prod, idx) ->
        let rhs = Production.rhs prod in
        if Array.length rhs <= idx
        then Some (Prod prod)
        else
          match rhs.(idx) with
          | N nt, _, _
            when Nonterminal.nullable nt -> Some (Nt nt)
          | _ -> None)
    |> ActionSet.of_seq
  end

let emit_action_in ppf =
  Fmt.string ppf {|
type action =
  | Feed: 'a Menhir.nonterminal -> action
  | Reduce: Menhir.production -> action

type dry_action =
  | X: 'a Menhir.nonterminal -> dry_action
  | P: int -> dry_action

let actions_in ~env : action list =
  List.rev_map (function
      | P prod_idx -> Reduce (Menhir.find_production prod_idx)
      | X nt -> Feed nt)
  @@
  match Menhir.current_state_number env with
|};
  Lr1.fold (fun lr1 acc ->
      match action_in lr1 with
      | empty when ActionSet.is_empty empty -> acc
      | actions -> Action_mapping.add_to_list actions lr1 acc)
    Action_mapping.empty
  |> inv (Action_mapping.fold (fun s l acc -> (l, ActionSet.elements s)::acc)) []
  |> pp_match_cases ppf
    Fmt.(using Lr1.to_int int)
    (pp_brackets @@ pp_list @@ pp_action)
    "[]"

let accumulate_terminals: lr1 -> CompEntrySet.t =
  let rec continue_while_terminal rhs i l =
    match rhs.(i) with
    | T t, _, _
      when is_valid_for_comp t ->
      continue_while_terminal rhs (i+1) (t::l)
    | _
    | exception Invalid_argument _ ->
      List.rev l
  in
  let next_tokens_from_item item =
    let (prod, idx) = item in
    let rhs = Production.rhs prod in
    match rhs.(idx) with
    | N nt, _, _ -> Seq.filter_map terminal_filter_map (List.to_seq @@ Nonterminal.first nt)
    | T _, _, _ -> begin
        match continue_while_terminal rhs idx [] with
        | [] -> Seq.empty
        | l -> Seq.return @@ K (NEL.of_list l) end
    | exception Invalid_argument _ -> Seq.empty
  in
  Lr1.tabulate begin fun lr1 ->
    Seq.append
      (List.to_seq (Lr1.transitions lr1)
       |> Seq.filter_map begin function
         | N nonterm, _ -> nonterminal_filter_map nonterm
         | _ -> None end)
      ((List.to_seq @@ Lr0.items @@ Lr1.lr0 lr1)
       |> Seq.flat_map next_tokens_from_item)
    |> CompEntrySet.of_seq
  end

let emit_completion_entries_in_env ppf =
  Fmt.string ppf {|
let completion_entries_in ~env : Completion_entry.t list =
  NEL.(match Menhir.current_state_number env with
|};
  Lr1.fold (fun lr1 acc ->
      match accumulate_terminals lr1 with
      | s when CompEntrySet.is_empty s -> acc
      | s -> Compentries_mapping.add_to_list s lr1 acc)
    Compentries_mapping.empty
  |> inv (Compentries_mapping.fold (fun s l acc -> (l, CompEntrySet.elements s)::acc)) []
  |> pp_match_cases ppf
    Fmt.(using Lr1.to_int int)
    (pp_brackets @@ pp_list pp_completion_entry)
    "[])"

type pre_comp =
  | O of terminal (* optional *)
  | M of terminal (* mandatory *)
  | UserDefinedWords of string
  | Endrule

module PreCompSet = struct
  include Set.Make(struct
      type t = pre_comp
      let compare pc1 pc2 =
        let to_int = function
          | O _ -> 0
          | M _ -> 1
          | UserDefinedWords _ -> 2
          | Endrule -> 3
        in
        match pc1, pc2 with
        | O t1, O t2
        | M t1, M t2 -> Terminal.compare t1 t2
        | UserDefinedWords s1, UserDefinedWords s2 -> String.compare s1 s2
        | _ -> to_int pc1 - to_int pc2
    end)
end

let nonterminal_pre_comp_filter_map: nonterminal -> pre_comp option = fun nonterm ->
  Nonterminal.attributes nonterm |>
  List.find_opt (Attribute.has_label "completion") |>
  Option.map (fun attr -> UserDefinedWords (Attribute.payload attr))

let rec listpop l n =
  if n==0
  then l
  else match l with
    | [] -> []
    | _::tl -> listpop tl (n-1)

let follow_transition: lr1 -> symbol -> lr1 option =
  Lr1.tabulate begin fun lr1 symbol ->
    List.find_map begin fun (s, next) ->
      if Symbol.equal s symbol
        then Some next
        else None end
    (Lr1.transitions lr1)
    end

let reduction_from_terminal: lr1 -> terminal -> production option =
  Lr1.tabulate begin fun lr1 ->
    Terminal.tabulate begin fun terminal ->
      List.find_map begin fun (t, prod) ->
        if Terminal.equal t terminal
        then Some prod
        else None end
        (Lr1.get_reductions lr1)
    end
  end

let reduce_prod (prod: production) (stack: lr1 NEL.t) : lr1 NEL.t option =
  let depth = Array.length @@ Production.rhs prod in
  let produced = Production.lhs prod in
  match listpop (NEL.to_list stack) depth with
  | [] -> None
  | red_lr1::_ as popped ->
    follow_transition red_lr1 (N produced)
    |> Option.map (fun new_lr1 -> NEL.of_list (new_lr1::popped))

let accumulate_precomp: lr1 -> PreCompSet.t =
  let rec nullable_firsts ?(first_call=false) lr1 item =
    let (prod, idx) = item in
    let rhs = Production.rhs prod in
    match rhs.(idx) with
    | N nt, _, _
      when Nonterminal.nullable nt &&
           first_call ->
      Seq.append
        (Seq.map (fun t -> O t) (List.to_seq @@ Nonterminal.first nt))
        (nullable_firsts lr1 (prod, idx+1))
    | N nt, _, _
      when Nonterminal.nullable nt &&
           not first_call ->
      nullable_firsts lr1 (prod, idx+1)
    | N nt, _, _ ->
      Seq.map (fun t -> M t) (List.to_seq @@ Nonterminal.first nt)
    | T t, _, _ ->
      Seq.return @@ M t
    | exception Invalid_argument _ ->
      Seq.return @@ Endrule
  in
  Lr1.tabulate begin fun lr1 ->
    Seq.append
      (List.to_seq (Lr1.transitions lr1)
       |> Seq.filter_map begin function
         | N nonterm, _ -> nonterminal_pre_comp_filter_map nonterm
         | T _, _ -> None end)
      ((List.to_seq @@ Lr0.items @@ Lr1.lr0 lr1)
       |> Seq.flat_map (nullable_firsts ~first_call:true lr1))
    |> PreCompSet.of_seq
  end

let next_single_mandatory_terminal (stack: _ NEL.t) =
  let lr1 = NEL.hd stack in
  let comp_set = accumulate_precomp lr1 in
  let cardinal = PreCompSet.cardinal comp_set in
  match PreCompSet.choose_opt comp_set with
  | Some M term
    when is_valid_for_comp term && cardinal == 1 ->
    Some (term)
  | _ -> None

let eager_entries_of: lr1 -> CompEntrySet.t =
  let rec eager_next_tokens: lr1 NEL.t -> terminal -> terminal NEL.t =
    fun stack current_term ->
      let lr1 = NEL.hd stack in
      match
        Lr1.default_reduction lr1,
        follow_transition lr1 (T current_term),
        reduction_from_terminal lr1 current_term
      with
      | _, _, Some prod
      | Some prod, _, _ -> begin
          match reduce_prod prod stack with
          | None -> NEL.One current_term
          | Some reduced_stack ->
            eager_next_tokens reduced_stack current_term
        end
      | _, Some new_lr1, _ ->
        eager_next_tokens_consummed NEL.(new_lr1::stack) current_term
      | _ ->
        Pretty.failwith
          "Lr1 %d cannot be followed by terminal %s, originated from lr1 %d"
          (Lr1.to_int lr1)
          (Terminal.name current_term)
          (Lr1.to_int @@ NEL.last stack)
  and eager_next_tokens_consummed: lr1 NEL.t -> terminal -> terminal NEL.t =
    fun stack consummed_term ->
      let current_nel = NEL.One consummed_term in
      let lr1 = NEL.hd stack in
      match Lr1.default_reduction lr1 with
      | Some prod -> begin
          match reduce_prod prod stack with
          | None -> current_nel
          | Some reduced_stack ->
            eager_next_tokens_consummed reduced_stack consummed_term
        end
      | None ->
        match next_single_mandatory_terminal stack with
        | None -> current_nel
        | Some next_term ->
          consummed_term::(eager_next_tokens stack next_term)
  in
  Lr1.tabulate
    begin fun lr1 ->
      let pre_comp_set = accumulate_precomp lr1 in
      let stack = NEL.One lr1 in
      PreCompSet.to_seq pre_comp_set |>
      Seq.filter_map begin function
        | O term | M term
          when is_valid_for_comp term ->
          Some (K (eager_next_tokens stack term))
        | UserDefinedWords s -> Some (Custom s)
        | Endrule | O _ | M _ -> None
      end
      |> CompEntrySet.of_seq
    end

let emit_eager_completion_entries_in_env ppf =
  Fmt.string ppf {|
let eager_completion_entries_in ~env : Completion_entry.t list =
  NEL.(match Menhir.current_state_number env with
|};
  Lr1.fold (fun lr1 acc ->
      match eager_entries_of lr1 with
      | s when CompEntrySet.is_empty s -> acc
      | s -> Compentries_mapping.add_to_list s lr1 acc)
    Compentries_mapping.empty
  |> inv (Compentries_mapping.fold
            (fun s l acc -> (l, CompEntrySet.elements s)::acc)) []
  |> pp_match_cases ppf
    Fmt.(using Lr1.to_int int)
    (pp_brackets @@ pp_list pp_completion_entry)
    "[])"

let guess_default_value typ =
  let optionRegexp = Str.regexp "^[^*]+ option$" in
  let listRegexp = Str.regexp "^[^*]+ list$" in
  if Str.string_match optionRegexp typ 0
  then Some "None"
  else if Str.string_match listRegexp typ 0
  then Some "[]"
  else if String.equal typ "unit"
  then Some "()"
  else if String.equal typ "bool"
  then Some "false"
  else None

let extra_default = match !extra_default with
| "" -> None
| s -> Some s

let best_guess_default_value = Nonterminal.tabulate begin fun nt ->
    if not (Nonterminal.nullable nt)
    then None
    else
      let given_default =
        Nonterminal.attributes nt
        |> List.find_opt begin fun attr ->
          Attribute.has_label "default" attr ||
          Option.fold ~none:false ~some:(inv Attribute.has_label attr) extra_default
        end
        |> Option.map Attribute.payload in
      match given_default with
      | Some _ -> given_default
      | None ->
        Option.bind (Nonterminal.typ nt) guess_default_value
  end

let emit_default_nonterminal_value ppf =
  Fmt.string ppf {|
let default_nonterminal_value (type a): a Menhir.nonterminal -> a = function
  (* If this function does not compile, it has probably incorrecly
     guessed the type of a nonterinal, either change the name
     or add a @default attribute on the faulty nonterminal *)
|};
  Nonterminal.fold begin fun nt acc ->
    match best_guess_default_value nt with
    | None -> acc
    | Some default -> ([nt], default)::acc
  end []
  |> pp_match_cases ppf
    Fmt.(using Nonterminal.mangled_name (any "N_" ++ string))
    Fmt.string
    "raise Not_found"

let __ensure_all_nullable_nonterminal_have_default =
  let nt_without_default = Nonterminal.fold (fun nt acc -> begin
        if Nonterminal.nullable nt
        && best_guess_default_value nt == None
        then nt::acc else acc
      end) [] in
  match List.hd nt_without_default with
  | nt ->
      Pretty.failwith
        "Unable to find a default value for nonterminal '%s'; \
        please provide a @default annotation"
        (Nonterminal.mangled_name nt)
  | exception Failure _ -> ()

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

  let emit_productions_with_num ppf =
    Production.iter begin fun production ->
      Fmt.(
        pf ppf "-%d- %a\n" (Production.to_int production)
          Print.production production)
    end

  let emit_state_productions ppf =
    Lr1.iter begin fun lr1 ->
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
      let is_in_prods = fun prod ->
        Option.is_some @@ List.find_opt (Production.equal prod) (List.map fst items) in
      Fmt.pf ppf "REDUCTIONS [%a]\n"
        Fmt.(list ~sep:(any "; ")
               (fun ppf (t, p) ->
                  Print.terminal ppf t;
                  if not @@ is_in_prods p then (Fmt.string ppf " "; Print.production ppf p)))
        (Lr1.get_reductions lr1) end

  let emit_nullable_unrecoverable ppf =
    let nt_without_default = Nonterminal.fold (fun nt acc -> begin
          if Nonterminal.nullable nt
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
     \nmodule NEL = %s\
     \nmodule Menhir = Grammar.MenhirInterpreter\
     \nopen %s@\n\n"
    cmlyname !nel_module !external_tokens;

  emit_completion_entry ppf;
  emit_action_in ppf;
  emit_completion_entries_in_env ppf;
  emit_eager_completion_entries_in_env ppf;
  emit_default_nonterminal_value ppf;

  (* DEBUG.emit_firsts ppf; *)
  (* DEBUG.emit_state_productions ppf; *)
  ()
