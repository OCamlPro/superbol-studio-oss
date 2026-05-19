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

let name = ref ""

let usage () =
  Printf.eprintf "Usage: %s file.cmly\n" Sys.argv.(0);
  exit 1

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    if !name = ""
    then name := Sys.argv.(i)
    else usage ()
  done;
  if !name = ""
  then usage ()

(* --- *)

let status = ref 0

include MenhirSdk.Cmly_read.Read (struct let filename = !name end)

let pp_pos ppf r =
  let Lexing.({ pos_lnum = l1; pos_bol = b1; pos_cnum = c1; pos_fname; _ },
              { pos_lnum = l2; pos_bol = b2; pos_cnum = c2; _ })
    = Range.(startp r, endp r) in
  Fmt.pf ppf "%s:%a" pos_fname Fmt.text_loc ((l1, c1 - b1), (l2, c2 - b2))

let context attrs =
  List.find_opt (Attribute.has_label "context") attrs |>
  Option.map (fun a -> Attribute.payload a, Attribute.position a)

let nonterminal_context n : (string * Range.t) option =
  match Nonterminal.kind n with
  | `REGULAR -> context (Nonterminal.attributes n)
  | `START -> None

let emit_prelude ppf =
  List.iter begin fun a ->
    if Attribute.has_label "header" a ||
       Attribute.has_label "context.header" a then
      Format.fprintf ppf "%s\n" (Attribute.payload a)
  end Grammar.attributes

let emit_nonterminal_contexts ppf =
  Fmt.pf ppf "\
    let nonterminal_context: type k. k nonterminal -> _ option = function\n";
  Nonterminal.iter begin fun n -> match nonterminal_context n with
    | Some (s, pos) ->
        if Nonterminal.nullable n then begin
          Fmt.epr "%a:@\n\
                   @[<2>** Error:@ context@ `%s'@ on@ nullable@ non-terminal@]@.\
                  " pp_pos pos s;
          status := 1
        end;
        Fmt.pf ppf "  | N_%s -> Some %s\n" (Nonterminal.name n) (String.capitalize_ascii s)
    | None -> ()
  end;
  Fmt.pf ppf "\
    \  | _ -> None\n"

let pp_context ppf s =
  Fmt.string ppf (String.capitalize_ascii s)

let pp_contexts =
  Fmt.(list ~sep:(any ";@ ") pp_context)

let context_of_production =
  Production.tabulate begin fun prod ->
    Option.map fst @@ nonterminal_context (Production.lhs prod)
  end

let emit_contexts_mapping ppf =
  Fmt.pf ppf "\
    let contexts_for_state_num: int -> _ list = function\n";
  Lr1.iter begin fun s ->
    let contexts =
      List.filter_map begin fun (prod, i) ->
        if i == 1
        then context_of_production prod
        else None
      end (Lr0.items (Lr1.lr0 s))
    in
    match List.sort_uniq String.compare contexts with
    | [] -> ()                                                         (* skip *)
    | ctxs -> Fmt.pf ppf "  | %d -> [%a]\n" (Lr1.to_int s) pp_contexts ctxs
  end;
  Fmt.pf ppf "\
    \  | _ -> []\
    \n\
    \nlet contexts: type k. k lr1state -> _ list = fun s ->\
    \n  contexts_for_state_num (number s)\n"


module ISet = Set.Make (Int)
module SSet = Set.Make (String)
module SMap = Map.Make (String)


let contexts_at_state: lr1 -> nonterminal SMap.t =
  Lr1.tabulate begin fun s ->
    List.fold_left begin fun acc (prod, _) ->
      match context_of_production prod with
      | None -> acc
      | Some ctx -> SMap.add ctx (Production.lhs prod) acc
    end SMap.empty (Lr0.items (Lr1.lr0 s))
  end


let trivial_items items =
  try
    let nonterminals =
      List.fold_left begin fun acc (prod, _) ->
        let acc = ISet.add (Nonterminal.to_int @@ Production.lhs prod) acc in
        if ISet.cardinal acc > 1 then raise Exit;               (* early exit *)
        acc
      end ISet.empty items
    in
    ISet.cardinal nonterminals < 2
  with Exit -> false


let symbols_continuing_nonterminal nt state_items =
  List.fold_left begin fun ((ts, nts) as acc) (prod, idx) ->
    let lhs = Production.lhs prod
    and rhs = Production.rhs prod in
    match rhs.(idx) with
    | T t, _, _ when nt = lhs ->        (* non-terminal under interest *)
        ISet.add (Terminal.to_int t) ts,
        nts
    | N nt', _, _ when nt = lhs ||      (* non-terminal under interest *)
                       nt' = lhs ->     (* "looping" production *)
        List.fold_left (fun acc t -> ISet.add (Terminal.to_int t) acc) ts
          (Nonterminal.first nt'),
        ISet.add (Nonterminal.to_int nt') nts
    | T _, _, _
    | N _, _, _
    | exception Invalid_argument _ ->
        acc
  end (ISet.empty, ISet.empty) state_items



let states_leaving_nonterminal s nt =
  let lr0 = Lr1.lr0 s in
  let items = Lr0.items lr0 in
  let continuing_terminals,
      continuing_nonterminals = symbols_continuing_nonterminal nt items in
  List.filter_map begin function
    | T t, dest
      when not (ISet.mem (Terminal.to_int t) continuing_terminals) ->
        Some dest
    | N nt, dest
      when not (ISet.mem (Nonterminal.to_int nt) continuing_nonterminals) ->
        Some dest
    | _, _ ->
        None
  end (Lr1.transitions s)




let all_context_sinks_of_state: lr1 -> ISet.t SMap.t =
  Lr1.tabulate begin fun s ->
    let lr0 = Lr1.lr0 s in
    let items = Lr0.items lr0 in
    let all_contexts = contexts_at_state s in
    if SMap.is_empty all_contexts || trivial_items items
    then SMap.empty                                 (* no early exit possible *)
    else
      (* Fmt.epr "State: %d@." (Lr1.to_int s); *)
      (* Fmt.epr "%a" Print.itemset (Lr0.items (Lr1.lr0 s)); *)
      SMap.fold begin fun context nt acc ->
        match states_leaving_nonterminal s nt with
        | [] -> acc
        | l -> SMap.add context (ISet.of_list @@ List.rev_map Lr1.to_int l) acc
      end all_contexts SMap.empty
  end


let emit_contexts_sinks ppf =
  Fmt.pf ppf "\
    let context_sinks_on_shift': context -> int -> int -> bool = fun c s s' ->@\n\
   \  match c, s, s' with@\n";

  Lr1.iter begin fun s ->
    let s_num = Lr1.to_int s in
    SMap.iter begin fun context leaving_states ->
      Fmt.pf ppf "  | %a, %d, (@[%a@]) -> true@\n"
        pp_context context s_num
        Fmt.(list int ~sep:(any " |@ ")) (ISet.elements leaving_states)
    end (all_context_sinks_of_state s)
  end;

  Fmt.pf ppf "\
    \  | _ -> false\
    \n\
    \nlet context_sinks_on_shift: type k k'. context -> k lr1state -> k' lr1state -> \
          bool = fun c s s' ->\
    \n  context_sinks_on_shift' c (number s) (number s')\n"


let emit ppf =
  Fmt.pf ppf
    "(* Caution: this file was automatically generated from %s; do not edit *)\
     @\nopen %s\
     @\nopen MenhirInterpreter\
     @\n%t\
     @\n%t\
     @\n%t\
     @\n%t\
     @\n"
    !name
    (String.capitalize_ascii (Filename.basename Grammar.basename))
    emit_prelude
    emit_nonterminal_contexts
    emit_contexts_mapping
    emit_contexts_sinks

let () =
  emit Fmt.stdout;
  exit !status
