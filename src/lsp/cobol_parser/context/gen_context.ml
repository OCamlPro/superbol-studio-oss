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
        if Nonterminal.nullable n then
          Fmt.epr "%a:@\n\
                   @[<2>** Warning:@ context@ `%s'@ on@ nullable@ \
                   non-terminal@]@." pp_pos pos s;
        Fmt.pf ppf "  | N_%s -> Some %s\n" (Nonterminal.name n) (String.capitalize_ascii s)
    | None -> ()
  end;
  Fmt.pf ppf "\
    \  | _ -> None\n"

let pp_contexts =
  Fmt.(list ~sep:(any ";@ ") (fun ppf s -> pf ppf "%s" (String.capitalize_ascii s)))

let emit_contexts_mapping ppf =
  Fmt.pf ppf "\
    let contexts_for_state_num: int -> _ list = function\n";
  Lr1.iter begin fun s ->
    let contexts =
      List.filter_map begin fun (prod, i) ->
        if i == 1
        then Option.map fst @@ nonterminal_context (Production.lhs prod)
        else None
      end (Lr0.items (Lr1.lr0 s))
    in
    match List.sort_uniq (String.compare) contexts with
    | [] -> ()                                                         (* skip *)
    | ctxs -> Fmt.pf ppf "  | %d -> [%a]\n" (Lr1.to_int s) pp_contexts ctxs
  end;
  Fmt.pf ppf "\
    \  | _ -> []\
    \n\
    \nlet contexts: type k. k lr1state -> _ list = fun s ->\
    \n  contexts_for_state_num (number s)\n"

let emit ppf =
  Fmt.pf ppf
    "(* Caution: this file was automatically generated from %s; do not edit *)\
     @\nopen %s\
     @\nopen MenhirInterpreter\
     @\n%t\
     @\n%t\
     @\n%t\
     @\n"
    !name
    (String.capitalize_ascii (Filename.basename Grammar.basename))
    emit_prelude
    emit_nonterminal_contexts
    emit_contexts_mapping

let () =
  emit Fmt.stdout
