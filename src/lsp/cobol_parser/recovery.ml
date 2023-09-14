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

(* Note that's heavily inspired from merlin's own code for recovery *)

module Make
    (Parser: MenhirLib.IncrementalEngine.EVERYTHING)
    (Recovery: sig
       val default_value: 'a Parser.symbol -> 'a
       val token_of_terminal: 'a Parser.terminal -> 'a -> Parser.token
       val depth: int array

       type action =
         | Abort
         | R of int
         | S: 'a Parser.symbol -> action
         | Sub of action list
       and decision =
         | Nothing
         | One of action list
         | Select of (int -> action list)

       val recover: int -> decision

       val print_symbol: Parser.xsymbol -> string
       val print_token: Parser.token -> string
       val benign_assumption: Parser.token -> bool
     end) =
struct

  type 'a candidate =
    {
      env: 'a Parser.env;
      visited: 'a operation list;
      assumed: assumption list;
    }
  and 'a operation =
    | Env of 'a Parser.env
    | Prod of Parser.production
  and assumption =
    {
      show: Pretty.delayed option;
      pos: Lexing.position;
      benign: bool;
    }
  type 'a candidates =
    {
      final: ('a * assumption list) option;
      candidates: 'a candidate list;
    }

  module T = struct
    (* FIXME: this is a bit ugly. We should ask for the type to be exported
       publicly by MenhirLib. *)

    [@@@ocaml.warning "-37"]

    type 'a checkpoint =
      | InputNeeded of 'a Parser.env
      | Shifting of 'a Parser.env * 'a Parser.env * bool
      | AboutToReduce of 'a Parser.env * Parser.production
      | HandlingError of 'a Parser.env
      | Accepted of 'a
      | Rejected
    external inj : 'a checkpoint -> 'a Parser.checkpoint = "%identity"
  end

  let feed_token token visited env =
    let rec aux visited = function
      | Parser.HandlingError _ | Rejected -> `Fail
      | Accepted v -> `Accept v
      | Shifting (e, _, _) as c -> aux (Env e :: visited) (Parser.resume c)
      | AboutToReduce (_, p) as c -> aux (Prod p :: visited) (Parser.resume c)
      | InputNeeded env as c -> `Recovered (c, env, visited)
    in
    aux visited (Parser.offer (T.inj (T.InputNeeded env)) token)

  let candidate env = { env; visited = []; assumed = [] }

  let attempt r token =
    let rec aux = function
      | [] -> `Fail
      | x :: xs -> match feed_token token x.visited x.env with
        | `Fail ->
            aux xs
        | `Recovered (c, e, visited) ->
            `Ok (c, x.env, List.rev (Env e :: visited), List.rev x.assumed)
        | `Accept v ->
            match aux xs with
            | `Fail -> `Accept (v, List.rev x.assumed)
            | x -> x
    in
    aux r.candidates

  let decide env =
    let rec nth_state env n =
      if n = 0
      then match Parser.top env with
        | None -> -1                  (*allow giving up recovery on empty files*)
        | Some (Parser.Element (state, _, _, _)) -> Parser.number state
      else match Parser.pop env with
        | None -> assert (n = 1); -1
        | Some env -> nth_state env (n - 1)
    in
    let st = nth_state env 0 in
    match Recovery.recover st with
    | Nothing -> []
    | One actions -> actions
    | Select f -> f (nth_state env Recovery.depth.(st))

  let generate (type a) (env: a Parser.env) =
    let module E = struct
      exception Result of (a * assumption list)
    end in
    let eval ~endp path : Recovery.action -> a Parser.env * _ * _ =
      let rec aux ((env, visited, assumed) as path) = function
        | Recovery.Abort ->
            raise Not_found
        | Sub actions ->
            List.fold_left aux path actions
        | R prod ->
            let prod = Parser.find_production prod in
            Parser.force_reduction prod env,
            Prod prod :: visited,
            assumed
        | S (N _ as sym) ->
            let env =
              Parser.feed sym endp (Recovery.default_value sym) endp env
            and show = match Recovery.print_symbol @@ X sym with
              | "" -> None
              | sym_str -> Some (Pretty.delayed "%s" sym_str)
            in
            (* Here, we assume that a symbol that shows as an empty string
               denotes a non-terminal that may be empty and is therefore a
               benign assumption. *)
            let benign = show = None in
            env, Env env :: visited, { show; pos = endp; benign } :: assumed
        | S (T t as sym) ->
            let v = Recovery.default_value sym in
            let token = Recovery.token_of_terminal t v in
            match feed_token (token, endp, endp) visited env with
            | `Fail ->
                assert false
            | `Accept v ->
                raise (E.Result (v, assumed))
            | `Recovered (_, env, visited) ->
                let show = match Recovery.print_token token with
                  | "" -> None
                  | sym_str -> Some (Pretty.delayed "%s" sym_str)
                and benign = Recovery.benign_assumption token in
                env, visited, { show; pos = endp; benign } :: assumed
      in
      aux path
    in
    let rec aux acc ((env, _visited, _assumed) as path) =
      match Parser.top env with
      | None ->
          None, acc
      | Some (Element (_, _, _, endp)) ->
          match
            let actions = decide env in
            List.fold_left begin fun (path, acc) action ->
              let env, visited, assumed as path = eval ~endp path action in
              path, { env; visited; assumed } :: acc
            end (path, []) actions |> snd
          with
          | [] ->
              None, acc
          | ({ env; visited; assumed } :: _) as candidates ->
              aux (candidates @ acc) (env, visited, assumed)
          | exception Not_found ->
              None, acc
          | exception (E.Result v) ->
              Some v, acc
    in
    aux [] (env, [], [])

  let generate env =
    let final, candidates = generate env in
    let candidates =
      List.fold_left begin fun acc t ->
        if not (Parser.env_has_default_reduction t.env)
        then t :: acc
        else acc
      end [] candidates
    in
    { final; candidates = candidate env :: candidates }

end
