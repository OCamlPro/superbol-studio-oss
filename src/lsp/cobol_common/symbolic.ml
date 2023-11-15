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

module NEL = Basics.NEL                                    (* non-empty lists *)

module type SYMBOL = sig
  include Set.OrderedType
  val pp: t Pretty.printer
end

module type LINEXPR = sig
  type factor
  type linexpr
  type var
  type const
  exception NON_LINEAR of var NEL.t
  exception NOT_SCALAR of [ `Vars of var NEL.t | `Consts of const NEL.t ]
  val pp_factor: factor Pretty.printer
  val pp_linexpr: linexpr Pretty.printer
  val int: int -> factor
  val var: var -> factor
  val const: const -> linexpr
  val factor: factor -> linexpr
  val zero: linexpr
  val add: linexpr -> by:linexpr -> linexpr
  val sub: linexpr -> by:linexpr -> linexpr
  val mult: linexpr -> by:factor -> linexpr
  val as_int: linexpr -> int
end

module Linear_exprs (Var: SYMBOL) (Const: SYMBOL)
  : LINEXPR with type var := Var.t
             and type const := Const.t =
struct
  type var = Var.t
  type const = Const.t

  module Factor_group = struct
    type t =
      | N
      | S of Var.t
    [@@deriving ord]
  end

  module Factors = Map.Make (Factor_group)

  type factors = int Factors.t

  module Term_group = struct
    type t =
      | Unit
      | E of Const.t
    [@@deriving ord]
  end

  module Terms = Map.Make (Term_group)

  type factor = factors                                 (* private, singleton *)

  type linexpr = factors Terms.t                       (* set of linear terms *)

  exception NON_LINEAR of var NEL.t
  exception NOT_SCALAR of [ `Vars of var NEL.t | `Consts of const NEL.t ]

  (* --- *)

  let pp_sum pp_term ppf terms =
    Pretty.list ~fopen:"@[<hov 3>(+ " ~fsep:"@;" ~fclose:")@]" ~fempty:"0"
      pp_term ppf terms

  let pp_factors ?(in_sum = false) ppf (factors: int Factors.t) =
    let pp_binding ppf = function
      | Factor_group.N, n -> Fmt.int ppf n
      | Factor_group.S c, 1 -> Var.pp ppf c
      | Factor_group.S c, n -> Pretty.print ppf "(* %d %a)" n Var.pp c
    in
    match Factors.bindings factors with
    | [binding] ->
        pp_binding ppf binding
    | bl when in_sum ->
        Pretty.list ~fopen:"" ~fsep:"@;" ~fclose:"" ~fempty:"" pp_binding ppf bl
    | bl ->
        pp_sum pp_binding ppf bl

  let pp_factor ppf c = pp_factors ppf c

  let identity_factor: factors =
    Factors.singleton N 1

  let is_identity_factor (c: factors) : bool =
    Factors.compare Int.compare identity_factor c = 0

  (* let is_const_factor (c: factors) : bool = *)
  (*   Factors.cardinal c = 1 && fst (Factors.choose c) = N *)

  (* --- *)

  let pp_term ?in_sum : (Term_group.t * int Factors.t) Pretty.printer = fun ppf ->
    function
    | Unit, factors ->
        pp_factors ?in_sum ppf factors
    | E es, factors when is_identity_factor factors ->
        Const.pp ppf es
    | E es, factors ->
        Pretty.print ppf "@[<hov 3>(* %a@;%a)@]" (pp_factors ?in_sum) factors
          Const.pp es

  let pp_linexpr: linexpr Pretty.printer = fun ppf expr ->
    match Terms.bindings expr with
    | [term] ->
        pp_term ~in_sum:false ppf term
    | terms ->
        pp_sum (pp_term ~in_sum:true) ppf terms

  (* --- *)

  let int: int -> factor = function
    | 0 -> Factors.empty
    | i -> Factors.singleton N i

  let var: var -> factor = fun s -> Factors.singleton (S s) 1

  let factor: factor -> linexpr = Terms.singleton Unit

  let const: const -> linexpr = fun d -> Terms.singleton (E d) (int 1)

  (* --- *)

  let not_scalar_vars vars =
    raise @@ NOT_SCALAR (`Vars vars)

  let as_int_factor: factors -> int = fun c ->
    if Factors.cardinal c = 1
    then match Factors.choose c with
      | N, n -> n
      | S s, _ -> not_scalar_vars (NEL.One s)
    else
      let symbolic = function Factor_group.N, _ -> None | S a, _-> Some a in
      let symbols = Factors.bindings c |> List.filter_map symbolic in
      not_scalar_vars (NEL.of_rev_list @@ List.rev symbols)

  let not_scalar_consts consts =
    raise @@ NOT_SCALAR (`Consts consts)

  let as_int: linexpr -> int = fun l ->
    if Terms.cardinal l = 1
    then match Terms.choose l with
      | Unit, factors -> as_int_factor factors
      | E d, _ -> not_scalar_consts (NEL.One d)
    else
      let symbolic = function Term_group.Unit, _ -> None | E d, _-> Some d in
      let symbols = Terms.bindings l |> List.filter_map symbolic in
      not_scalar_consts (NEL.of_rev_list @@ List.rev symbols)

  (* --- *)

  let zero = Terms.empty

  let elim_zero = function
    | 0 -> None
    | i -> Some i

  let elim_empty factors =
    if Factors.is_empty factors
    then None
    else Some factors

  let add_factors: factors -> factors -> factors =
    Factors.union (fun _ a b -> elim_zero (a + b))
  let add_terms: linexpr -> by:linexpr -> linexpr = fun s ~by ->
    Terms.union (fun _ a b -> elim_empty (add_factors a b)) s by

  let sub_factors: factors -> factors -> factors =
    Factors.union (fun _ a b -> elim_zero (a - b))
  let sub_terms: linexpr -> by:linexpr -> linexpr = fun s ~by ->
    Terms.union (fun _ a b -> elim_empty (sub_factors a b)) s by

  let mult_factors_with_scalar: factors -> int -> factors = fun a n ->
    Factors.filter_map (fun _ a -> elim_zero (a * n)) a

  let mult_factors_with_var: factors -> var -> int -> factors = fun a s n ->
    let an = try Factors.find N a with Not_found -> 1 in
    let factors = Factors.remove N a in
    let factors' = Factors.add (S s) (an * n) factors in
    if Factors.is_empty factors
    then
      factors'
    else
      let symbolic = function Factor_group.N, _ -> None | S a, _-> Some a in
      let symbols = Factors.bindings factors' |> List.filter_map symbolic in
      raise @@ NON_LINEAR (NEL.of_rev_list @@ List.rev symbols)

  let mult_factors: linexpr -> by:factors -> linexpr = fun s ~by ->
    if Factors.is_empty by then zero else                           (* absorb *)
      Factors.fold begin fun g n ->
        Terms.filter_map begin fun _ a -> elim_empty (match g with
            | N -> mult_factors_with_scalar a n
            | S v -> mult_factors_with_var a v n)
        end
      end by s

  let add = add_terms
  let sub = sub_terms
  let mult = mult_factors

end
