module Expression : sig
  type precedence = int

  type unary_op

  val pp_unary_op : unary_op Fmt.t

  val prefix : prec:precedence -> unit Fmt.t -> unary_op

  val postfix : prec:precedence -> unit Fmt.t -> unary_op

  type binary_op

  val pp_binary_op : binary_op Fmt.t

  val infix : prec:precedence -> unit Fmt.t -> binary_op

  val infixl : prec:precedence -> unit Fmt.t -> binary_op

  val infixr : prec:precedence -> unit Fmt.t -> binary_op

  type t

  val pp : t Fmt.t

  val atom : 'a Fmt.t -> 'a -> t

  val unary : unary_op -> t -> t

  val binary : t -> binary_op -> t -> t
end = struct
  type precedence = int

  type associativity = Left | Right | Nonassociative

  type _ fixity =
    | Prefix : [ `unary ] fixity
    | Postfix : [ `unary ] fixity
    | Infix : associativity -> [ `binary ] fixity

  let eq_fixity (type a b) (lhs : a fixity) (rhs : b fixity) =
    match lhs, rhs with
    | Prefix, Prefix -> true
    | Prefix, _ | _, Prefix -> false
    | Postfix, Postfix -> true
    | Postfix, _ | _, Postfix -> false
    | Infix a, Infix a' -> a = a'

  type _ op =
    | Nop : [ `atom ] op
    | Op : { name : unit Fmt.t ; prec : precedence ; fixity : 'a fixity } -> 'a op

  let infixl ~prec name = Op { name; prec; fixity = Infix Left }
  let infixr ~prec name = Op { name; prec; fixity = Infix Right }
  let infix ~prec name = Op { name; prec; fixity = Infix Nonassociative }
  let prefix ~prec name = Op { name; prec; fixity = Prefix }
  let postfix ~prec name = Op { name; prec; fixity = Postfix }

  type unary_op = [ `unary ] op

  let pp_unary_op ppf (Op { name; _ } : unary_op) = name ppf ()

  type binary_op = [ `binary ] op

  let pp_binary_op ppf (Op { name; _ } : binary_op) = name ppf ()

  type any_op = AnyOp : _ op -> any_op [@@unboxed]

  type t = any_op * unit Fmt.t

  let noparens (type ai ao) (inner : ai op) (outer : ao op) side =
    match inner, outer with
    (* Invariant: there is an outer operator *)
    | Op _, Nop -> assert false
    (* Inner operator has higher precedence: no parentheses needed *)
    | Nop, _ -> true
    | Op { prec = pi; _ }, Op { prec = po; _ } when pi > po -> true
    (* Inner operator has lower or equal precedence: maybe add parentheses *)
    | Op { prec = pi; fixity = fi; _ }, Op { prec = po; fixity = fo; _ } ->
      match fi, side with
      (* These are never ambiguous *)
      | Postfix, Left | Prefix, Right -> true
      (* Prefix and postfix operators can always nest *)
      | _, Nonassociative -> eq_fixity fi fo
      (* Parentheses not needed for same precedence and associativity *)
      | Infix Left, Left -> pi = po && eq_fixity fo (Infix Left)
      | Infix Right, Right -> pi = po && eq_fixity fo (Infix Right)
      | _ -> false

  let bracket (AnyOp inner, pp) side outer =
    if noparens inner outer side then pp else Fmt.parens pp

  let atom pp e = AnyOp Nop, Fmt.const pp e

  let unary (Op { name; fixity; _ } as op : unary_op) e =
    let pp = bracket e Nonassociative op in
    AnyOp op,
    match fixity with
    | Prefix -> Fmt.(name ++ sp ++ pp)
    | Postfix -> Fmt.(pp ++ sp ++ name)

  let binary lhs (Op { name; _ } as op : binary_op) rhs =
    let lhs = bracket lhs Left op
    and rhs = bracket rhs Right op in
    AnyOp op, Fmt.(lhs ++ any " " ++ name ++ sp ++ rhs)

  let pp ppf (_, pp) = pp ppf ()
end