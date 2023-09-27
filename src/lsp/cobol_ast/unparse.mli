module Expression : sig
  type precedence = int
  (** Operator precedence is represented using integers. Operators with a higher
      precedence value bind more tightly. *)

  type unary_op
  (** The type for unary (prefix and postfix) operators. *)

  val pp_unary_op : unary_op Fmt.t

  val prefix : prec:precedence -> unit Fmt.t -> unary_op
  (** Create a new prefix operator with the given precedence. *)

  val postfix : prec:precedence -> unit Fmt.t -> unary_op
  (** Create a new postfix operator with the given precedence. *)

  type binary_op
  (** The type for binary (infix) operators. *)

  val pp_binary_op : binary_op Fmt.t

  val infix : prec:precedence -> unit Fmt.t -> binary_op
  (** Create a new non-associative binary operator. *)

  val infixl : prec:precedence -> unit Fmt.t -> binary_op
  (** Create a new left-associative binary operator. *)

  val infixr : prec:precedence -> unit Fmt.t -> binary_op
  (** Create a new right-associative binary operator. *)

  type t
  (** The type of expressions built as a tree of operators. *)

  val pp : t Fmt.t

  val atom : 'a Fmt.t -> 'a -> t

  val unary : unary_op -> t -> t

  val binary : t -> binary_op -> t -> t
end