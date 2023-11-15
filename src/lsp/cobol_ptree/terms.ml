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

open Common
open Numericals

type name = string                                              [@@deriving ord]
let pp_name = Pretty.string

let pp_name' ppf { payload; _ } = pp_name ppf payload

(** {2 Term attributes} *)

(* Polymorphic GADT parameter types are a neat solution to share type and tag
   names. This is what we use to unify the type of all/most terms in the
   syntax.

   For now this appears to break `deriving.show` (ppx), but that limitation
   should not prevent us from using neat typing features and obtain ASTs that
   are easier to work with.
*)
type alnum_ = [ `AlphaNum ]
type bool_ = [ `Bool ]
type fixed_ = [ `Fixed ]
type float_ = [ `Float ]
type int_ = [ `Int ]
type name_ = [ `Name ]
type national_ = [ `National ]
type 'a qual_ = [ `Qual of 'a ]
type base_ident_ = [ `DirectIdent ]
type refmod_ident_ = [ `RefmodIdent ]
type ident_ = [base_ident_|refmod_ident_]
type qualname_ = [name_|name qual_]
type num_ = [int_|fixed_|float_]
type nonnum_ = [alnum_|national_|bool_]
type strlit_ = [alnum_|national_]
type lit_ = [nonnum_|bool_|num_]


(* Attributes for distinguishing expressions *)
type simple_ = [ `Simple ]
type complex_ = [ `Complex ]

(* Attributes for distinguishing sign conditions *)
type strict_ = [ `Strict ]
type loose_ = [ `Loose ]

type alphanum_kind =
  | Squote (* '...' *)
  | Dquote (* "..." *)
  | Hex (* X"..." *)
[@@deriving ord]

type alphanum_string = string * alphanum_kind
[@@deriving ord]

let pp_alphanum_string ppf (s, k) =
  match k with
  | Squote -> Fmt.pf ppf "'%s'" s
  | Dquote -> Fmt.pf ppf "\"%s\"" s
  | Hex -> Fmt.pf ppf "X\"%s\"" s

(** Now comes the type of all/most terms *)
type _ term =
  | Alphanum: alphanum_string -> [>alnum_] term
  | Boolean: boolean -> [>bool_] term
  | Fixed: fixed -> [>fixed_] term
  | Floating: floating -> [>float_] term
  | Integer: integer -> [>int_] term
  | National: string -> [>national_] term

  | NumFig: int_ figurative -> [>int_] term
  | Fig: [nonnum_|strlit_] figurative -> [>strlit_] term

  | Name: name with_loc -> [>name_] term
  | Qual: name with_loc * qualname_ term -> [>name qual_] term

  | Address: address -> [>base_ident_] term
  | Counter: counter -> [>base_ident_] term
  | InlineCall: inline_call -> [>base_ident_] term
  | InlineInvoke: inline_invocation -> [>base_ident_] term
  | ObjectView: object_view -> [>base_ident_] term
  | ObjectRef: object_ref -> [>base_ident_] term (* Includes predefined address (NULL) *)
  | QualIdent: qualident -> [>base_ident_] term  (* Includes subscripts *)
  | RefMod: base_ident_ term * refmod -> [>ident_] term (* Reference modification *)

  | StrConcat: strlit_ term * strlit_ term -> [>strlit_] term
  | Concat: nonnum_ term * nonnum_ term -> [>nonnum_] term

and _ figurative =
  | Zero: [<int_|nonnum_] figurative            (* ALPHA/NAT/BOOL/NUM *)
  | Space: [>strlit_] figurative                (* ALPHA/NAT *)
  | Quote: [>strlit_] figurative                (* ALPHA/NAT *)
  | LowValue: [>strlit_] figurative             (* ALPHA/NAT *)
  | HighValue: [>strlit_] figurative            (* ALPHA/NAT *)
  | All: nonnumlit -> [<nonnum_] figurative      (* ALPHA/NAT/BOOL + fig const *)
(* (\* | Symbolic of ident (\* use in alphanum, national *\) *\) *)

(** and then particular instantiations. *)

and ident = ident_ term
and qualname = qualname_ term

and literal = lit_ term
(*  | LitConcat of nonnumlit * nonnumlit (\* no ALL *\) *)

and numlit = num_ term
(*  | NumLitFigurative of figurative_constant (\* only ZERO, no ALL *\) *)

and alphanum = alnum_ term
and national = national_ term
and ident_or_alphanum = [ident_|alnum_] term
and ident_or_intlit = [ident_|int_] term

and ident_or_literal = [ident_|lit_] term
and ident_or_nonnum = [ident_|nonnum_] term
and ident_or_numlit = [ident_|num_] term
and ident_or_strlit = [ident_|strlit_] term
and name_or_alphanum = [name_|alnum_] term
and name_or_string = [name_|strlit_] term
and nonnumlit = nonnum_ term
and qualname_or_alphanum = [qualname_|alnum_] term
and qualname_or_intlit = [qualname_|int_] term
and qualname_or_literal = [qualname_|lit_] term
and strlit = strlit_ term
and strlit_or_intlit = [strlit_|int_] term

and binop =
  | BPlus
  | BMinus
  | BMul
  | BDiv
  | BPow
  | BAnd
  | BOr
  | BXor

and unop =
  | UPlus
  | UMinus
  | UNot

and expression =
  | Atom of ident_or_literal
  | Unop of unop * expression
  | Binop of expression * binop * expression (* split arith/bool ? *)

(** Any form of condition {v c v} *)
and _ cond =
  | Expr:
      expression -> [>simple_] cond         (** expression used as a condition *)
  | Relation:
      binary_relation -> [>simple_] cond            (** simple binary relation *)
  | Abbrev:
      abbrev_combined_relation -> [>simple_] cond     (** abbreviated relation *)
  | ClassCond:
      expression * class_ -> [>simple_] cond               (** class condition *)
  | SignCond:
      expression * signz -> [>simple_] cond (** {v e POSITIVE/NEGATIVE/ZERO v} *)
  | Omitted:
      expression -> [>simple_] cond                        (** {v c OMITTED v} *)
  | Not:
      _ cond -> [>complex_] cond                               (** {v NOT c v} *)
  | Logop:
      _ cond * logop * _ cond -> [>complex_] cond      (** {v c <AND/OR> c' v} *)

and binary_relation =
  expression * relop * expression                      (** {v e <relop> e' v} *)

(** An abbreviated combined relation describes a non-parenthesized condition:

    - {v e <relop> e' <AND/OR> <abbreviated-suffix> v} if [not neg] holds (the
      first item in the tuple);

    - {v NOT e <relop> e' <AND/OR> <abbreviated-suffix> v} otherwise. *)
and abbrev_combined_relation =
  bool * binary_relation * logop * flat_combined_relation

(** Suffix of non-parenthesized relational combined conditions ({v a v}) *)
and flat_combined_relation =
  | FlatAmbiguous of
      relop option * expression                          (** {v <relop>? e v} *)
  | FlatNotExpr of
      expression                                              (** {v NOT e v} *)
  | FlatRel of
      bool * binary_relation                     (**  {v NOT? e <relop> e' v} *)
  | FlatOther of
      condition            (** {v <non-relational/parenthesized condition> v} *)
  | FlatComb of
      (flat_combined_relation as 'x) * logop * 'x   (** {v a' <AND/OR> a'' v} *)

and condition = [simple_|complex_] cond
and simple_condition = simple_ cond

and logop =
  | LAnd
  | LOr

and relop =
  | Gt
  | Lt
  | Eq
  | Ne
  | Ge
  | Le

and class_ =
  | AlphabetOrClass of name with_loc
  | Alphabetic
  | AlphabeticLower
  | AlphabeticUpper
  | ClassBoolean
  | FarthestFromZero
  | FloatInfinity
  | FloatNotANumber
  | FloatNotANumberQuiet
  | FloatNotANumberSignaling
  | InArithmeticRange
  | NearestToZero
  | ClassNumeric


and inline_call =                               (* in ancient terms: funident *)
  {
    call_fun: name with_loc;
    call_args: effective_arg list;
  }

and effective_arg =                  (* TODO: could be an [expression option] *)
  | ArgExpr of expression (* Regroup identifiers, literals and arithmetic expressions *)
  | ArgOmitted

and qualident =
  {
    ident_name: qualname;
    ident_subscripts: subscript list;
  }

and subscript =
  | SubSAll
  | SubSExpr of expression
  | SubSIdx of name with_loc * sign * integer

and _ sign_cond =
  | SgnPositive: [<strict_|loose_] sign_cond
  | SgnNegative: [<strict_|loose_] sign_cond
  | SgnZero: [<loose_] sign_cond
and sign = strict_ sign_cond
and signz = loose_ sign_cond

and refmod =
  {
    refmod_left: expression;
    refmod_length: expression option;
  }

and inline_invocation =
  {
    invoke_class: ident;
    invoke_meth: literal;
    invoke_args: effective_arg list;
  }

and object_view =
  {
    object_view_ident: ident;
    object_view_spec: object_view_spec;
  }

and object_view_spec =
  | ObjViewAmbiguous of name with_loc                 (* Factory or Interface *)
  | ObjViewFactory of name with_loc
  | ObjViewOnly of name with_loc
  | ObjViewFactoryOnly of name with_loc
  (* | Interface of name with_loc (currently unused) *)
  | ObjViewUniversal

and object_ref =
  | ExceptionObject
  | Null
  | Self
  | Super of name with_loc option

and address =
  | DataAddress of ident
  | ProgAddress of ident_or_literal

and counter =
  {
    counter_kind: counter_kind;
    counter_name: name with_loc option;
  }
and counter_kind =
  | LineageCounter
  | PageCounter
  | LineCounter

module COMPARE = struct
  type 'a compare_fun = 'a -> 'a -> int
  (*manual compare for term*)
  let compare_struct first lazy_cmp =
    if first <> 0
    then first
    else Lazy.force lazy_cmp
  let compare_with_loc_name =
    compare_with_loc compare_name

  let rec compare_term: type a. a term compare_fun =
    fun x y -> match x , y with
      | Alphanum a, Alphanum b -> compare_alphanum_string a b
      | Alphanum _, Fig HighValue ->
          -1
      | Alphanum _, Fig _ -> 1
      | Boolean a, Boolean b ->
          Stdlib.compare a b
      | Integer a, Integer b ->
          String.compare a b
      | Integer _, Fig Zero
      | Integer _, NumFig Zero ->
          1
      | National a, National b ->
          String.compare a b
      | National _, Fig HighValue ->
          -1
      | National _, Fig _ ->
          1
      | NumFig Zero, Integer _ ->
          -1
      | NumFig Zero, NumFig Zero ->
          0
      | Fig Zero, Integer _ ->
          -1
      | Fig _, Fig _ ->
          0                                            (* TODO: `compare_fig` *)
      | Name a, Name b ->
          compare_with_loc_name a b
      | Qual (a, c), Qual (b, d) ->
          let first = compare_with_loc_name a b in
          if first <> 0
          then first
          else compare_term c d
      | Address a, Address b ->
          compare_address a b
      | Counter a, Counter b ->
          compare_counter a b
      | InlineCall a, InlineCall b ->
          compare_inline_call a b
      | InlineInvoke a, InlineInvoke b ->
          compare_inline_invoke a b
      | ObjectView a, ObjectView b ->
          compare_object_view a b
      | ObjectRef a, ObjectRef b ->
          compare_object_ref a b
      | QualIdent a, QualIdent b ->
          compare_qualident a b
      | RefMod (b1, r1), RefMod (b2, r2) ->
          compare_struct (compare_term b1 b2) @@ lazy (compare_refmod r1 r2)
      | StrConcat (a, c), StrConcat (b, d) ->
          compare_struct (compare_term a b) @@ lazy (compare_term c d)
      | Concat(a,c), Concat(b,d) ->
          compare_struct (compare_term a b) @@ lazy (compare_term c d)
      | a , b ->
          Stdlib.compare a b
  and compare_expression x y = match x, y with
    | Atom a ,Atom b ->
        compare_term a b
    | Unop(a, c), Unop(b, d) ->
        compare_struct (Stdlib.compare a b) @@ lazy (compare_expression c d)
    | Binop(a, c ,e), Binop(b, d, f) ->
        compare_struct (Stdlib.compare c d) @@
        lazy (compare_struct (compare_expression a b) @@
              lazy (compare_expression e f))
    (* take the arbitrari order Binop > Unop > Atom *)
    | Atom _, _ ->
        -1
    | _, Atom _->
        1
    | Unop _, _ ->
        -1
    | _, Unop _ ->
        1
  and compare_binary_relation (x1, r1, y1) (x2, r2, y2) =
    compare_struct (compare_expression x1 x2) @@
    lazy (compare_struct (compare r1 r2) @@ lazy (compare_expression y1 y2))
  and compare_abbrev_combined_relation (b1, r1, x1, y1) (b2, r2, x2, y2) =
    compare_struct (Bool.compare b1 b2) @@ lazy (
      compare_struct (compare_binary_relation r1 r2) @@ lazy (
        compare_struct (compare_logop x1 x2) @@
        lazy (compare_flat_combined_relation y1 y2)
      )
    )
  and compare_flat_combined_relation a b = match a, b with
    | FlatAmbiguous (ro1, e1), FlatAmbiguous (ro2, e2) ->
      compare_struct (Option.compare compare_relop ro1 ro2) @@
      lazy (compare_expression e1 e2)
    | FlatAmbiguous _, _ -> -1
    | _, FlatAmbiguous _ -> 1
    | FlatNotExpr e1, FlatNotExpr e2 -> compare_expression e1 e2
    | FlatNotExpr _, _ -> -1
    | _, FlatNotExpr _ -> 1
    | FlatRel (b1, r1), FlatRel (b2, r2) ->
      compare_struct (Bool.compare b1 b2) @@
      lazy (compare_binary_relation r1 r2)
    | FlatRel _, _ -> -1
    | _, FlatRel _ -> 1
    | FlatOther c1, FlatOther c2 -> compare_cond c1 c2
    | FlatOther _, _ -> -1
    | _, FlatOther _ -> 1
    | FlatComb (x1, o1, y1), FlatComb (x2, o2, y2) ->
      compare_struct (compare_flat_combined_relation x1 x2) @@ lazy (
        compare_struct (compare_logop o1 o2) @@
        lazy (compare_flat_combined_relation y1 y2)
      )

  and compare_cond : type a b. a cond -> b cond -> int =
    fun a b -> match a, b with
    | Expr x, Expr y ->
        compare_expression x y
    | Expr _, _ -> -1
    | _, Expr _ -> 1
    | Relation x, Relation y -> compare_binary_relation x y
    | Relation _, _ -> -1
    | _, Relation _ -> 1
    | Abbrev x, Abbrev y -> compare_abbrev_combined_relation x y
    | Abbrev _, _ -> -1
    | _, Abbrev _ -> 1
    | ClassCond (x1, c1), ClassCond (x2, c2) ->
        compare_struct (compare_expression x1 x2) @@ lazy (compare_class_ c1 c2)
    | ClassCond _, _ -> -1
    | _, ClassCond _ -> 1
    | SignCond (x1, s1), SignCond(x2, s2) ->
        compare_struct (compare_expression x1 x2) @@ lazy (compare_signz s1 s2)
    | SignCond _, _ -> -1
    | _, SignCond _ -> 1
    | Omitted x, Omitted y ->
        compare_expression x y
    | Omitted _, _ -> -1
    | _, Omitted _ -> 1
    | Not x, Not y ->
        compare_cond x y
    | Not _, _ -> -1
    | _, Not _ -> 1
    | Logop (x1, o1, y1), Logop (x2, o2, y2) ->
        compare_struct (compare_logop o1 o2) @@ lazy (
          compare_struct (compare_cond x1 x2) @@ lazy (
            compare_cond y1 y2
          )
        )
  and compare_relop =
    Stdlib.compare
  and compare_logop =
    Stdlib.compare
  and compare_class_ a b = match a, b with
    | AlphabetOrClass n1, AlphabetOrClass n2 ->
        compare_with_loc_name n1 n2
    | a, b ->
        Stdlib.compare a b
  and compare_qualident
      { ident_name = a; ident_subscripts = c }
      { ident_name = b; ident_subscripts = d } =
    compare_struct (compare_term a b) @@
    lazy (List.compare compare_subscript c d)
  and compare_subscript x y = match x,y with
    | SubSExpr a ,SubSExpr b ->
        compare_expression a b
    | SubSIdx(n1, s1, i1),
      SubSIdx(n2, s2, i2) ->
        compare_struct (compare_with_loc_name n1 n2) @@
        lazy (compare_struct (compare_sign s1 s2) @@ lazy (compare i1 i2))
    | a, b ->
        Stdlib.compare a b
  and compare_refmod
      { refmod_left = a; refmod_length = c }
      { refmod_left = b; refmod_length = d } =
    compare_struct (compare_expression a b) @@
    lazy (Option.compare compare_expression c d)
  and compare_sign : strict_ sign_cond compare_fun = compare
  and compare_signz : loose_ sign_cond compare_fun = compare
  and compare_object_ref x y = match x, y with
    | Super a, Super b ->
        Option.compare compare_with_loc_name a b
    | a, b ->
        Stdlib.compare a b
  and compare_object_view
      { object_view_ident = a; object_view_spec = c }
      { object_view_ident = b; object_view_spec = d } =
    compare_struct (compare_ident a b) @@ lazy (compare_object_view_spec c d)
  and compare_object_view_spec x y = match x, y with
    | ObjViewAmbiguous a , ObjViewAmbiguous b
    | ObjViewFactory a, ObjViewFactory b
    | ObjViewOnly a, ObjViewOnly b
    | ObjViewFactoryOnly a, ObjViewFactoryOnly b ->
        compare_with_loc_name a b
    | a, b ->
        compare a b
  and compare_inline_invoke
      { invoke_class = a; invoke_meth = c; invoke_args = e }
      { invoke_class = b; invoke_meth = d; invoke_args = f } =
    compare_struct (compare_term a b) @@
    lazy (compare_struct (compare_term c d) @@
          lazy (List.compare compare_effective_arg e f))
  and compare_inline_call
      { call_fun = a; call_args = c }
      { call_fun = b; call_args = d } =
    compare_struct (compare_with_loc_name a b) @@
    lazy (List.compare compare_effective_arg c d)
  and compare_effective_arg x y = match x, y with
    | ArgExpr a, ArgExpr b ->
        compare_expression a b
    | ArgExpr _, ArgOmitted ->
        1
    | ArgOmitted, ArgOmitted ->
        0
    | ArgOmitted, ArgExpr _ ->
        -1
  and compare_address x y = match x, y with
    | DataAddress a, DataAddress b ->
        compare_term a b
    | ProgAddress a, ProgAddress b ->
        compare_term a b
    | a, b ->
        compare a b
  and compare_counter
      { counter_kind = a; counter_name = c }
      { counter_kind = b; counter_name = d } =
    compare_struct (Stdlib.compare a b) @@
    lazy (Option.compare (compare_with_loc_name) c d)

  and compare_ident: ident compare_fun = fun a b -> compare_term a b

  let compare_qualname: qualname compare_fun = compare_term
  let compare_literal: literal compare_fun = compare_term
  let compare_ident_or_numlit: ident_or_numlit compare_fun = compare_term
  let compare_ident_or_alphanum: ident_or_alphanum compare_fun = compare_term
  let compare_ident_or_intlit: ident_or_intlit compare_fun = compare_term
  let compare_ident_or_literal: ident_or_literal compare_fun = compare_term
  let compare_ident_or_nonnum: ident_or_nonnum compare_fun = compare_term
  let compare_ident_or_strlit: ident_or_strlit compare_fun = compare_term
  let compare_name_or_alphanum: name_or_alphanum compare_fun = compare_term
  let compare_name_or_string: name_or_string compare_fun = compare_term
  let compare_qualname_or_alphanum: qualname_or_alphanum compare_fun = compare_term
  let compare_qualname_or_intlit: qualname_or_intlit compare_fun = compare_term
  let compare_qualname_or_literal: qualname_or_literal compare_fun = compare_term
  let compare_strlit: strlit compare_fun = compare_term
  let compare_strlit_or_intlit: strlit_or_intlit compare_fun = compare_term

  let compare_condition = compare_cond
end
include COMPARE

(* (\** [major_qualifier qualname] returns [Name name] when [qualname] is *)
(*     [Qual (..., Qual (_, Name name)) | Name name]*\) *)
(* let rec major_qualifier_of_qualname qualname = *)
(*   match (qualname: qualname) with *)
(*   | Qual (_, qn) -> *)
(*       major_qualifier_of_qualname qn *)
(*   | Name n -> n *)

(* (\** [qualifier_of_qualname qualname] returns [name] when [qualname] is [Name name] or *)
(*     [Qual (name, _)] *\) *)
(* let qualifier_of_qualname: qualname -> name with_loc = function *)
(*   | Qual (name, _) -> name *)
(*   | Name name -> name *)


(** {2 Manual prettty-printing for terms} *)
module FMT = struct

  open Fmt

  let pp_boolean: boolean Pretty.printer = fun ppf -> function
    (* | { bool_width = 0; _ } -> *)
    (*     string ppf "zero-length-boolean" *)
    | { bool_value; _ } ->
        string ppf bool_value

  let rec pp_term: type k. k term Pretty.printer = fun ppf -> function
    | Alphanum s -> pp_alphanum_string ppf s
    | Boolean b -> pp_boolean ppf b
    | Fixed f -> pp_fixed ppf f
    | Floating f -> pp_floating ppf f
    | Integer i -> pp_integer ppf i
    | National s -> fmt "N%S" ppf s
    | NumFig f -> pp_figurative ppf f
    | Fig f -> pp_figurative ppf f

    | Name n -> pp_name' ppf n
    | Qual (n, q) -> fmt "%a@ IN@ %a" ppf pp_name' n pp_term q

    | Address i -> pp_address ppf i
    | Counter c -> pp_counter ppf c
    | InlineCall i -> pp_inline_call ppf i
    | InlineInvoke i -> pp_inline_invocation ppf i
    | ObjectView o -> pp_object_view ppf o
    | ObjectRef o -> pp_object_ref ppf o
    | QualIdent i -> pp_qualident ppf i
    | RefMod (i, r) -> fmt "@[%a@ %a@]" ppf pp_term i pp_refmod r

    | StrConcat (a, b) -> fmt "%a@ &@ %a" ppf pp_term a pp_term b
    | Concat (a, b) -> fmt "%a@ &@ %a" ppf pp_term a pp_term b

  and pp_figurative: type k. k figurative Pretty.printer = fun ppf -> function
    | Zero -> string ppf "ZERO"
    | Space -> string ppf "SPACE"
    | Quote -> string ppf "QUOTE"
    | LowValue -> fmt "LOW-VALUES" ppf
    | HighValue -> fmt "HIGH-VALUES" ppf
    | All l -> fmt "ALL@ %a" ppf pp_term l

  and pp_subscript ppf : subscript -> unit = function
    | SubSAll -> string ppf "ALL"
    | SubSExpr e -> pp_expression ppf e
    | SubSIdx (n, s, i) -> fmt "%a@ %a@ %a" ppf pp_name' n pp_sign s pp_integer i

  and pp_refmod ppf { refmod_left; refmod_length } =
    fmt "@[<1>(%a:%a)@]" ppf
      pp_expression refmod_left
      (option pp_expression) refmod_length

  and pp_qualident ppf { ident_name = n; ident_subscripts } =
    pp_qualname ppf n;
    if ident_subscripts <> []
    then fmt "@[<1>(%a)@]" ppf (list ~sep:comma pp_subscript) ident_subscripts

  and pp_qualname ppf = pp_term ppf
  and pp_qualname' ppf = pp_with_loc pp_qualname ppf

  and pp_address ppf = function
    | DataAddress i -> fmt "ADDRESS@ OF@ %a" ppf pp_ident i
    | ProgAddress i -> fmt "ADDRESS@ OF@ PROGRAM@ %a" ppf pp_term i

  and pp_inline_call ppf { call_fun; call_args } =
    fmt "FUNCTION@ %a@ @[<1>(%a)@]" ppf pp_name' call_fun
      (list ~sep:comma pp_effective_arg) call_args

  and pp_inline_invocation ppf { invoke_class; invoke_meth; invoke_args } =
    fmt "%a::%a@ @[<1>(%a)@]" ppf pp_ident invoke_class pp_literal invoke_meth
      (list ~sep:comma pp_effective_arg) invoke_args

  and pp_effective_arg ppf = function
    | ArgOmitted -> string ppf "OMITTED"
    (* In a sequence of arguments, `1, - X` is parsed as `1 - X` so we need to
       parenthesize the unary part to get `1 (- X)`. But then if we have
        `Y (- X)`  that is no longer correct. And even consider `(1 + X) (- 2)`
       So we just put parentheses everywhere and call it a day for now (which is
       a pity, given that we do work in pp_expression to avoid unnecessary
       parentheses). *)
    | ArgExpr e -> Fmt.parens pp_expression ppf e

  and pp_object_view ppf { object_view_ident; object_view_spec } =
    fmt "%a@ AS@ " ppf pp_ident object_view_ident;
    match object_view_spec with
    | ObjViewAmbiguous n -> pp_name' ppf n
    | ObjViewOnly n -> fmt "%a@ ONLY" ppf pp_name' n
    | ObjViewFactory n -> fmt "FACTORY@ OF@ %a" ppf pp_name' n
    | ObjViewFactoryOnly n -> fmt "FACTORY@ OF@ %a@ ONLY" ppf pp_name' n
    | ObjViewUniversal -> string ppf "UNIVERSAL"

  and pp_object_ref ppf = function
    | ExceptionObject -> string ppf "EXCEPTION-OBJECT"
    | Null -> string ppf "NULL"
    | Self -> string ppf "SELF"
    | Super None -> string ppf "SUPER"
    | Super (Some n) -> fmt "%a@ OF@ SUPER" ppf pp_name' n

  and pp_counter ppf { counter_kind; counter_name } =
    let k = match counter_kind with
      | LineageCounter -> "LINAGE-COUNTER"
      | PageCounter -> "PAGE-COUNTER"
      | LineCounter -> "LINE-COUNTER"
    in
    string ppf k;
    Option.iter (fmt "@ OF@ %a" ppf pp_name') counter_name

  and pp_expression ppf e = Unparse.Expression.pp ppf (pretty_expression e)

  and pretty_expression = function
    | Atom a -> Unparse.Expression.atom pp_term a
    | Unop (o, e) ->
      Unparse.Expression.unary (pretty_unop o) (pretty_expression e)
    | Binop (a, o, b) ->
      Unparse.Expression.binary
        (pretty_expression a) (pretty_binop o) (pretty_expression b)

  and pretty_unop = function
    | UPlus -> Unparse.Expression.prefix ~prec:4 (Fmt.const pp_unop UPlus)
    | UMinus -> Unparse.Expression.prefix ~prec:4 (Fmt.const pp_unop UMinus)
    | UNot -> Unparse.Expression.prefix ~prec:4 (Fmt.const pp_unop UNot)
  and show_unop = function
    | UPlus  -> "+"
    | UMinus -> "-"
    | UNot -> "B-NOT"
  and pp_unop ppf o = string ppf (show_unop o)

  and pretty_binop op =
    match op with
    | BPlus -> Unparse.Expression.infixl ~prec:1 (Fmt.const pp_binop op)
    | BMinus -> Unparse.Expression.infixl ~prec:1 (Fmt.const pp_binop op)
    | BMul -> Unparse.Expression.infixl ~prec:2 (Fmt.const pp_binop op)
    | BDiv -> Unparse.Expression.infixl ~prec:2 (Fmt.const pp_binop op)
    | BPow -> Unparse.Expression.infixr ~prec:3 (Fmt.const pp_binop op)
    | BAnd -> Unparse.Expression.infixl ~prec:3 (Fmt.const pp_binop op)
    | BOr -> Unparse.Expression.infixl ~prec:1 (Fmt.const pp_binop op)
    | BXor -> Unparse.Expression.infixl ~prec:2 (Fmt.const pp_binop op)
  and show_binop = function
    | BPlus -> "+"
    | BMinus -> "-"
    | BMul -> "*"
    | BDiv -> "/"
    | BPow -> "**"
    | BAnd -> "B-AND"
    | BOr  -> "B-OR"
    | BXor -> "B-XOR"
  and pp_binop ppf o = string ppf (show_binop o)

  and pp_binary_relation ppf (a, o, b) =
    fmt "%a@ %a@ %a" ppf
      pp_expression a pp_relop o pp_expression b

  and pp_cond
    : type k. ?pos:_ -> k cond Pretty.printer = fun ?(pos = true) ppf -> function
    | Expr e ->
        fmt "%a%a" ppf not_ pos pp_expression e
    | Relation rel ->
        fmt "%a@[<1>(%a)@]" ppf not_ pos pp_binary_relation rel
    | Abbrev (neg, rel, o, comb) ->
        fmt "%a@[<1>(%a%a@ %a@ %a)@]" ppf
          not_ pos not_ (not neg) pp_binary_relation rel pp_logop o
          pp_flat_combined_relation comb
    | ClassCond (e, c) ->
        fmt "%a@ %a%a" ppf pp_expression e not_ pos pp_class_ c
    | SignCond (e, s) ->
        fmt "%a@ %a%a" ppf pp_expression e not_ pos pp_sign s
    | Omitted e ->
        fmt "%a@ %aOMITTED" ppf pp_expression e not_ pos
    | Not c ->
        pp_cond ~pos:(not pos) ppf c
    | Logop (a, o, b) ->
        fmt "@[<1>%a(%a@ %a@ %a)@]" ppf
          not_ pos (pp_cond ~pos:true) a pp_logop o (pp_cond ~pos:true) b

  and pp_flat_combined_relation ppf = function
    | FlatAmbiguous (None, e) ->
        pp_expression ppf e
    | FlatAmbiguous (Some r, e) ->
        fmt "%a@ %a" ppf pp_relop r pp_expression e
    | FlatNotExpr e ->
        fmt "NOT@ %a" ppf pp_expression e
    | FlatRel (neg, rel) ->
        fmt "%a%a" ppf not_ (not neg) pp_binary_relation rel
    | FlatOther c ->
        fmt "@[<1>(%a)@]" ppf pp_condition c
    | FlatComb (c1, o, c2) ->
        fmt "%a@ %a@ %a" ppf
          pp_flat_combined_relation c1
          pp_logop o
          pp_flat_combined_relation c2

  and pp_condition ppf = pp_cond ppf
  and not_ ppf = function false -> fmt "NOT@ " ppf | true -> ()

  and show_relop = function
    | Gt -> ">"
    | Lt -> "<"
    | Eq -> "="
    | Ne -> "<>"
    | Ge -> ">="
    | Le -> "<="
  and pp_relop ppf o = string ppf (show_relop o)

  and show_class_ = function
    | AlphabetOrClass n -> str "%a" pp_name' n
    | Alphabetic -> "ALPHABETIC"
    | AlphabeticLower -> "ALPHABETIC-LOWER"
    | AlphabeticUpper -> "ALPHABETIC-UPPER"
    | ClassBoolean -> "BOOLEAN"
    | FarthestFromZero -> "FARTHEST-FROM-ZERO"
    | FloatInfinity -> "FLOAT-INFINITY"
    | FloatNotANumber -> "FLOAT-NOT-A-NUMBER"
    | FloatNotANumberQuiet -> "FLOAT-NOT-A-NUMBER-QUIET"
    | FloatNotANumberSignaling -> "FLOAT-NOT-A-NUMBER-SIGNALING"
    | InArithmeticRange -> "IN-ARITHMETIC-RANGE"
    | NearestToZero -> "NEAREST-TO-ZERO"
    | ClassNumeric -> "NUMERIC"
  and pp_class_ ppf c = string ppf (show_class_ c)

  and pp_sign: type k. k sign_cond Pretty.printer = fun ppf -> function
    | SgnPositive -> string ppf "POSITIVE"
    | SgnNegative -> string ppf "NEGATIVE"
    | SgnZero -> string ppf "ZERO"
  and pp_signz ppf = pp_sign ppf

  and pp_logop ppf = function
    | LAnd -> string ppf "AND"
    | LOr -> string ppf "OR"

  and pp_literal: literal Pretty.printer = fun ppf -> pp_term ppf
  and pp_literal' = fun ppf -> pp_with_loc pp_literal ppf
  and pp_ident: ident Pretty.printer = fun ppf -> pp_term ppf

  (** Pretty-printing for named unions of term types (some are yet to be
      renamed) *)

  let pp_ident_or_alphanum: ident_or_alphanum Pretty.printer = pp_term
  let pp_ident_or_intlit: ident_or_intlit Pretty.printer = pp_term
  let pp_ident_or_literal: ident_or_literal Pretty.printer = pp_term
  let pp_ident_or_nonnum: ident_or_nonnum Pretty.printer = pp_term
  let pp_ident_or_numlit: ident_or_numlit Pretty.printer = pp_term
  let pp_ident_or_strlit: ident_or_strlit Pretty.printer = pp_term
  let pp_strlit: strlit Pretty.printer = pp_term
  let pp_name_or_string: name_or_string Pretty.printer = pp_term
  let pp_name_or_alphanum: name_or_alphanum Pretty.printer = pp_term
  let pp_strlit_or_intlit: strlit_or_intlit Pretty.printer = pp_term
  let pp_qualname_or_literal: qualname_or_literal Pretty.printer = pp_term
  let pp_qualname_or_intlit: qualname_or_intlit Pretty.printer = pp_term
  let pp_qualname_or_alphanum: qualname_or_alphanum Pretty.printer = pp_term

end
include FMT

module UPCAST = struct
  (** Exlicit term upcasting utilities, that should all reduce to identity. *)

  let ident_with_alphanum: ident -> ident_or_alphanum = function
    | QualIdent _ as v -> v
    | InlineCall _ as v -> v
    | InlineInvoke _ as v -> v
    | ObjectView _ as v -> v
    | ObjectRef _ as v -> v
    | Address _ as v -> v
    | Counter _ as v -> v
    | RefMod _ as v -> v

  let ident_with_nonnum: ident -> ident_or_nonnum = function
    | QualIdent _ as v -> v
    | InlineCall _ as v -> v
    | InlineInvoke _ as v -> v
    | ObjectView _ as v -> v
    | ObjectRef _ as v -> v
    | Address _ as v -> v
    | Counter _ as v -> v
    | RefMod _ as v -> v

  let ident_with_numeric: ident -> ident_or_numlit = function
    | QualIdent _ as v -> v
    | InlineCall _ as v -> v
    | InlineInvoke _ as v -> v
    | ObjectView _ as v -> v
    | ObjectRef _ as v -> v
    | Address _ as v -> v
    | Counter _ as v -> v
    | RefMod _ as v -> v

  let ident_with_string: ident -> ident_or_strlit = function
    | QualIdent _ as v -> v
    | InlineCall _ as v -> v
    | InlineInvoke _ as v -> v
    | ObjectView _ as v -> v
    | ObjectRef _ as v -> v
    | Address _ as v -> v
    | Counter _ as v -> v
    | RefMod _ as v -> v

  let ident_with_literal: ident -> ident_or_literal = function
    | QualIdent _ as v -> v
    | InlineCall _ as v -> v
    | InlineInvoke _ as v -> v
    | ObjectView _ as v -> v
    | ObjectRef _ as v -> v
    | Address _ as v -> v
    | Counter _ as v -> v
    | RefMod _ as v -> v

  let ident_with_integer: ident -> ident_or_intlit = function
    | QualIdent _ as v -> v
    | InlineCall _ as v -> v
    | InlineInvoke _ as v -> v
    | ObjectView _ as v -> v
    | ObjectRef _ as v -> v
    | Address _ as v -> v
    | Counter _ as v -> v
    | RefMod _ as v -> v

  let string_with_name: strlit -> name_or_string = function
    | Alphanum _ as v -> v
    | National _ as v -> v
    | Fig _ as v -> v
    | StrConcat _ as v -> v

  let string_with_ident: strlit -> ident_or_strlit = function
    | Alphanum _ as v -> v
    | National _ as v -> v
    | Fig _ as v -> v
    | StrConcat _ as v -> v

  let numeric_with_ident: numlit -> ident_or_numlit = function
    | Integer _ as v -> v
    | Fixed _ as v -> v
    | Floating _ as v -> v
    | NumFig _ as v -> v

  let nonnum_with_ident: nonnumlit -> ident_or_nonnum = function
    | Alphanum _ as v -> v
    | National _ as v -> v
    | Boolean _ as v -> v
    | Fig _ as v -> v
    | StrConcat _ as v -> v
    | Concat _ as v -> v

  let literal_with_ident: literal -> ident_or_literal = function
    | Alphanum _ as v -> v
    | National _ as v -> v
    | Boolean _ as v -> v
    | Integer _ as v -> v
    | Fixed _ as v -> v
    | Floating _ as v -> v
    | NumFig _ as v -> v
    | Fig _ as v -> v
    | StrConcat _ as v -> v
    | Concat _ as v -> v

  let literal_with_qualdatname: literal -> qualname_or_literal = function
    | Alphanum _ as v -> v
    | National _ as v -> v
    | Boolean _ as v -> v
    | Integer _ as v -> v
    | Fixed _ as v -> v
    | Floating _ as v -> v
    | NumFig _ as v -> v
    | Fig _ as v -> v
    | StrConcat _ as v -> v
    | Concat _ as v -> v

  let qualname_with_alphanum: qualname -> qualname_or_alphanum = function
    | Name _ as v -> v
    | Qual _ as v -> v

  let qualname_with_literal: qualname -> qualname_or_literal = function
    | Name _ as v -> v
    | Qual _ as v -> v

  let qualname_with_integer: qualname -> qualname_or_intlit = function
    | Name _ as v -> v
    | Qual _ as v -> v

  let base_ident_with_refmod: base_ident_ term -> ident = function
    | QualIdent _ as v -> v
    | InlineCall _ as v -> v
    | InlineInvoke _ as v -> v
    | ObjectView _ as v -> v
    | ObjectRef _ as v -> v
    | Address _ as v -> v
    | Counter _ as v -> v

  let simple_cond: simple_condition -> condition = function
    | Expr _ as c -> c
    | Relation _ as c -> c
    | Abbrev _ as c -> c
    | ClassCond _ as c -> c
    | SignCond _ as c -> c
    | Omitted _ as c -> c
end

type rounded_ident =
  {
    rounded_ident: ident;
    rounded_rounding: rounding;
  }
[@@deriving ord]

and rounding =
  | RoundingNotAny
  | RoundingDefault
  | RoundingMode of rounding_mode
[@@deriving ord]

and rounding_mode =
  | AwayFromZero
  | NearestAwayFromZero
  | NearestEven
  | NearestTowardZero
  | TowardGreater
  | TowardLesser
  | Truncation
  | Prohibited
[@@deriving ord]

and rounded_idents = rounded_ident list
[@@deriving ord]

let pp_rounding_mode ppf = function
  | AwayFromZero -> Fmt.pf ppf "AWAY-FROM-ZERO"
  | NearestAwayFromZero -> Fmt.pf ppf "NEAREST-AWAY-FROM-ZERO"
  | NearestEven -> Fmt.pf ppf "NEAREST-EVEN"
  | NearestTowardZero -> Fmt.pf ppf "NEAREST-TOWARD-ZERO"
  | TowardGreater -> Fmt.pf ppf "TOWARD-GREATER"
  | TowardLesser -> Fmt.pf ppf "TOWARD-LESSER"
  | Truncation -> Fmt.pf ppf "TRUNCATION"
  | Prohibited -> Fmt.pf ppf "PROHIBITED"

let pp_rounding ppf = function
  | RoundingNotAny -> ()
  | RoundingDefault -> Fmt.pf ppf "ROUNDED"
  | RoundingMode rm -> Fmt.pf ppf "ROUNDED MODE IS %a" pp_rounding_mode rm

let pp_rounded_ident ppf { rounded_ident = i; rounded_rounding = r } =
  match r with
  | RoundingNotAny | RoundingMode Truncation -> pp_ident ppf i
  | _ -> Fmt.pf ppf "%a %a" pp_ident i pp_rounding r

let pp_rounded_idents = Fmt.(list ~sep:sp pp_rounded_ident)
