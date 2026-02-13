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

type name = string
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
type object_view_ = [ `ObjectView ]
type object_ref_ = [ `ObjectRef ]
type qualident_ = [`Ident] qual_
type address_ = [ `Address ]
type counter_ = [ `Counter ]
type inline_call_ = [ `InlineCall ]
type inline_invoke_ = [ `InlineInvoke ]
type length_of_ = [ `LengthOf ]
type arith_value_ = length_of_                             (* may be expanded *)
type refmod_scalar_ident_ = [ `RefmodScalarIdent ]
type scalar_ident_ = [qualident_|address_|counter_|inline_call_|inline_invoke_|
                      object_ref_|refmod_scalar_ident_]
type base_ident_ = [scalar_ident_|object_view_]
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

type alphanum_quote =
  | Simple_quote (* '...' *)
  | Double_quote (* "..." *)
[@@deriving ord]

type alphanum_repr =
  | Native_bytes
  | Null_terminated_bytes
[@@deriving ord]

type intrinsic_name =
  | ABS
  | ABSOLUTE_VALUE
  | ACOS
  | ANNUITY
  | ASIN
  | ATAN
  | BASECONVERT
  | BIT_OF
  | BIT_TO_CHAR
  | BOOLEAN_OF_INTEGER
  | BYTE_LENGTH
  | CHAR
  | CHAR_NATIONAL
  | COMBINED_DATETIME
  | CONCAT
  | CONCATENATE
  | CONTENT_LENGTH
  | CONTENT_OF
  | CONVERT
  | COS
  | CURRENCY_SYMBOL
  | CURRENT_DATE
  | DATE_OF_INTEGER
  | DATE_TO_YYYYMMDD
  | DAY_OF_INTEGER
  | DAY_TO_YYYYDDD
  | DISPLAY_OF
  | E
  | EXCEPTION_FILE
  | EXCEPTION_FILE_N
  | EXCEPTION_LOCATION
  | EXCEPTION_LOCATION_N
  | EXCEPTION_STATEMENT
  | EXCEPTION_STATUS
  | EXP
  | EXP10
  | FACTORIAL
  | FIND_STRING
  | FORMATTED_CURRENT_DATE
  | FORMATTED_DATE
  | FORMATTED_DATETIME
  | FORMATTED_TIME
  | FRACTION_PART
  | HEX_OF
  | HEX_TO_CHAR
  | HIGHEST_ALGEBRAIC
  | INTEGER
  | INTEGER_OF_BOOLEAN
  | INTEGER_OF_DATE
  | INTEGER_OF_DAY
  | INTEGER_OF_FORMATTED_DATE
  | INTEGER_PART
  | LENGTH
  | LENGTH_AN
  | LOCALE_COMPARE
  | LOCALE_DATE
  | LOCALE_TIME
  | LOCALE_TIME_FROM_SECONDS
  | LOG
  | LOG10
  | LOWER_CASE
  | LOWEST_ALGEBRAIC
  | MAX
  | MEAN
  | MEDIAN
  | MIDRANGE
  | MIN
  | MOD
  | MODULE_CALLER_ID
  | MODULE_DATE
  | MODULE_FORMATTED_DATE
  | MODULE_ID
  | MODULE_NAME
  | MODULE_PATH
  | MODULE_SOURCE
  | MODULE_TIME
  | MONETARY_DECIMAL_POINT
  | MONETARY_THOUSANDS_SEPARATOR
  | NATIONAL_OF
  | NUMERIC_DECIMAL_POINT
  | NUMERIC_THOUSANDS_SEPARATOR
  | NUMVAL
  | NUMVAL_C
  | NUMVAL_F
  | ORD
  | ORD_MAX
  | ORD_MIN
  | PI
  | PRESENT_VALUE
  | RANDOM
  | RANGE
  | REM
  | REVERSE
  | SECONDS_FROM_FORMATTED_TIME
  | SECONDS_PAST_MIDNIGHT
  | SIGN
  | SIN
  | SQRT
  | STANDARD_COMPARE
  | STANDARD_DEVIATION
  | STORED_CHAR_LENGTH
  | SUBSTITUTE
  | SUBSTITUTE_CASE
  | SUM
  | TAN
  | TEST_DATE_YYYYMMDD
  | TEST_DAY_YYYYDDD
  | TEST_FORMATTED_DATETIME
  | TEST_NUMVAL
  | TEST_NUMVAL_C
  | TEST_NUMVAL_F
  | TRIM
  | UPPER_CASE
  | VARIANCE
  | WHEN_COMPILED
  | YEAR_TO_YYYY
[@@deriving ord, show { with_path = false }]

let show_intrinsic_name i =
  String.map (function '_' -> '-' | c -> c) (show_intrinsic_name i)

let pp_intrinsic_name ppf i =
  Pretty.string ppf (show_intrinsic_name i)

type alphanum =
  {
    str: string;
    quotation: alphanum_quote;
    hexadecimal: bool;
    runtime_repr: alphanum_repr;
  }
[@@deriving ord]

let pp_alphanum ppf { hexadecimal; quotation; str; runtime_repr } =
  if runtime_repr = Null_terminated_bytes then Fmt.char ppf 'Z';
  if hexadecimal then Fmt.char ppf 'X';
  match quotation with
  | Simple_quote -> Fmt.pf ppf "'%s'" str
  | Double_quote -> Fmt.pf ppf "\"%s\"" str

type national = string                                             (* for now *)
[@@deriving ord, show]

(** Now comes the type of all/most terms *)
type _ term =
  | Alphanum: alphanum -> [>alnum_] term
  | Boolean: boolean -> [>bool_] term
  | Fixed: fixed -> [>fixed_] term
  | Floating: floating -> [>float_] term
  | Integer: integer -> [>int_] term
  | National: national -> [>national_] term

  | NumFig: int_ figurative -> [>int_] term
  | Fig: [nonnum_|strlit_] figurative -> [>strlit_] term

  | Name: name with_loc -> [>name_] term
  | Qual: name with_loc * qualname_ term -> [>name qual_] term

  | Address: address -> [>address_] term
  | Counter: counter -> [>counter_] term
  | InlineCall: inline_call -> [>inline_call_] term
  | InlineInvoke: inline_invocation -> [>inline_invoke_] term
  | LengthOf: [ident_|lit_] term -> [>length_of_] term
  | ObjectView: object_view -> [>object_view_] term
  | ObjectRef: object_ref -> [>object_ref_] term (* Includes predefined address (NULL) *)
  | QualIdent: qualident -> [>qualident_] term  (* Includes subscripts *)
  | RefMod: base_ident_ term * refmod -> [>refmod_ident_] term (* Reference modification *)
  | ScalarRefMod: scalar_ident_ term * refmod -> [>refmod_scalar_ident_] term

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

(** As per ISO/IEC 2014: reference to a data item. *)
and ident = ident_ term

(** (Qualified) name (should be `qualref` for "qualified reference" instead). *)
and qualname = qualname_ term

(** Any sort of literal (Boolean, alphanumeric, national, numeric,
    figurative) *)
and literal = lit_ term
(*  | LitConcat of nonnumlit * nonnumlit (\* no ALL *\) *)

and numlit = num_ term
(*  | NumLitFigurative of figurative_constant (\* only ZERO, no ALL *\) *)

and ident_or_alphanum = [ident_|alnum_] term
and ident_or_intlit = [ident_|int_] term

and ident_or_literal = [ident_|lit_] term
and ident_or_nonnum = [ident_|nonnum_] term
and ident_or_numlit = [ident_|num_] term
and ident_or_strlit = [ident_|strlit_] term
and name_or_alphanum = [name_|alnum_] term
and name_or_string = [name_|strlit_] term
and name_or_literal = [name_|lit_] term
and nonnumlit = nonnum_ term
and qualname_or_alphanum = [qualname_|alnum_] term
and qualname_or_intlit = [qualname_|int_] term
and qualname_or_literal = [qualname_|lit_] term
and strlit = strlit_ term
and strlit_or_intlit = [strlit_|int_] term

(* TODO: this is quite general to accomodate any kind of scalar.  However,
   renaming this one to `scalar_or_address` and having a more specific `scalar`
   for numerics (that does not allow `ADDRESS OF`, or later, mnemonics), and
   corresponds to the `arith_x` rule, may make more sense.  *)
and scalar = [scalar_ident_|refmod_ident_|lit_|arith_value_] term

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
  | Atom of scalar
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
  expression * relop * expression                       (** {v e <relop> e' v} *)

(** An abbreviated combined relation describes a condition of the form
    {v NOT? subject <abbrev-relation-operand> v}.
    The leftmost non AbbrevCond element is always an AbbrevRelOp.
    Be careful: the COBOL standard imposes that the optional NOT only applies to the leftmost object in the abbreviated relational condition.
    *)
and abbrev_combined_relation =
  bool * expression * abbrev_relation_operand

(** Suffix of relational combined conditions ({v a v}) *)
and abbrev_relation_operand =
  | AbbrevRelOp of relop * abbrev_relation_operand         (** {v <relop> a v} *)
  | AbbrevObject of bool * expression                         (** {v NOT? e v} *)
  | AbbrevSubject of abbrev_combined_relation                   (** {v NOT? e a v} *)
  | AbbrevNot of abbrev_relation_operand                     (** {v NOT (a) v} *)
  | AbbrevOther of condition              (** {v <non-relational condition> v} *)
  | AbbrevComb of
      (abbrev_relation_operand as 'x) * logop * 'x   (** {v a' <AND/OR> a'' v} *)

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


and inline_call =                   (* in ancient terms: funident *)
  | CallFunc of
      {                  (* name to avoid clash with call statement *)
        func: name with_loc;
        args: effective_arg list;
      }
  | CallGenericIntrinsic of
      {
        func: intrinsic_name with_loc;
        args: effective_arg list;
      }
  (* TODO: for all the ones below: store the intrinsic name with location;
     possibly a CallCustomIntrinsic is in order. *)
  | CallTrim of
      {
        arg: effective_arg;
        tip: trimming_tip option;
      }
  | CallLength of
      {
        arg: ident_or_nonnum;
        physical: bool;
      }
  | CallNumvalC of ident_or_nonnum list
  | CallLocaleDate of locale_func_args
  | CallLocaleTime of locale_func_args
  | CallLocaleTimeFromSeconds of locale_func_args
  | CallFormattedDatetime of formatted_func_args
  | CallFormattedTime of formatted_func_args

and formatted_func_args =
  {
    formatted_func_args: effective_arg list;
    formatted_func_system_offset: bool;
  }

and locale_func_args =
  {
    locale_func_args: effective_arg;
    locale_func_locale: qualname option;
  }

and trimming_tip =
  | Leading
  | Trailing

and effective_arg =                  (* TODO: could be an [expression option] *)
  | ArgExpr of expression (* Regroup identifiers, literals and arithmetic expressions *)
  | ArgOmitted

and qualident =
  {
    ident_name: qualname with_loc;
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
    refmod_left: expression with_loc;
    refmod_length: expression with_loc option;
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

(* --- *)

module COMPARE = struct
  type 'a compare_fun = 'a -> 'a -> int
  (*manual compare for term*)
  let compare_struct first lazy_cmp =
    if first <> 0
    then first
    else Lazy.force lazy_cmp

  let compare_name a b =
    String.compare (String.uppercase_ascii a) (String.uppercase_ascii b)

  let rec compare_term: type a. a term compare_fun =
    fun x y -> match x , y with
      | Alphanum a, Alphanum b ->
          compare_alphanum a b
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
          compare_with_loc compare_name a b
      | Qual (a, c), Qual (b, d) ->
          let first = compare_with_loc compare_name a b in
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
      | LengthOf a, LengthOf b ->
          compare_term a b
      | ObjectView a, ObjectView b ->
          compare_object_view a b
      | ObjectRef a, ObjectRef b ->
          compare_object_ref a b
      | QualIdent a, QualIdent b ->
          compare_qualident a b
      | RefMod (b1, r1), RefMod (b2, r2) ->
          compare_struct (compare_term b1 b2) @@ lazy (compare_refmod r1 r2)
      | ScalarRefMod (b1, r1), ScalarRefMod (b2, r2) ->
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
        compare_struct (Stdlib.compare a b) @@
        lazy (compare_expression c d)
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

  and compare_expression' x y = compare_with_loc compare_expression x y

  and compare_binary_relation (x1, r1, y1) (x2, r2, y2) =
    compare_struct (compare_expression x1 x2) @@
    lazy (compare_struct (compare r1 r2) @@
          lazy (compare_expression y1 y2))
  and compare_abbrev_combined_relation (b1, e1, a1) (b2, e2, a2) =
    compare_struct (Bool.compare b1 b2) @@
    lazy (compare_struct (compare_expression e1 e2) @@
          lazy (compare_abbrev_relation_operand a1 a2))
  and compare_abbrev_relation_operand a b = match a, b with
    | AbbrevRelOp (r1, a1), AbbrevRelOp (r2, a2) ->
        compare_struct (compare_relop r1 r2) @@
        lazy (compare_abbrev_relation_operand a1 a2)
    | AbbrevRelOp _, _ -> -1
    | _, AbbrevRelOp _ -> 1
    | AbbrevObject (b1, e1), AbbrevObject (b2, e2) ->
        compare_struct (Bool.compare b1 b2) @@
        lazy (compare_expression e1 e2)
    | AbbrevObject _, _ -> -1
    | _, AbbrevObject _ -> 1
    | AbbrevSubject r1, AbbrevSubject r2 ->
        compare_abbrev_combined_relation r1 r2
    | AbbrevSubject _, _ -> -1
    | _, AbbrevSubject _ -> 1
    | AbbrevNot a1, AbbrevNot a2 ->
        compare_abbrev_relation_operand a1 a2
    | AbbrevNot _, _ -> -1
    | _, AbbrevNot _ -> 1
    | AbbrevOther c1, AbbrevOther c2 -> compare_cond c1 c2
    | AbbrevOther _, _ -> -1
    | _, AbbrevOther _ -> 1
    | AbbrevComb (x1, o1, y1), AbbrevComb (x2, o2, y2) ->
        compare_struct (compare_abbrev_relation_operand x1 x2) @@
        lazy (compare_struct (compare_logop o1 o2) @@
              lazy (compare_abbrev_relation_operand y1 y2))

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
          compare_struct (compare_expression x1 x2) @@
          lazy (compare_class_ c1 c2)
      | ClassCond _, _ -> -1
      | _, ClassCond _ -> 1
      | SignCond (x1, s1), SignCond(x2, s2) ->
          compare_struct (compare_expression x1 x2) @@
          lazy (compare_signz s1 s2)
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
          compare_struct (compare_logop o1 o2) @@
          lazy (compare_struct (compare_cond x1 x2) @@
                lazy (compare_cond y1 y2))
  and compare_relop =
    Stdlib.compare
  and compare_logop =
    Stdlib.compare
  and compare_class_ a b = match a, b with
    | AlphabetOrClass n1, AlphabetOrClass n2 ->
        compare_with_loc compare_name n1 n2
    | a, b ->
        Stdlib.compare a b
  and compare_qualident
      { ident_name = a; ident_subscripts = c }
      { ident_name = b; ident_subscripts = d } =
    compare_struct (compare_with_loc compare_term a b) @@
    lazy (List.compare compare_subscript c d)
  and compare_subscript x y = match x,y with
    | SubSExpr a ,SubSExpr b ->
        compare_expression a b
    | SubSIdx(n1, s1, i1),
      SubSIdx(n2, s2, i2) ->
        compare_struct (compare_with_loc compare_name n1 n2) @@
        lazy (compare_struct (compare_sign s1 s2) @@
              lazy (compare i1 i2))
    | a, b ->
        Stdlib.compare a b
  and compare_refmod
      { refmod_left = a; refmod_length = c }
      { refmod_left = b; refmod_length = d } =
    compare_struct (compare_expression' a b) @@
    lazy (Option.compare compare_expression' c d)
  and compare_sign : strict_ sign_cond compare_fun = compare
  and compare_signz : loose_ sign_cond compare_fun = compare
  and compare_object_ref x y = match x, y with
    | Super a, Super b ->
        Option.compare (compare_with_loc compare_name) a b
    | a, b ->
        Stdlib.compare a b
  and compare_object_view
      { object_view_ident = a; object_view_spec = c }
      { object_view_ident = b; object_view_spec = d } =
    compare_struct (compare_ident a b) @@
    lazy (compare_object_view_spec c d)
  and compare_object_view_spec x y = match x, y with
    | ObjViewAmbiguous a , ObjViewAmbiguous b
    | ObjViewFactory a, ObjViewFactory b
    | ObjViewOnly a, ObjViewOnly b
    | ObjViewFactoryOnly a, ObjViewFactoryOnly b ->
        compare_with_loc compare_name a b
    | a, b ->
        compare a b
  and compare_inline_invoke
      { invoke_class = a; invoke_meth = c; invoke_args = e }
      { invoke_class = b; invoke_meth = d; invoke_args = f } =
    compare_struct (compare_term a b) @@
    lazy (compare_struct (compare_term c d) @@
          lazy (List.compare compare_effective_arg e f))
  and compare_inline_call x y = match x, y with
    | CallFunc { func = a; args = c }, CallFunc { func = b; args = d } ->
        compare_struct (compare_with_loc compare_name a b) @@
        lazy (List.compare compare_effective_arg c d)
    | CallGenericIntrinsic { func = a; args = c },
      CallGenericIntrinsic { func = b; args = d } ->
        compare_struct (compare_with_loc compare_intrinsic_name a b) @@
        lazy (List.compare compare_effective_arg c d)
    | CallTrim { arg = a; tip = c }, CallTrim { arg = b; tip = d } ->
        compare_struct (compare_effective_arg a b) @@
        lazy (Option.compare compare_trimming_tip c d)
    | CallLength { arg = a; physical = c }, CallLength { arg = b; physical = d } ->
        compare_struct (compare_term a b) @@
        lazy (Bool.compare c d)
    | CallNumvalC a, CallNumvalC b ->
        List.compare compare_term a b
    | CallLocaleDate a, CallLocaleDate b
    | CallLocaleTime a, CallLocaleTime b
    | CallLocaleTimeFromSeconds a, CallLocaleTimeFromSeconds b ->
        compare_locale_func_args a b
    | CallFormattedDatetime a, CallFormattedDatetime b
    | CallFormattedTime a, CallFormattedTime b ->
        compare_formatted_func_args a b
    | CallFunc _, _                  -> -1
    | _, CallFunc _                  ->  1
    | CallGenericIntrinsic _, _      -> -1
    | _, CallGenericIntrinsic _      ->  1
    | CallTrim _, _                  -> -1
    | _, CallTrim _                  ->  1
    | CallLength _, _                -> -1
    | _, CallLength _                ->  1
    | CallNumvalC _ , _              -> -1
    | _ , CallNumvalC _              ->  1
    | CallLocaleDate _, _            -> -1
    | _, CallLocaleDate _            ->  1
    | CallLocaleTime _, _            -> -1
    | _, CallLocaleTime _            ->  1
    | CallLocaleTimeFromSeconds _, _ -> -1
    | _, CallLocaleTimeFromSeconds _ ->  1
    | CallFormattedDatetime _, _     -> -1
    | _, CallFormattedDatetime _     ->  1
  and compare_locale_func_args
      { locale_func_args = a; locale_func_locale = c }
      { locale_func_args = b; locale_func_locale = d } =
    compare_struct (compare_effective_arg a b) @@
    lazy (Option.compare compare_term c d)
  and compare_formatted_func_args
      { formatted_func_args = a; formatted_func_system_offset = c }
      { formatted_func_args = b; formatted_func_system_offset = d } =
    compare_struct (List.compare compare_effective_arg a b) @@
    lazy (Bool.compare c d)
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
    lazy (Option.compare (compare_with_loc compare_name) c d)

  and compare_ident: ident compare_fun = fun a b -> compare_term a b
  and compare_trimming_tip x y =
    match x, y with
    | Leading, Leading | Trailing, Trailing -> 0
    | Trailing, Leading -> -1
    | Leading, Trailing -> 1

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
  let compare_name_or_literal: name_or_literal compare_fun = compare_term
  let compare_qualname_or_alphanum: qualname_or_alphanum compare_fun = compare_term
  let compare_qualname_or_intlit: qualname_or_intlit compare_fun = compare_term
  let compare_qualname_or_literal: qualname_or_literal compare_fun = compare_term
  let compare_strlit: strlit compare_fun = compare_term
  let compare_strlit_or_intlit: strlit_or_intlit compare_fun = compare_term
  let compare_scalar: scalar compare_fun = compare_term

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
    | Alphanum s -> pp_alphanum ppf s
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
    | LengthOf i -> fmt "@[LENGTH OF@ %a@]" ppf pp_term i
    | ObjectView o -> pp_object_view ppf o
    | ObjectRef o -> pp_object_ref ppf o
    | QualIdent i -> pp_qualident ppf i
    | RefMod (i, r) -> fmt "@[%a@ %a@]" ppf pp_term i pp_refmod r
    | ScalarRefMod (i, r) -> fmt "@[%a@ %a@]" ppf pp_term i pp_refmod r

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
      pp_expression' refmod_left
      (option pp_expression') refmod_length

  and pp_qualident ppf { ident_name = n; ident_subscripts } =
    pp_qualname' ppf n;
    if ident_subscripts <> []
    then fmt "@[<1>(%a)@]" ppf (list ~sep:comma pp_subscript) ident_subscripts

  and pp_qualname ppf = pp_term ppf
  and pp_qualname' ppf = pp_with_loc pp_qualname ppf

  and pp_address ppf = function
    | DataAddress i -> fmt "ADDRESS@ OF@ %a" ppf pp_ident i
    | ProgAddress i -> fmt "ADDRESS@ OF@ PROGRAM@ %a" ppf pp_term i

  and pp_inline_call ppf = function
    | CallFunc { func; args } ->
        fmt "FUNCTION@ %a@ @[<1>(%a)@]" ppf pp_name' func
          (list ~sep:sp pp_effective_arg) args
    | CallGenericIntrinsic { func; args } ->
        fmt "FUNCTION@ %a@ @[<1>(%a)@]" ppf
          (pp_with_loc pp_intrinsic_name) func
          (list ~sep:sp pp_effective_arg) args
    | CallTrim { arg; tip } ->
        fmt "FUNCTION@ TRIM@ @[<1>(%a, %a)@]" ppf
          pp_effective_arg arg
          (option pp_trimming_tip) tip
    | CallLength { arg; physical } ->
        fmt "FUNCTION@ LENGTH@ @[<1>(%a%t)@]" ppf
          pp_term arg
          (if physical then fmt ",@ PHYSICAL" else ignore)
    | CallNumvalC args ->
        fmt "FUNCTION@ NUMVAL-C@ @[<1>(%a)@]" ppf
          (list ~sep:sp pp_term) args
    | CallLocaleDate { locale_func_args = dt;
                       locale_func_locale = l } ->
        fmt "FUNCTION@ LOCALE-DATE@ @[<1>(%a%a)@]" ppf
          pp_effective_arg dt
          (option ~none:nop (fun ppf -> fmt ",@ %a" ppf pp_term)) l
    | CallLocaleTime { locale_func_args = dt;
                       locale_func_locale = l } ->
        fmt "FUNCTION@ LOCALE-TIME@ @[<1>(%a,@ %a)@]" ppf
          pp_effective_arg dt
          (option ~none:nop (fun ppf -> fmt ",@ %a" ppf pp_term)) l
    | CallLocaleTimeFromSeconds { locale_func_args = dt;
                                  locale_func_locale = l } ->
        fmt "FUNCTION@ LOCALE-TIME-FROM-SECONDS@ @[<1>(%a, %a)@]" ppf
          pp_effective_arg dt
          (option ~none:nop (fun ppf -> fmt ",@ %a" ppf pp_term)) l
    | CallFormattedDatetime { formatted_func_args = a;
                              formatted_func_system_offset = so } ->
        fmt "FUNCTION@ FORMATTED-DATETIME@ @[<1>(%a%t)@]" ppf
          (list pp_effective_arg) a
          (if so then fmt ",@ SYSTEM-OFFSET" else ignore)
    | CallFormattedTime { formatted_func_args = a;
                          formatted_func_system_offset = so } ->
        fmt "FUNCTION@ FORMATTED-TIME@ @[<1>(%a%t)@]" ppf
          (list pp_effective_arg) a
          (if so then fmt ",@ SYSTEM-OFFSET" else ignore)

  and pp_trimming_tip ppf = function
    | Leading -> fmt "LEADING" ppf
    | Trailing -> fmt "TRAILING" ppf

  and pp_inline_invocation ppf { invoke_class; invoke_meth; invoke_args } =
    fmt "%a::%a@ @[<1>(%a)@]" ppf pp_ident invoke_class pp_literal invoke_meth
      (list ~sep:sp pp_effective_arg) invoke_args

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

  and pp_scalar: scalar Pretty.printer = fun ppf -> pp_term ppf

  and pp_expression ppf e = Unparse.Expression.pp ppf (pretty_expression e)
  and pp_expression' ppf = pp_with_loc pp_expression ppf

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
    | Abbrev r ->
        fmt "%a%a" ppf not_ pos pp_abbrev_combined_relation r
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

  and pp_abbrev_combined_relation ppf (neg, e, a) =
    fmt "@[<1>(%a%a@ %a)@]" ppf not_ (not neg) pp_expression e pp_abbrev_relation_operand a

  and pp_abbrev_relation_operand ppf = function
    | AbbrevRelOp (o, a) ->
        fmt "%a@ %a" ppf pp_relop o pp_abbrev_relation_operand a
    | AbbrevObject (neg, e) ->
        fmt "%a%a" ppf not_ (not neg) pp_expression e
    | AbbrevSubject r ->
        pp_abbrev_combined_relation ppf r
    | AbbrevNot a ->
        fmt "NOT@ @[<1>(%a)@]" ppf pp_abbrev_relation_operand a
    | AbbrevOther c ->
        fmt "@[<1>(%a)@]" ppf pp_condition c
    | AbbrevComb (a1, o, a2) ->
        fmt "@[<1>(%a@ %a@ %a)@]" ppf
          pp_abbrev_relation_operand a1
          pp_logop o
          pp_abbrev_relation_operand a2

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
  let pp_name_or_literal: name_or_literal Pretty.printer = pp_term
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
    | ScalarRefMod _ as v -> v

  let ident_with_nonnum: ident -> ident_or_nonnum = function
    | QualIdent _ as v -> v
    | InlineCall _ as v -> v
    | InlineInvoke _ as v -> v
    | ObjectView _ as v -> v
    | ObjectRef _ as v -> v
    | Address _ as v -> v
    | Counter _ as v -> v
    | RefMod _ as v -> v
    | ScalarRefMod _ as v -> v

  let ident_with_numeric: ident -> ident_or_numlit = function
    | QualIdent _ as v -> v
    | InlineCall _ as v -> v
    | InlineInvoke _ as v -> v
    | ObjectView _ as v -> v
    | ObjectRef _ as v -> v
    | Address _ as v -> v
    | Counter _ as v -> v
    | RefMod _ as v -> v
    | ScalarRefMod _ as v -> v

  let ident_with_string: ident -> ident_or_strlit = function
    | QualIdent _ as v -> v
    | InlineCall _ as v -> v
    | InlineInvoke _ as v -> v
    | ObjectView _ as v -> v
    | ObjectRef _ as v -> v
    | Address _ as v -> v
    | Counter _ as v -> v
    | RefMod _ as v -> v
    | ScalarRefMod _ as v -> v

  let ident_with_literal: ident -> ident_or_literal = function
    | QualIdent _ as v -> v
    | InlineCall _ as v -> v
    | InlineInvoke _ as v -> v
    | ObjectView _ as v -> v
    | ObjectRef _ as v -> v
    | Address _ as v -> v
    | Counter _ as v -> v
    | RefMod _ as v -> v
    | ScalarRefMod _ as v -> v

  let ident_with_integer: ident -> ident_or_intlit = function
    | QualIdent _ as v -> v
    | InlineCall _ as v -> v
    | InlineInvoke _ as v -> v
    | ObjectView _ as v -> v
    | ObjectRef _ as v -> v
    | Address _ as v -> v
    | Counter _ as v -> v
    | RefMod _ as v -> v
    | ScalarRefMod _ as v -> v

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

  let literal_with_name: literal -> name_or_literal = function
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

  let name_with_literal: name_ term -> name_or_literal = function
    | Name _ as v -> v

  let base_ident_with_refmod: base_ident_ term -> ident = function
    | QualIdent _ as v -> v
    | InlineCall _ as v -> v
    | InlineInvoke _ as v -> v
    | ObjectView _ as v -> v
    | ObjectRef _ as v -> v
    | Address _ as v -> v
    | Counter _ as v -> v
    | ScalarRefMod _ as v -> v

  let scalar_ident_as_scalar: scalar_ident_ term -> scalar = function
    | QualIdent _ as v -> v
    | InlineCall _ as v -> v
    | InlineInvoke _ as v -> v
    | ObjectRef _ as v -> v
    | Address _ as v -> v
    | Counter _ as v -> v
    | ScalarRefMod _ as v -> v

  let numeric_as_scalar: numlit -> scalar = function
    | Integer _ as v -> v
    | Fixed _ as v -> v
    | Floating _ as v -> v
    | NumFig _ as v -> v

  let nonnumlit_as_scalar: nonnumlit -> scalar = function
    | Alphanum _ as v -> v
    | National _ as v -> v
    | Boolean _ as v -> v
    | Fig _ as v -> v
    | StrConcat _ as v -> v
    | Concat _ as v -> v

  let literal_as_scalar: literal -> scalar = function
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

  let simple_cond: simple_condition -> condition = function
    | Expr _ as c -> c
    | Relation _ as c -> c
    | Abbrev _ as c -> c
    | ClassCond _ as c -> c
    | SignCond _ as c -> c
    | Omitted _ as c -> c
end

(* --- *)

type rounding =
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

(* --- *)

type procedure_name = qualname
[@@deriving ord]

let pp_procedure_name = pp_qualname
let pp_procedure_name' = pp_with_loc pp_qualname
