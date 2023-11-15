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

open Cobol_common
open Srcloc.TYPES
module CHARS = Cobol_common.Basics.CharSet

module TYPES = struct

  type symbol =
    | A
    | B
    | CR
    | CS
    | DB
    | DecimalSep
    | E
    | GroupingSep
    | L
    | Minus
    | N
    | Nine
    | One
    | P
    | Plus
    | S
    | Slant
    | Star
    | V
    | X
    | Z
    | Zero
  [@@deriving ord]

  let pp_symbol ppf = function
    | A -> Fmt.char ppf 'A'
    | B -> Fmt.char ppf 'B'
    | CR -> Fmt.string ppf "CR"
    | CS -> Fmt.fmt "currency@ symbol" ppf
    | DB -> Fmt.string ppf "DB"
    | DecimalSep -> Fmt.fmt "decimal@ point" ppf
    | E -> Fmt.char ppf 'E'
    | GroupingSep -> Fmt.fmt "grouping@ separator" ppf
    | Minus -> Fmt.char ppf '-'
    | N -> Fmt.char ppf 'N'
    | Nine -> Fmt.char ppf '9'
    | One -> Fmt.char ppf '1'
    | P -> Fmt.char ppf 'P'
    | Plus -> Fmt.char ppf '+'
    | L -> Fmt.char ppf 'L'
    | S -> Fmt.char ppf 'S'
    | Slant -> Fmt.char ppf '/'
    | Star -> Fmt.char ppf '*'
    | V -> Fmt.char ppf 'V'
    | X -> Fmt.char ppf 'X'
    | Z -> Fmt.char ppf 'Z'
    | Zero -> Fmt.char ppf '0'

  type symbols =
    {
      symbol: symbol;
      symbol_occurences: int;
    }
  [@@deriving show, ord]

  type editions =
    {
      basics: basic_edition list;
      floating: floating_insertion option;
      zerorepl: zero_replacement option;
    }
  [@@deriving show, ord]

  and basic_edition =
    | SimpleInsertion of simple_insertion
    | SpecialInsertion of special_insertion
    | FixedInsertion of fixed_insertion

  (* The following may later becore mappings from Int to symbols, and even
     become abstract to maintain some level of integrity on the integers
     involved. *)
  and simple_insertion =
    {
      simple_insertion_symbols: symbols;
      simple_insertion_offset: int;
    }

  and special_insertion =
    {
      special_insertion_offset: int;
      special_insertion_length: int;
    }

  and fixed_insertion =
    {
      fixed_insertion_symbol: symbol;
      fixed_insertion_offset: int;
    }

  and floating_insertion =
    {
      floating_insertion_symbol: symbol;
      floating_insertion_ranges: floating_range list;
    }

  and zero_replacement =
    {
      zero_replacement_symbol: symbol;
      zero_replacement_ranges: floating_range list;
    }

  and floating_range =
    {
      floating_range_offset: int;
      floating_range_length: int;
    }

  type category =
    | Alphabetic of
        {
          length: int;
        }
    | Alphanumeric of
        {
          length: int;
          insertions: simple_insertion list;
        }
    | Boolean of
        {
          length: int;
        }
    | National of
        {
          length: int;
          insertions: simple_insertion list;
        }
    | FixedNum of
        {
          digits: int;
          scale: int;
          with_sign: bool;
          editions: editions;
        }
    | FloatNum of
        {
          digits: int;
          scale: int;
          with_sign: bool;
          exponent_digits: int;
          editions: basic_edition list;
        }
  [@@deriving ord]


  let pp_category ppf = function
    | Alphabetic { length } ->
        Fmt.fmt "ALPHABETIC(%u)" ppf length
    | Alphanumeric { length; insertions = [] } ->
        Fmt.fmt "ALPHANUMERIC(%u)" ppf length
    | Alphanumeric { length; insertions = _ } ->
        Fmt.fmt "ALPHANUMERIC-EDITED(%u)" ppf length
    | Boolean { length } ->
        Fmt.fmt "BOOLEAN(%u)" ppf length
    | National { length; insertions = [] } ->
        Fmt.fmt "NATIONAL(%u)" ppf length
    | National { length; insertions = _ } ->
        Fmt.fmt "NATIONAL-EDITED(%u)" ppf length
    | FixedNum { digits; scale; with_sign;
                 editions = { basics = [];
                              floating = None;
                              zerorepl = None } } ->
        Fmt.fmt "NUMERIC(@[digits = %u,@;scale = %d,@;with_sign = %B@])" ppf
          digits scale with_sign
    | FixedNum { digits; scale; with_sign; editions } ->
        Fmt.fmt "NUMERIC-EDITED(@[digits = %u,@;scale = %d,@;\
                 with_sign = %B,@;editions = %a@])" ppf
          digits scale with_sign pp_editions editions
    | FloatNum { digits; scale; with_sign; exponent_digits; editions = [] } ->
        Fmt.fmt "FLOAT(@[digits = %u,@;scale = %d,@;exponent_digits = \
                 %u,@;with_sign = %B@])" ppf
          digits scale exponent_digits with_sign
    | FloatNum { digits; scale; with_sign; exponent_digits; editions } ->
        Fmt.fmt "FLOAT(@[digits = %u,@;scale = %d,@;exponent_digits = \
                 %u,@;with_sign = %B,@;%a@])" ppf
          digits scale exponent_digits with_sign
          (Fmt.list pp_basic_edition) editions


  type picture =
    {
      category: category;
      pic: symbols list;
    }
  [@@deriving show, ord]

  type config = {
    max_pic_length : int;
    decimal_char: char;
    currency_signs: Cobol_common.Basics.CharSet.t;
  }

  type error =
    | May_only_appear_once of { symbol_precedence: int;
                                decimal_char: char }
    | May_not_follow of { symbol_precedence: int;
                          prev_precedence: int;
                          decimal_char: char }
    | Parenthesis_must_be_preceded_by_picture_symbol
    | Unexpected_char of char
    | Extraneous_symbol_in_exponent
    | Symbol_may_only_appear_once of symbol
    | Symbol_must_be_at_start of symbol
    | Symbol_must_be_at_end of symbol
    | Symbol_must_be_at_start_or_end of symbol
    | Symbols_are_mutually_exclusive of symbol * symbol
    | Unexpected_symbol of symbol * category option
    | Empty_picture_string
    | Picture_length_exceeds_limit of (* length *) int * (* max_len *) int
    | Missing_symbol_in_exponent
    | Missing_digits_in_exponent
    | Picture_describes_empty_data_item
    | Numeric_item_cannot_exceed_38_digits of int

  module type ENV = sig
    val decimal_char: char
    val currency_signs: Cobol_common.Basics.CharSet.t
  end

end

type t = TYPES.picture
[@@deriving show, ord]

open TYPES

module Symbol = struct type t = symbol let compare = Stdlib.compare end
module Symbols = Set.Make (Symbol)
module SymbolsMap = Map.Make (Symbol)

let pp_category = pp_category

(* --- *)

let is_edited = function
  | Alphabetic _
  | Boolean _
  | Alphanumeric { insertions = []; _ }
  | National { insertions = []; _ }
  | FixedNum { editions = { basics = []; floating = None; zerorepl = None }; _ }
  | FloatNum { editions = []; _ } -> false
  | _ -> true

let data_size: category -> int = function
  | Alphabetic { length }
  | Boolean { length }
  | Alphanumeric { length; _ } -> length
  | National { length; _ } -> length * 2
  | FixedNum { digits; _ } -> digits
  | FloatNum { digits; exponent_digits; _ } -> digits + exponent_digits

let edited_size: category -> int =
  let simple_insertion_size { simple_insertion_symbols = symbols; _ } =
    symbols.symbol_occurences
  and special_insertion_size { special_insertion_length = n; _ } = n in
  let simple_insertions_size =
    List.fold_left (fun s i -> s + simple_insertion_size i) 0
  and basic_editions_size basics =
    List.fold_left begin fun s -> function
      | SimpleInsertion i -> s + simple_insertion_size i
      | SpecialInsertion i -> s + special_insertion_size i
      | FixedInsertion _ -> s + 1
    end 0 basics
  in
  let editions_size { basics; floating; _ } =
    basic_editions_size basics + if floating = None then 0 else 1
  in
  function
  | Alphabetic { length }
  | Boolean { length } ->
      length
  | Alphanumeric { length; insertions } ->
      length +
      simple_insertions_size insertions
  | National { length; insertions } ->
      length * 2 +
      simple_insertions_size insertions
  | FixedNum { digits; with_sign; editions; _ } ->
      digits + editions_size editions +
      if with_sign then 1 else 0  (* WARNING: depends on SIGN/SEPARATE phrase *)
  | FloatNum { digits; exponent_digits; editions; _ } ->
      digits + exponent_digits + basic_editions_size editions

let size = edited_size

(* --- *)

(* NOTE: During construction edition lists in categories are in reversed
   order. *)

let reverse_editions =
  let reverse_floating { floating_insertion_symbol;
                         floating_insertion_ranges } =
    { floating_insertion_symbol;
      floating_insertion_ranges = List.rev floating_insertion_ranges }
  and reverse_zerorepl { zero_replacement_symbol;
                         zero_replacement_ranges } =
    { zero_replacement_symbol;
      zero_replacement_ranges = List.rev zero_replacement_ranges }
  in
  function
  | Alphabetic _ | Boolean _ as c ->
      c
  | Alphanumeric { length; insertions } ->
      Alphanumeric {length; insertions = List.rev insertions }
  | National { length; insertions } ->
      National {length; insertions = List.rev insertions }
  | FixedNum { digits; scale; with_sign;
               editions = { basics; floating; zerorepl } } ->
      let basics = List.rev basics
      and floating = Option.map reverse_floating floating
      and zerorepl = Option.map reverse_zerorepl zerorepl in
      FixedNum { digits; scale; with_sign;
                 editions = { basics; floating; zerorepl } }
  | FloatNum { digits; scale; with_sign; exponent_digits; editions } ->
      FloatNum { digits; scale; with_sign; exponent_digits;
                 editions = List.rev editions }




let as_simple_insertions = function
  | { floating = Some _; _ } ->
      raise Exit
  | { basics; _ } ->
      List.map begin function
        | SimpleInsertion i -> i
        | SpecialInsertion _
        | FixedInsertion _ -> raise Exit
      end basics


let append_insertion ({ basics; floating; _ } as editions) symbols offset =
  let has_any_fixed_insertion =
    List.exists (function FixedInsertion _ -> true | _ -> false)
  in
  let rec previous_fixed_insertion acc = function
    | [] ->
        Ok None
    | FixedInsertion { fixed_insertion_symbol = s;
                       fixed_insertion_offset = o } :: tl
      when s = symbols.symbol ->
        if has_any_fixed_insertion acc
        then Error ()     (* no fixed insertion is allowed in floating string *)
        else Ok (Some (o, List.rev_append acc tl))
    | i :: tl ->
        previous_fixed_insertion (i :: acc) tl
  in
  let new_range = { floating_range_offset = offset;
                    floating_range_length = symbols.symbol_occurences } in
  match floating with
  | Some { floating_insertion_symbol = s;
           floating_insertion_ranges = ranges }
    when s = symbols.symbol ->
      let editions =
        { editions with
          floating = Some { floating_insertion_symbol = s;
                            floating_insertion_ranges = new_range :: ranges } }
      in
      Ok (editions, symbols.symbol_occurences) (* all symbols count as digits *)
  | _
    when symbols.symbol_occurences = 1 ->        (* fixed insertion with a priori
                                                    non-floating symbol *)
      let editions =                   (* (further checked in check_editions) *)
        { editions with
          basics = FixedInsertion { fixed_insertion_symbol = symbols.symbol;
                                    fixed_insertion_offset = offset } :: basics }
      in
      Ok (editions, 0)          (* no more digit *)
  | Some _ ->                    (* invalid floating insertion with new symbol *)
      Error ()
  | None ->
      match previous_fixed_insertion [] basics with
      | Ok None ->
          let editions =
            { editions with
              floating = Some { floating_insertion_symbol = symbols.symbol;
                                floating_insertion_ranges = [new_range] } }
          in
          Ok (editions, symbols.symbol_occurences - 1)
      | Ok (Some (previous_offset, basics)) ->
          let old_range = { floating_range_offset = previous_offset;
                            floating_range_length = 1 } in
          let editions =
            { editions with
              basics;
              floating = Some { floating_insertion_symbol = symbols.symbol;
                                floating_insertion_ranges = [new_range;
                                                             old_range] } }
          in
          Ok (editions, symbols.symbol_occurences)
      | Error () ->
          Error ()


let append_zero_replacement ({ zerorepl; _ } as editions) symbols offset =
  let new_range = { floating_range_offset = offset;
                    floating_range_length = symbols.symbol_occurences } in
  match zerorepl with
  | None ->
      Ok { editions with
           zerorepl = Some { zero_replacement_symbol = symbols.symbol;
                             zero_replacement_ranges = [new_range] } }
  | Some { zero_replacement_symbol = s;
           zero_replacement_ranges = ranges }
    when s = symbols.symbol ->
      Ok { editions with
           zerorepl = Some { zero_replacement_symbol = s;
                             zero_replacement_ranges = new_range :: ranges } }
  | Some _ ->
      Error ()


let append category ~after_v ({ symbol; symbol_occurences = n } as symbols) =
  let error = Result.Error (category, symbol) in
  let alphanum length insertions =
    Ok (Alphanumeric { length; insertions })
  and numeric
      ?(with_sign = false)
      ?(editions = { basics = []; floating = None; zerorepl = None })
      digits scale =
    FixedNum { digits; scale; with_sign; editions }
  and float
      ?(with_sign = false)
      ?(editions = [])
      digits scale exponent_digits =
    FloatNum { digits; scale; with_sign; exponent_digits; editions }
  in
  let append_A = function
    | Alphabetic { length } ->
        Ok (Alphabetic { length = length + n })
    | Alphanumeric { length; insertions } ->
        Ok (Alphanumeric { length = length + n; insertions })
    | FixedNum { digits; scale = 0; with_sign = false; editions } ->
        (try alphanum (digits + n) (as_simple_insertions editions)
         with Exit -> error)                (* 'cause of non-simple insertions *)
    | _ -> error
  and append_9 = function
    | Alphabetic { length } ->
        alphanum (length + n) []
    | Alphanumeric { length; insertions } ->
        alphanum (length + n) insertions
    | FixedNum { digits; scale; with_sign; editions } ->
        Ok (numeric (digits + n) (if after_v then scale + n else scale)
              ~with_sign ~editions)
    | FloatNum { digits; scale; with_sign; exponent_digits; editions } ->
        Ok (float digits scale (exponent_digits + n) ~with_sign ~editions)
    | _ -> error
  and append_X = function
    | Alphabetic { length } ->
        alphanum (length + n) []
    | Alphanumeric { length; insertions } ->
        alphanum (length + n) insertions
    | FixedNum { digits; scale = 0; with_sign = false; editions }  ->
        (try alphanum (digits + n) (as_simple_insertions editions)
         with Exit -> error)
    | _ -> error
  and append_P = function
    | FixedNum { digits; scale; with_sign; editions } ->
        Ok (numeric ~with_sign ~editions
              (digits + n)
              (scale + if digits = 0 then n else - n))
    | _ -> error
  and append_simple_insertion =
    let simple_insertion c =
      { simple_insertion_symbols = symbols;
        simple_insertion_offset = size c }
    in
    function
    | Alphabetic { length } as c ->
        alphanum length [simple_insertion c]
    | Alphanumeric { length; insertions } as c ->
        alphanum length (simple_insertion c :: insertions)
    | FixedNum { digits; scale; with_sign; editions } as c ->
        let editions =
          { editions with
            basics = SimpleInsertion (simple_insertion c) :: editions.basics } in
        Ok (numeric ~with_sign ~editions digits scale)
    | _ -> error
  and append_fixed_or_floating_insertion = function
    | FixedNum { digits; scale; with_sign; editions } as c
      when not with_sign || digits > 0 ->    (* forbidden in between S and digits *)
        (match append_insertion editions symbols (edited_size c) with
         | Ok (editions, digits') ->
             let digits = digits + digits'
             and scale = if after_v then scale + digits' else scale in
             Ok (numeric ~with_sign ~editions digits scale)
         | Error () -> error)
    | _ -> error
  and append_special_insertion offset = function
    | FixedNum { digits; scale; with_sign; editions } ->
        let special = SpecialInsertion { special_insertion_offset = offset;
                                         special_insertion_length = n } in
        Ok (numeric ~with_sign digits scale
              ~editions:{ editions with basics = special :: editions.basics })
    | _ -> error
  and append_zero_replacement = function
    | FixedNum { digits; scale; with_sign; editions } as c ->
        (match append_zero_replacement editions symbols (edited_size c) with
         | Ok editions ->
             let digits = digits + n
             and scale = if after_v then scale + n else scale in
             Ok (numeric ~with_sign ~editions digits scale)
         | Error () -> error)
    | _ -> error
  and append_E = function
    | FixedNum { digits; scale; with_sign;
                 editions = { basics; floating = None; zerorepl = None } } ->
        Ok (float digits scale 0 ~with_sign ~editions:basics)
    | _ -> error
  in
  (* TODO: always numeric-edited when BLANK WHEN ZERO *)
  match category, symbol with
  | None, (A | L) ->
      Ok (Alphabetic { length = n })
  | None, X ->
      Ok (Alphanumeric { length = n; insertions = [] })
  | None, One ->
      Ok (Boolean { length = n })
  | None, N ->
      Ok (National { length = n; insertions = [] })
  | None, Nine ->
      Ok (numeric n 0)
  | None, S ->
      Ok (numeric 0 0 ~with_sign:true)
  | None, V ->
      Ok (numeric 0 0)
  | None, P ->
      Ok (numeric n n)
  | None, (Zero | B | Slant) ->                (* default to numeric if prefix *)
      append_simple_insertion (numeric 0 0)
  | None, GroupingSep ->                                   (* simple insertion *)
      append_simple_insertion (numeric 0 0)
  | None, (CS | Plus | Minus | CR | DB) ->      (* fixed or floating insertion *)
      append_fixed_or_floating_insertion (numeric 0 0)
  | None, DecimalSep ->                                   (* special insertion *)
      append_special_insertion 0 (numeric 0 0)
  | None, (Z | Star) ->
      append_zero_replacement (numeric 0 0)
  | Some c, A ->
      append_A c
  | Some c, X ->
      append_X c
  | Some (Boolean { length }), One ->
      Ok (Boolean { length = length + n })
  | Some (National { length; insertions }), N ->
      Ok (National { length = length + n; insertions })
  | Some c, Nine ->
      append_9 c
  | Some c, P ->
      append_P c
  | Some (FixedNum _ as c), V when not after_v ->
      Ok c
  | Some c, E ->
      append_E c
  | Some (FloatNum { exponent_digits = 0; _ } as c), Plus ->
      (* NOTE: that + seems to be mandatory according to ISO/IEC 2014; this is
         checked at (E+ check) below. *)
      Ok c
  | Some c, (Zero | B | Slant) ->
      append_simple_insertion c
  | Some c, GroupingSep ->                                 (* simple insertion *)
      append_simple_insertion c
  | Some c, (CS | Plus | Minus | CR | DB) ->                (* fixed insertion *)
      append_fixed_or_floating_insertion c
  | Some c, DecimalSep ->                                 (* special insertion *)
      append_special_insertion (edited_size c) c
  | Some c, (Z | Star) ->
      append_zero_replacement c
  | _ ->
      error

exception INVALIDCHAR of char

let symbol_of_char config c =
  match c with
  | c when CHARS.mem c config.currency_signs -> CS
  | '.' | ',' as c when c == config.decimal_char -> DecimalSep
  | '.' | ',' -> GroupingSep
  | '*' -> Star
  | '+' -> Plus
  | '-' -> Minus
  | '/' -> Slant
  | '0' -> Zero
  | '1' -> One
  | '9' -> Nine
  | 'A' -> A
  | 'B' -> B
  | 'E' -> E
  | 'L' -> L
  | 'N' -> N
  | 'P' -> P
  | 'S' -> S
  | 'V' -> V
  | 'X' -> X
  | 'Z' -> Z
  | c -> raise @@ INVALIDCHAR c

let symbol_precedence_index
    ~max_idx ~after_v ~after_e ~idx ~zero_suppress_or_floating_insert
  : symbol -> int = function
  | B | Zero | Slant -> 0
  | GroupingSep -> 1
  | DecimalSep -> 2
  | Plus when after_e -> 3
  | Plus | Minus when not zero_suppress_or_floating_insert && idx < max_idx -> 4
  | Plus | Minus when not zero_suppress_or_floating_insert -> 5
  | CR | DB -> 6
  | CS when not zero_suppress_or_floating_insert && idx < 2 -> 7
  | CS when not zero_suppress_or_floating_insert -> 8
  | Z | Star when not after_v -> 9
  | Z | Star -> 10
  | Plus | Minus when not after_v -> 11
  | Plus | Minus -> 12
  | CS when not after_v -> 13
  | CS -> 14
  | Nine -> 15
  | A | X -> 16
  | L -> 17
  | S -> 18
  | V -> 19
  | P when not after_v -> 20
  | P -> 21
  | One -> 22
  | N -> 23
  | E -> 24

let precedence_table =
  (* An 'x' indicates that the symbol in the colon may precede the symbol of the
     row.
      B  ,  .  +  +  +  CR cs cs Z  Z  +  +  cs cs 9  A  L  S  V  P  P  1  N  E
      0           -  -  DB       *  *  -  -           X
      /                                                                         *)
  Array.map (fun xs -> Array.init 25 (fun i -> xs.[i * 3 + 1] = 'x')) [|
    " x  x  x     x        x     x  x  x  x  x  x  x  x        x     x     x    ";
    " x  x  x     x        x     x  x  x  x  x  x  x           x     x          ";
    " x  x        x        x     x     x     x     x                            ";
    "                                                                         x ";
    "                                                                           ";
    " x  x  x              x  x  x  x        x  x  x           x  x  x          ";
    " x  x  x              x  x  x  x        x  x  x           x  x  x          ";
    "             x                                                             ";
    " x  x  x     x              x  x              x           x  x  x          ";
    " x  x        x        x     x                                              ";
    " x  x  x     x        x     x  x                          x     x          ";
    " x  x                 x           x                                        ";
    " x  x  x              x           x  x                    x                ";
    " x  x        x                          x                                  ";
    " x  x  x     x                          x  x              x                ";
    " x  x  x  x  x        x     x     x     x     x  x  x  x  x     x        x ";
    " x                                            x  x  x                      ";
    "                                                                           ";
    "                                                                           ";
    " x  x        x        x     x     x     x     x        x     x             ";
    " x  x        x        x     x     x     x     x        x     x             ";
    "             x        x                                x  x     x          ";
    "                                                                   x       ";
    " x                                                                    x    ";
    " x  x  x     x                                x                            ";
  |]

exception BREAK of int (* Internal exception *)


let char_order_checker_for_pic_string config =
  (* From GnuCOBOL, itself inspired by the standard's way of specifying
     precedence. *)
  let seen = Array.make 25 false in
  let check_char_order symbol_precedence =
    try
      Array.iteri begin fun i -> function
        | false when seen.(i) -> raise @@ BREAK i
        | _ -> ()
      end precedence_table.(symbol_precedence);
      seen.(symbol_precedence) <- true;
      Ok ()
    with BREAK prev_precedence ->
      seen.(symbol_precedence) <- true;
      let diag =
        let decimal_char = config.decimal_char in
        if symbol_precedence = prev_precedence then
          May_only_appear_once { symbol_precedence; decimal_char }
        else
          May_not_follow { symbol_precedence; prev_precedence; decimal_char }
      in
      Error diag
  in
  let reset () =
    Array.(fill seen 0 (length seen) false)
  in
  check_char_order, reset

(* Maybe not in ISO/IEC 2014: Z/CS, B/* *)
let mutual_exclusions =
  SymbolsMap.of_seq @@ List.to_seq [
    B, Symbols.singleton Star;
    CS, Symbols.singleton Z;
    DecimalSep, Symbols.of_list [P; V];
    P, Symbols.singleton DecimalSep;
    Star, Symbols.of_list [Z; B];
    V, Symbols.singleton DecimalSep;
    Z, Symbols.of_list [Star; CS];
  ]

type exp_sequence_state =
  | ExpNone
  | ExpWaitingPlus
  | ExpWaitingDigits

type acc =
  {
    v_idx: int option;            (* Some _ => V, P, or DecimalSep \in seen *)
    e_idx: int option;
    seen: Symbols.t;                           (* Records only some symbols *)
    exp_sequence: exp_sequence_state;
    errors: (error * (int*int)) list;
  }

let with_error acc loc error = { acc with errors = (error, loc) :: acc.errors }

module SCANNING = struct
  type 'a string_parser = string -> 'a

  let mk_parser spec cstr : _ string_parser = fun str ->
    Scanf.sscanf str spec cstr

  let try_parse (specs: 'a string_parser list) str pos len =
    let str = String.sub str pos (len-pos) in
    let rec try_parse = function
      | [] -> None
      | f :: tl -> try Some (f str) with
        | End_of_file | Scanf.Scan_failure _ -> try_parse tl
    in
    try_parse specs


  let single symbol =
    { symbol; symbol_occurences = 1 }

  let rec pic_symbol ?expect ~config s pos len =

    let lookahead c n l suff =
      let pos = len - String.length suff in
      let c = Char.uppercase_ascii c in
      let symbol = symbol_of_char config c in
      match expect with
      | Some cc when cc <> c ->
          { symbol; symbol_occurences = n }, l
      | _ ->
          (* look ahead for further occurences *)
          match pic_symbol ~expect:c ~config s pos len with
          | Some ({ symbol = s'; symbol_occurences = n' }, l')
            when s' = symbol ->
              { symbol; symbol_occurences = n + n' }, l' + l
          | _ ->
              { symbol; symbol_occurences = n }, l
          | exception INVALIDCHAR _ ->            (* delay for better location *)
              { symbol; symbol_occurences = n }, l
    in
    if pos = len then
      None
    else
      try_parse [
        (* NOTE: those scanners allow spaces to be inserted in PICTURE
             strings; this should be ok as parsed tokens should contain no
             space characters; plus this may allow more readable string
             internally. *)
        mk_parser "CR%s%!" (fun _suff -> single CR, 2);
        mk_parser "DB%s%!" (fun _suff -> single DB, 2);
        mk_parser "%c(%u)%n%s%!" lookahead;
        mk_parser "%c%s%!" (fun c -> lookahead c 1 1)
      ] s pos len


end


let of_string config str =

  let len = String.length str in
  let next_symbols pos acc =
    (* Lookup and characterize next sequence of consecutive characters: *)
    match SCANNING.pic_symbol ~config str pos len with
    | None when pos = len ->                 (* end of PICTURE string *)
        None, acc
    | None ->
        None,
        let c = str.[pos] in
        if c = '('
        then with_error acc (pos,1)
            Parenthesis_must_be_preceded_by_picture_symbol
        else with_error acc (pos,1) (Unexpected_char c)
    | Some (symbols, span) ->
        let pos' = pos + span in
        Some (symbols, (pos,span), pos'), acc
    | exception INVALIDCHAR c ->
        None,
        with_error acc (pos, 1) (Unexpected_char c)
  in

  let rec of_string_rec acc category pic idx pos =
    match next_symbols pos acc with
    | None, acc ->
        category, pic, acc, idx - 1                             (* all done *)
    | Some (symbols, (_, span as loc), pos'), acc ->
        let after_v = acc.v_idx <> None in
        let acc, ok = match check_occurences acc symbols loc with
          | Ok acc -> acc, true
          | Error acc -> acc, false
        in
        let acc, ok = match check_positions acc symbols loc idx pos' with
          | Ok acc -> acc, ok
          | Error acc -> acc, false
        in
        let acc, ok = match check_mutual_exclutions acc symbols loc with
          | Ok acc -> acc, ok
          | Error acc -> acc, false
        in
        if ok then
          let category = append ~after_v category symbols in
          let acc = match symbols.symbol with
            | V | DecimalSep when acc.v_idx = None ->
                { acc with v_idx = Some idx }
            | P when acc.v_idx = None ->
                let v_idx = match category with
                  | Ok c | Error (Some c, _)
                    when data_size c = symbols.symbol_occurences -> idx - 1
                  | Error (None, _) -> idx - 1
                  | _ -> idx + symbols.symbol_occurences
                in
                { acc with v_idx = Some v_idx }
            | E ->
                { acc with e_idx = Some idx; exp_sequence = ExpWaitingPlus }
            | Plus when acc.exp_sequence = ExpWaitingPlus ->
                { acc with exp_sequence = ExpWaitingDigits }
            | Plus when acc.exp_sequence = ExpWaitingDigits ->
                with_error acc loc Extraneous_symbol_in_exponent
            | _ ->
                acc
          in
          check acc ~loc category (symbols :: pic) (idx + span) pos'
        else                                              (* skip symbol(s) *)
          of_string_rec acc category pic (idx + span) pos'

  and check_occurences acc symbols loc =
    match symbols.symbol with                           (* check occurences *)
    | CR | DB | E | S | V | L | DecimalSep as s ->
        if Symbols.mem s acc.seen || symbols.symbol_occurences > 1
        then Error (with_error acc loc (Symbol_may_only_appear_once s))
        else Ok { acc with seen = Symbols.add s acc.seen }
    | P | Z | Star |CS | B as s ->           (* record for mutual exclutions *)
        Ok { acc with seen = Symbols.add s acc.seen }
    | _ ->
        Ok acc

  and check_positions acc symbols loc idx pos =
    match symbols.symbol with    (* check for some positions (quite ad hoc) *)
    | L | S as s when idx <> 0 ->
        Error (with_error acc loc (Symbol_must_be_at_start s))
    | CR | DB as s when pos < len ->
        Error (with_error acc loc (Symbol_must_be_at_end s))
    | P when idx = 0 ||
             idx = 1 && Symbols.(mem S acc.seen || mem V acc.seen) ||
             idx = 2 && Symbols.(mem S acc.seen && mem V acc.seen) ||
             pos = len || String.sub str pos (len-pos) = "V" ->
        Ok acc
    | P as s ->
        Error (with_error acc loc (Symbol_must_be_at_start_or_end s))
    | _ ->
        Ok acc

  and check_mutual_exclutions acc symbols loc =
    match SymbolsMap.find_opt symbols.symbol mutual_exclusions with
    | None ->
        Ok acc
    | Some set ->
        let violations = Symbols.inter set acc.seen in
        if Symbols.is_empty violations then Ok acc else
          Result.error @@ Symbols.fold (fun s acc ->
              with_error acc loc (Symbols_are_mutually_exclusive (symbols.symbol, s))
            ) violations acc

  and check acc ~loc category' pic idx suff =
    match category' with
    | Ok category ->
        of_string_rec acc (Some category) pic idx suff
    | Error (c, s) ->
        c, pic,
        with_error acc loc (Unexpected_symbol (s, c)),
        idx - 1
  in
  let category, pic, acc, max_idx =
    of_string_rec {
      v_idx = None;
      e_idx = None;
      exp_sequence = ExpNone;
      seen = Symbols.empty;
      errors = [];
    } None [] 0 0
  in
  let loc = (0, len) in
  (* Last remaining checks; global checks should come here. *)
  let acc = match pic with
    | [] ->
        with_error acc loc Empty_picture_string
    | _ when len > config.max_pic_length ->
        with_error acc loc
          (Picture_length_exceeds_limit (len, config.max_pic_length))
    | _ -> acc
  in

  let acc = match acc.exp_sequence with                       (* (E+ check) *)
    | ExpWaitingPlus ->
        with_error acc loc Missing_symbol_in_exponent
    | _ -> acc
  in
  let acc = match category with
    | Some (FloatNum { exponent_digits = 0; _ }) ->
        with_error acc loc Missing_digits_in_exponent
    | _ -> acc
  in

  let acc = match Option.map (fun c -> c, data_size c) category with
    | None ->
        acc
    | Some (_, 0) ->
        (* In GnuCOBOL, note the `U` in: "PICTURE string must contain at least
           one of the set A, N, U, X, Z, 1, 9 and *; or at least two of the
           set +, - and the currency symbol" *)
        with_error acc loc Picture_describes_empty_data_item
    | Some ((FixedNum _ | FloatNum _), data_size) when data_size > 38 ->
        with_error acc loc (Numeric_item_cannot_exceed_38_digits data_size)
    | _ -> acc
  in

  let pic = List.rev pic in

  let _, acc =
    (* Check precedence rules w.r.t the standards.  This also ensures some
       form of well-formedness of the editions. *)
    let symbol_precedence_index = symbol_precedence_index ~max_idx in
    let check_char_order, reset = char_order_checker_for_pic_string config in
    let floating_symbolp = match category with
      | Some (FixedNum { editions = { floating = Some f; _ }; _ }) ->
          fun p -> p = f.floating_insertion_symbol
      | _ ->
          fun _ -> false
    in
    List.fold_left begin fun (idx, acc) { symbol; symbol_occurences = n } ->
      if symbol = E then reset ();   (* consider exponent string separately *)
      let prec idx =
        symbol_precedence_index symbol ~idx
          ~after_v:(Option.fold ~some:(fun i -> idx > i) ~none:false acc.v_idx)
          ~after_e:(Option.fold ~some:(fun i -> idx > i) ~none:false acc.e_idx)
          ~zero_suppress_or_floating_insert:(floating_symbolp symbol)
      in
      let acc =
        match check_char_order (prec idx) with
        | Error diag -> with_error acc loc diag
        | Ok () when n = 1 -> acc
        | Ok () ->                        (* check twice on repeated symbols *)
            match check_char_order (prec (idx + n - 1)) with
            | Error diag -> with_error acc loc diag
            | Ok () -> acc
      in
      idx + n, acc
    end (0, acc) pic
  in

  let pic =
    let default = Alphanumeric { length = 0; insertions = [] } in
    {
      category = Option.fold ~some:reverse_editions ~none:default category;
      pic;
    }
  in
  match acc.errors with
  | [] -> Ok pic
  | errors -> Error (errors, pic)


let alphanumeric ~size =
  {
    category = Alphanumeric { length = size; insertions = [] };
    pic = [{ symbol = X; symbol_occurences = size }];
  }


let pp_meaning_of_precedence_index ~decimal_char ppf = function
  | 0 -> Fmt.pf ppf "B, 0 or /"
  | 1 -> Fmt.pf ppf "grouping@ separator ('%c')"
           (if decimal_char = '.' then ',' else '.')
  | 2 -> Fmt.pf ppf "decimal@ point ('%c')" decimal_char
  | 3 -> Fmt.pf ppf "the sign of floating exponent"
  | 4 -> Fmt.pf ppf "a leading +/- sign"
  | 5 -> Fmt.pf ppf "a trailing +/- sign"
  | 6 -> Fmt.pf ppf "CR or DB"
  | 7 -> Fmt.pf ppf "a leading currency symbol"
  | 8 -> Fmt.pf ppf "a trailing currency symbol"
  | 9 -> Fmt.pf ppf "a Z or * which is before the decimal point"
  | 10 -> Fmt.pf ppf "a Z or * which is after the decimal point"
  | 11 -> Fmt.pf ppf "a floating +/- string which is before the decimal point"
  | 12 -> Fmt.pf ppf "a floating +/- string which is after the decimal point"
  | 13 -> Fmt.pf ppf "a floating currency symbol string which is before the \
                      decimal point"
  | 14 -> Fmt.pf ppf "a floating currency symbol string which is after the \
                      decimal point"
  | 15 -> Fmt.pf ppf "9"
  | 16 -> Fmt.pf ppf "A or X"
  | 17 -> Fmt.pf ppf "L"
  | 18 -> Fmt.pf ppf "S"
  | 19 -> Fmt.pf ppf "V"
  | 20 -> Fmt.pf ppf "a P which is before the decimal point"
  | 21 -> Fmt.pf ppf "a P which is after the decimal point"
  | 22 -> Fmt.pf ppf "1"
  | 23 -> Fmt.pf ppf "N"
  | 24 -> Fmt.pf ppf "E"
  | _ as d -> Fmt.pf ppf "an unkown character of value: %d" d

let pp_error ppf error =
  match error with
  | May_only_appear_once { symbol_precedence; decimal_char } ->
      Format.fprintf ppf
        "%a@ may@ only@ appear@ once@ in@ a@ PICTURE@ string"
        (pp_meaning_of_precedence_index ~decimal_char) symbol_precedence
  | May_not_follow { symbol_precedence; prev_precedence; decimal_char } ->
      Format.fprintf ppf
        "%a@ may@ not@ follow@ %a"
        (pp_meaning_of_precedence_index ~decimal_char) symbol_precedence
        (pp_meaning_of_precedence_index ~decimal_char) prev_precedence
  | Parenthesis_must_be_preceded_by_picture_symbol ->
      Format.fprintf ppf
        "Parenthesis@ must@ be@ preceded@ by@ a@ picture@ symbol"
  | Unexpected_char c ->
      Format.fprintf ppf
        "Unexpected@ character@ %c@ in@ PICTURE@ string" c
  | Extraneous_symbol_in_exponent ->
      Format.fprintf ppf "Extraneous@ +@ symbol(s)@ in@ exponent"
  | Symbol_may_only_appear_once s ->
      Format.fprintf ppf "%a@ may@ only@ occur@ once@ in@ a@ \
                          PICTURE@ string" pp_symbol s
  | Symbol_must_be_at_start s ->
      Format.fprintf ppf "%a@ must@ be@ at@ start@ of@ PICTURE@ string"
        pp_symbol s
  | Symbol_must_be_at_end s ->
      Format.fprintf ppf "%a@ must@ be@ at@ end@ of@ PICTURE@ string"
        pp_symbol s
  | Symbol_must_be_at_start_or_end s ->
      Format.fprintf ppf
        "%a@ must@ be@ at@ start@ or@ end@ of@ PICTURE@ string"
        pp_symbol s
  | Symbols_are_mutually_exclusive (s1, s2) ->
      Format.fprintf ppf "%a@ and@ %a@ are@ mutually@ exclusive@ in@ a@ \
                          PICTURE@ string"
        pp_symbol s1 pp_symbol s2
  | Unexpected_symbol (s, c) ->
      Format.fprintf ppf "Unexpected@ %a@ in@ PICTURE@ string%a"
        pp_symbol s
        Fmt.(option (fun ppf -> fmt "@ of@ category@ %a" ppf pp_category))
        (Option.map reverse_editions c)
  | Empty_picture_string ->
      Format.fprintf ppf "Empty@ PICTURE@ string"
  | Picture_length_exceeds_limit (len, max_length) ->
      Format.fprintf ppf
        "length@ of@ PICTURE@ string@ exeeds@ allowed@ size@ (max@ length:@ \
         %d,@ given@ length:%d)" max_length len
  | Missing_symbol_in_exponent ->
      Format.fprintf ppf "Missing@ +@ symbol@ in@ exponent"
  | Missing_digits_in_exponent ->
      Format.fprintf ppf "Missing@ digits@ in@ exponent"
  | Picture_describes_empty_data_item ->
      Format.fprintf ppf
        "PICTURE@ string@ describes@ an@ empty@ data@ item"
  | Numeric_item_cannot_exceed_38_digits len ->
      Format.fprintf ppf
        "Numeric@ item@ cannot@ be@ longer@ than@ 38@ digits, %d found" len

module DIAGS = Cobol_common.Diagnostics

let add_diag acc ~loc fmt =
  DIAGS.kerror ~loc (fun diag -> DIAGS.Set.cons diag acc) fmt

let add_hint acc ~loc fmt =
  DIAGS.khint ~loc (fun diag -> DIAGS.Set.cons diag acc) fmt

let add_diags acc ~loc error =
  let acc = add_diag acc ~loc "%a" pp_error error in
  match error with
  | Picture_describes_empty_data_item ->
      add_hint acc ~loc
        "PICTURE@ string@ must@ contain@ at@ least@ one@ of@ the@ set@ A,@ N,@ \
         X,@ Z,@ 1,@ 9@ and@ *;@ or@ at@ least@ two@ of@ the@ set@ +,@ -@ and@ \
         the@ currency@ symbol"
  | _ -> acc

let rev_errors_with_loc ~loc errors =
  List.rev_map begin fun (error, (pos, len)) ->
    { payload = error; loc = Srcloc.sub loc ~pos ~len }
  end errors

let error_diagnostics ~loc errors =
  (* reverse [errors] first to emit in proper order *)
  List.fold_left begin fun acc (error, (pos, len)) ->
    add_diags acc ~loc:(Srcloc.sub loc ~pos ~len) error
  end DIAGS.Set.none (List.rev errors)

module Make (Config: Cobol_config.T) (Env: ENV) = struct

  exception InvalidPicture of
      string with_loc * Cobol_common.Diagnostics.diagnostics * picture

  let of_string ({ payload; loc } as str) =

    let config = {
      max_pic_length = Config.pic_length#value;
      decimal_char = Env.decimal_char;
      currency_signs = Env.currency_signs;
    } in
    match of_string config payload with
    | Ok pic ->
        { payload = pic; loc }
    | Error (errors, pic) ->
        raise @@ InvalidPicture (str, error_diagnostics ~loc errors, pic)

end

let config = { max_pic_length = 100; decimal_char = '.';
               currency_signs = CHARS.add '$' CHARS.empty }

let unit_test ?(config=config) ~expect picture =
  let ppf = Format.str_formatter in
  begin
    match of_string config picture with
    | Ok pic ->
        pp_picture ppf pic;
    | Error (errors, _) ->
        List.iter (fun (error, (pos, len))->
            Format.fprintf ppf "Loc: %d (%d)@." pos len;
            pp_error ppf error;
            Format.fprintf ppf "@.";
          ) errors
  end;
  let res = Format.flush_str_formatter () in
  if res <> expect then begin
    Printf.eprintf "Unit test %S failed:\n%!" picture;
    Printf.eprintf "  Result:\n";
    Printf.eprintf "{|%s|} ;\n" res;
    Printf.eprintf "  Expected:\n";
    Printf.eprintf "{|%s|} ;\n%!" expect;
    false
  end
  else
    true
