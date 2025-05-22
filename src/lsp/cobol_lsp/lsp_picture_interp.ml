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

open Cobol_data.Picture
open TYPES


let simple_insertion_char_of ~symbol =
  match symbol with
  | B -> ' '
  | Zero -> '0'
  | Slant -> '/'
  | DecimalSep -> '.'
  | GroupingSep -> ','
  | _ -> Pretty.invalid_arg
           "Not a simple insertion symbol '%a'"
           pp_symbol symbol

let fixed_insertion_str_of ~symbol ~is_negative =
  match symbol with
  | CS -> "$"
  | Plus | Minus when is_negative -> "-"
  | Plus -> "+"
  | Minus -> " "
  | CR | DB when not is_negative -> "  "
  | CR -> "CR"
  | DB -> "DB"
  | _ -> Pretty.invalid_arg
           "Not a fixed insertion symbol '%a'"
           pp_symbol_cobolized symbol

let do_basic_edit_on ~is_negative basic s =
  let (offset, insertion) =
    match basic with
    | SimpleInsertion
        { simple_insertion_symbols = { symbol_occurences = n; symbol };
          simple_insertion_offset = offset } ->
      offset, String.make n @@ simple_insertion_char_of ~symbol
    | SpecialInsertion { special_insertion_offset = offset } ->
      offset, "."
    | FixedInsertion { fixed_insertion_symbol = symbol; fixed_insertion_offset = offset } ->
      offset, fixed_insertion_str_of ~symbol ~is_negative
  in
  Str.string_before s offset
  ^ insertion
  ^ Str.string_after s offset

let all_repl_indexes_from ~ranges s digits =
  let indexes =
    ranges
    |> List.rev_map begin fun { floating_range_offset = offset;
                                floating_range_length = len } ->
      List.init len (fun i -> offset + i)
    end
    |> List.flatten |> List.sort Int.compare
  in
  let is_only_repl_char = List.length indexes >= digits in
  let min_index = List.hd indexes in
  let (_, (all_indexes, all_zero)) = String.fold_left
      begin fun ((idx, (acc_repl, should_continue_repl)) as acc) ch ->
        if not should_continue_repl then acc else
        if List.mem idx indexes
        then (idx+1,
              if ch == '0'
              then (idx::acc_repl, true)
              else (acc_repl, false))
        else
        if min_index < idx && List.mem ch [' '; ',']
        then (idx+1, (idx::acc_repl, true))
        else (idx+1, (acc_repl, should_continue_repl))
      end (0, ([], true)) s
  in all_indexes, all_zero && is_only_repl_char

let do_floatedit_n_zerorepl_on digits is_negative
    symbol ranges s =
  if ranges == [] then s else
    let floating_last_ch = match symbol with
      | Plus | Minus when is_negative -> '-'
      | Plus -> '+'
      | Minus -> ' '
      | CS -> '$'
      | Z -> ' '
      | Star -> '*'
      | _ -> Pretty.invalid_arg
               "Floating edit or zero replacement symbol '%a' is invalid"
               pp_symbol_cobolized symbol
    in
    let repl_ch = match symbol with
      | Minus | Plus | CS | Z -> ' '
      | Star -> '*'
      | _ -> Pretty.invalid_arg
               "Floating edit or zero replacement symbol '%a' is invalid"
               pp_symbol_cobolized symbol
    in
    let repl_str = String.make 1 repl_ch in
    let all_repl_indexes, repl_everything =
      all_repl_indexes_from ~ranges s digits in
    if repl_everything
    then
      String.map begin fun ch ->
        if ch == '.' && symbol == Star
        then '.'
        else repl_ch
      end s
    else
      let (_, _, last_repl_idx, res) = String.fold_left
          begin fun (i, after_decimal_point, last_repl_idx, res) ch ->
            let orig_str = String.make 1 ch in
            if after_decimal_point
            then (i+1, after_decimal_point, last_repl_idx, res ^ orig_str)
            else
            if ch == '.'
            then (i+1, true, last_repl_idx, res ^ ".")
            else
            if List.mem i all_repl_indexes
            then (i+1, after_decimal_point, i, res ^ repl_str)
            else (i+1, after_decimal_point, last_repl_idx, res ^ orig_str)
          end (0, false, -1, "") s
      in
      String.mapi begin fun i ch ->
        if i == last_repl_idx
        then floating_last_ch
        else ch
      end
        res

let rec edit_basics ~is_negative basics s =
  match basics with
  | [] -> s
  | hd::tl ->
    do_basic_edit_on ~is_negative hd s
    |> edit_basics ~is_negative tl

let simple_example_of ~digits ~scale ~with_dot value =
  let str_val = string_of_float (Float.abs value) in
  let i = String.index str_val '.' in
  let whole_part = Str.string_before str_val i in
  let whole_len = String.length whole_part in
  let floating_part = Str.string_after str_val (i+1) in
  let required_len = digits - scale in
  (String.init required_len
     (fun i ->
        if i < required_len - whole_len
        then '0'
        else whole_part.[i - (required_len - whole_len)])
   )
  ^ (if scale > 0
     then
       (if with_dot then "." else "")
       ^ String.init scale
         (fun i ->
            if i < String.length floating_part
            then floating_part.[i]
            else '0')
     else "")

let example_of ~picture value =
  if List.exists (fun { symbol; _ } -> symbol == P) picture.pic
  then raise @@ Invalid_argument "No example with P yet" (* /!\ scale can be negative: PIC 9P *)
  else
    match picture.category with
    | Alphabetic _ | Boolean _ | National _ | Alphanumeric _ -> ""
    | FloatNum _ -> raise @@ Invalid_argument "No example for floatnum yet"
    | FixedNum { digits; scale; with_sign; _ }
      when not @@ is_edited picture  ->
      (if with_sign then "+" else "")
      ^ simple_example_of ~digits ~scale ~with_dot:true value
    | FixedNum { digits; scale; with_sign;
                 editions = { basics; floating; zerorepl } } ->
      ignore (with_sign);
      let is_negative = value < 0. in
      let edit_zerorepl = Option.fold ~none:Fun.id
          ~some:(fun { zero_replacement_symbol = symbol;
                       zero_replacement_ranges = ranges } ->
                  do_floatedit_n_zerorepl_on digits is_negative symbol ranges)
          zerorepl in
      let edit_floating = Option.fold ~none:Fun.id
          ~some:(fun { floating_insertion_symbol = symbol;
                       floating_insertion_ranges = ranges } ->
                  do_floatedit_n_zerorepl_on digits is_negative symbol ranges)
          floating in
      try
        (if Option.is_some floating
         then "0"
         else "")
        ^ simple_example_of ~digits ~scale ~with_dot:false value
        |> edit_basics ~is_negative:(value < 0.) basics
        |> edit_zerorepl
        |> edit_floating
      with Invalid_argument e ->
        Pretty.invalid_arg
          "Unable to build example of picture, error '%s'" e
