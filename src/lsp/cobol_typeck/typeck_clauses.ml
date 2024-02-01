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

open Cobol_data.Types
open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX
open Diagnostics

module PIC = Cobol_data.Picture

type data_clauses =
  {
    occurs: Cobol_ptree.Types.data_occurs_clause with_loc option;
    usage: Cobol_ptree.Types.usage_clause with_loc option;
    picture: Cobol_ptree.Types.picture_clause with_loc option;
    value: Cobol_ptree.Types.data_value_clause with_loc option;
    redefines: Cobol_ptree.Types.name with_loc option;
    clause_diags: diagnostics;
  }


let init_clauses =
  {
    occurs = None;
    usage = None;
    picture = None;
    value = None;
    redefines = None;
    clause_diags = [];
  }


let clause_error acc error =
  { acc with clause_diags = Data_error error :: acc.clause_diags }

let clause_warn acc warn =
  { acc with clause_diags = Data_warning warn :: acc.clause_diags }

let clause_diagnostic acc diag =
  { acc with clause_diags = diag :: acc.clause_diags }

let register_used_feature acc ~loc:usage_loc ~feature =
  clause_diagnostic acc @@ Dialect_feature_used { feature; usage_loc }


let on_unique_clause ~clause_name ~f prev_val acc clause =
  match prev_val with
  | Some { loc; _ } ->
      clause_warn acc (Duplicate_clause { clause_name;
                                          first_loc = loc;
                                          second_loc = ~@clause })
  | None ->
      f acc clause


let on_occurs_clause acc =
  on_unique_clause ~clause_name:"OCCURS" acc.occurs acc
    ~f:(fun acc clause -> { acc with occurs = Some clause })


let on_usage_clause acc =
  on_unique_clause ~clause_name:"USAGE" acc.usage acc
    ~f:(fun acc clause -> { acc with usage = Some clause })


let on_picture_clause acc =
  on_unique_clause ~clause_name:"PICTURE" acc.picture acc
    ~f:(fun acc clause -> { acc with picture = Some clause })


let on_value_clause acc =
  on_unique_clause ~clause_name:"VALUE" acc.value acc
    ~f:(fun acc clause -> { acc with value = Some clause })


let on_redefines_clause acc =
  on_unique_clause ~clause_name:"REDEFINES" acc.redefines acc
    ~f:begin fun acc clause ->
      let acc =
        if acc != init_clauses then  (* note: hackish use of physical equality *)
          register_used_feature acc
            ~feature:Cobol_config.Options.free_redefines_position
            ~loc:~@clause
        else acc
      in
      { acc with redefines = Some ~&clause }
    end


let of_data_item (data_clauses: Cobol_ptree.Types.data_clause with_loc list) =
  List.fold_left begin fun acc { payload = clause; loc } ->
    match (clause: Cobol_ptree.Types.data_clause) with
    | DataOccurs    o -> on_occurs_clause acc (o &@ loc)
    | DataRedefines r -> on_redefines_clause acc (r &@ loc)
    | DataUsage     u -> on_usage_clause acc (u &@ loc)
    | DataPicture   p -> on_picture_clause acc p
    | DataValue     d -> on_value_clause acc (d &@ loc)
    | _ -> acc
  end init_clauses data_clauses


(* --- *)

let translate_picture_clause
    ~picture_config
    { payload = Cobol_ptree.Types.{ picture;
                              picture_locale = _;
                              picture_depending = _ }; loc = _ } =
  match PIC.(of_string picture_config ~&picture) with
  | Ok pic ->
      Ok (pic &@<- picture)
  | Error (errors, _) ->                    (* note: errors are still reversed *)
      PIC.rev_errors_with_loc ~loc:~@picture errors |>
      List.fold_left begin fun acc err ->
        Data_error (Picture_error { picture_loc = ~@picture; error = err })
        :: acc
      end [] |>
      Result.error


let display_usage_from_literal: Cobol_ptree.Types.literal -> usage =
  (* TODO: `Display|`National *)
  let detect_sign i =
    if EzString.starts_with ~prefix:"-" i
    then true, String.length i - 1
    else false, String.length i
  in
  function
  | Alphanum { str; hexadecimal = false; _ } ->
      Display (PIC.alphanumeric ~size:(String.length str))
  | Alphanum { str; hexadecimal = true; _ } ->
      Display (PIC.alphanumeric ~size:(String.length str / 2))
  | Boolean { bool_base = `Bool; bool_value } ->
      Display (PIC.boolean (String.length bool_value))
  | Boolean { bool_base = `Hex; bool_value } ->
      Display (PIC.boolean (String.length bool_value * 4))
  | Integer i ->
      let with_sign, digits = detect_sign i in
      Display (PIC.fixed_numeric ~with_sign digits 0)
  | Fixed { fixed_integer; fixed_fractional } ->
      let with_sign, int_digits = detect_sign fixed_integer
      and frac_digits = String.length fixed_fractional in
      Display (PIC.fixed_numeric ~with_sign int_digits frac_digits)
  | _ ->                                                            (* TODO... *)
      Display (PIC.alphanumeric ~size:1)


let data_error diags e = Data_error e :: diags


let ensure_picture diags
    ?(only: [`Numeric_category |
             `Boolean_class |
             `Nonalpha_class |
             `Any_class] = `Any_class)
    ~(usage_clause: Cobol_ptree.Types.usage_clause) picture =
  let pic = match picture with
    | Some Ok pic ->
        Ok pic
    | Some Error pic ->                         (* grab the raw length for now *)
        Error (`Length (String.length ~&(~&pic.Cobol_ptree.Types.picture)))
    | None ->
        Error `None
  in
  let guess_picture pic diags =
    let pic_len = match pic with
      | `Picture pic -> PIC.data_size ~&pic
      | `Length pic_len -> pic_len
      | `None -> 1                              (* in case no picture is given *)
    in
    diags, match usage_clause with
    | Bit ->
        PIC.boolean pic_len
    | Binary
    | PackedDecimal
    | Index ->
        PIC.digits pic_len
    | Display ->
        PIC.alphanumeric ~size:pic_len
    | National ->
        PIC.national ~size:pic_len
    | _ ->                      (* TODO: recover with more advanced heuristics *)
        PIC.alphanumeric ~size:pic_len                             (* for now *)
  in
  match pic, only with
  | Ok pic, `Numeric_category
    when PIC.is_numeric ~&pic && not (PIC.is_edited ~&pic) ->
      diags, ~&pic
  | Ok pic, `Boolean_class
    when PIC.is_boolean ~&pic ->
      diags, ~&pic
  | Ok pic, `Nonalpha_class
    when PIC.is_numeric ~&pic || PIC.is_boolean ~&pic || PIC.is_national ~&pic ->
      diags, ~&pic
  | Ok pic, `Any_class ->
      diags, ~&pic
  | Ok pic, (`Numeric_category | `Boolean_class | `Nonalpha_class as expected) ->
      guess_picture (`Picture pic) @@
      data_error diags @@ Incompatible_picture { picture = pic; usage_clause;
                                                 expected }
  | Error pic_len, _ ->
      guess_picture pic_len diags


let auto_usage diags ~usage_clause picture =
  match (usage_clause: Cobol_ptree.Types.usage_clause) with
  | Binary ->
      let diags, picture
        = ensure_picture diags ~only:`Numeric_category ~usage_clause picture in
      diags, Some (Binary picture)
  | Bit ->
      let diags, picture
        = ensure_picture diags ~only:`Boolean_class ~usage_clause picture in
      diags, Some (Bit picture)
  | Display ->
      let diags, picture
        = ensure_picture diags ~only:`Any_class ~usage_clause picture in
      diags, Some (Display picture)
  | National ->
      let diags, picture
        = ensure_picture diags ~only:`Nonalpha_class ~usage_clause picture in
      diags, Some (National picture)
  | PackedDecimal ->
      let diags, picture
        = ensure_picture diags ~only:`Numeric_category ~usage_clause picture in
      diags, Some (Packed_decimal picture)
  | _ ->
      diags, None



let to_usage_n_value ~item_name ~item_loc ~picture_config item_clauses =
  let diags = [] in
  let diags, value = match item_clauses.value with
    | Some { payload = ValueTable _; loc = value_loc } ->
        data_error diags @@ Unexpected_table_value_clause { item_name;
                                                            value_loc },
        None
    | Some { payload = ValueData literal; _ } ->
        diags, Some literal
    | None ->
        diags, None
  in
  let diags, picture =
    match item_clauses.picture with
    | None ->
        diags, None
    | Some picture_clause ->
        match translate_picture_clause ~picture_config picture_clause with
        | Ok pic ->
            diags, Some (Ok pic)
        | Error diags' ->
            diags' @ diags, Some (Error picture_clause)
  in
  let usage_clause = match item_clauses.usage with
    | Some usage ->
        ~&usage
    | None ->                      (* fallback to DISPLAY *)
        Display                   (* TODO: NATIONAL in case value is a natlit *)
  in
  let signedness s =
    Cobol_data.Types.{ signed = s <> Some Cobol_ptree.Types.Unsigned }
  and endian =     (* TODO: set default via FLOAT-BINARY in OPTIONS paragraph *)
    Option.value ~default:Cobol_ptree.Types.HighOrderLeft
  and encoding =  (* TODO: set default via FLOAT-DECIMAL in OPTIONS paragraph *)
    Option.value ~default:Cobol_ptree.Types.DecimalEncoding
  in
  let diags, usage = match usage_clause with

    | Binary ->
        auto_usage diags ~usage_clause picture

    | BinaryChar s ->
        diags, Some (Binary_char (signedness s))

    | BinaryDouble s ->
        diags, Some (Binary_double (signedness s))

    | BinaryLong s ->
        diags, Some (Binary_long (signedness s))

    | BinaryShort s ->
        diags, Some (Binary_short (signedness s))

    | Bit ->
        auto_usage diags ~usage_clause picture

    | Display ->
        begin match picture, value with
          | None, None ->
              diags, None
          | None, Some value ->
              diags, Some (display_usage_from_literal ~&value)
          | Some _, _ ->
              auto_usage diags ~usage_clause picture
        end

    | FloatBinary32 e ->
        diags, Some (Float_binary { width = `W32;
                                    endian = endian e })

    | FloatBinary64 e ->
        diags, Some (Float_binary { width = `W64;
                                    endian = endian e })

    | FloatBinary128 e ->
        diags, Some (Float_binary { width = `W128;
                                    endian = endian e })

    | FloatDecimal16 { encoding_endianness = e;
                       encoding_mode = c } ->
        diags, Some (Float_decimal { width = `W16;
                                     endian = endian e;
                                     encoding = encoding c })

    | FloatDecimal34 { encoding_endianness = e;
                       encoding_mode = c } ->
        diags, Some (Float_decimal { width = `W34;
                                     endian = endian e;
                                     encoding = encoding c })

    | FloatExtended ->
        diags, Some Float_extended

    | FloatLong ->
        diags, Some Float_long

    | FloatShort ->
        diags, Some Float_short

    | FunctionPointer f ->
        diags, Some (Function_pointer f)

    | Index ->                                 (* TODO: check value \in Z (N?) *)
        diags, Some Index

    | National -> (* <- TODO: better handling of NATIONAL (partial in GnuCOBOL) *)
        auto_usage diags ~usage_clause picture

    | ObjectReference r ->
        diags, Some (Object_reference r)

    | PackedDecimal ->
        auto_usage diags ~usage_clause picture

    | Pointer p ->
        diags, Some (Pointer p)

    | ProgramPointer p ->
        diags, Some (Program_pointer p)

    (* TODO: customizable USAGE mapping *)
    | UsagePending `BinaryCLong s ->
        diags, Some (Binary_C_long (signedness s))
    | UsagePending `Comp1 ->
        diags, Some Float_short
    | UsagePending `Comp2 ->
        diags, Some Float_long
    | UsagePending `Comp3 ->                  (* == Packed_decimal in GnuCOBOL *)
        auto_usage diags ~usage_clause:PackedDecimal picture

    | _ ->                         (* FIXME: we ignore the other cases for now *)
        diags, None
  in
  let diags = match usage, item_clauses.picture with
    | _, None
    | None, Some _
    | Some (Binary _ |
            Bit _ |
            Display _ |
            National _ |
            Packed_decimal _), Some _ ->
        diags
    | Some (Binary_C_long _ |
            Binary_char _ |
            Binary_double _ |
            Binary_long _ |
            Binary_short _ |
            Float_binary _ |
            Float_decimal _ |
            Float_extended |
            Float_long |
            Float_short |
            Function_pointer _ |
            Index |
            Object_reference _ |
            Pointer _ |
            Program_pointer _), Some picture ->
        data_error diags @@
        Unexpected_picture_clause { picture; item_name; item_loc;
                                    reason = `Item_with_usage usage_clause }
  in
  (* TODO: check value matches usage *)
  diags, usage, value
