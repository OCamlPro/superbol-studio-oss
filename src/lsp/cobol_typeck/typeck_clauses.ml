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

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX
open Cobol_data.Picture.TYPES
open Cobol_data.Types
open Typeck_diagnostics
open Typeck_data_diagnostics

module PIC = Cobol_data.Picture

type data_config =
  {
    picture_config: Cobol_data.Types.picture_config;
    display_sign_config: Cobol_data.Types.display_sign_config;
  }

type data_clauses =
  {
    occurs: Cobol_ptree.data_occurs_clause with_loc option;
    usage: Cobol_ptree.usage_clause with_loc option;
    picture: Cobol_ptree.picture_clause with_loc option;
    value: Cobol_ptree.data_value_clause with_loc option;
    redefines: Cobol_ptree.name with_loc option;
    sign: Cobol_ptree.sign_clause with_loc option;
    data_clause_diags: diagnostics;
  }


type fd_clauses =
  {
    fd_record: Cobol_ptree.record_clause with_loc option;
    fd_clause_diags: diagnostics;
  }

(* --- *)

let no_data_clauses =
  {
    occurs = None;
    usage = None;
    picture = None;
    value = None;
    redefines = None;
    sign = None;
    data_clause_diags = [];
  }

let no_fd_clauses =
  {
    fd_record = None;
    fd_clause_diags = [];
  }


let clause_error acc error =
  { acc with data_clause_diags = Data_error error :: acc.data_clause_diags }

let clause_warn acc warn =
  { acc with data_clause_diags = Data_warning warn :: acc.data_clause_diags }

let clause_diagnostic acc diag =
  { acc with data_clause_diags = diag :: acc.data_clause_diags }

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
        if acc != no_data_clauses then (* note: hackish use of physical equality *)
          register_used_feature acc
            ~feature:Cobol_config.Options.free_redefines_position
            ~loc:~@clause
        else acc
      in
      { acc with redefines = Some ~&clause }
    end


let on_sign_clause acc =
  on_unique_clause ~clause_name:"SIGN" acc.sign acc
    ~f:(fun acc clause -> { acc with sign = Some clause })


let of_data_item (data_clauses: Cobol_ptree.data_clause with_loc list) =
  List.fold_left begin fun acc { payload = clause; loc } ->
    match (clause: Cobol_ptree.data_clause) with
    | DataOccurs    o -> on_occurs_clause acc (o &@ loc)
    | DataRedefines r -> on_redefines_clause acc (r &@ loc)
    | DataUsage     u -> on_usage_clause acc (u &@ loc)
    | DataPicture   p -> on_picture_clause acc p
    | DataValue     d -> on_value_clause acc (d &@ loc)
    | DataSign      s -> on_sign_clause acc (s &@ loc)
    | _ -> acc
  end no_data_clauses data_clauses


(* --- *)


let fd_clause_warn acc warn =
  { acc with fd_clause_diags = Data_warning warn :: acc.fd_clause_diags }


let on_unique_file_clause ~clause_name ~f prev_val acc clause =
  match prev_val with
  | Some { loc; _ } ->
      fd_clause_warn acc (Duplicate_clause { clause_name;
                                             first_loc = loc;
                                             second_loc = ~@clause })
  | None ->
      f acc clause


let on_fd_record_clause acc =
  on_unique_file_clause ~clause_name:"RECORD" acc.fd_record acc
    ~f:(fun acc clause -> { acc with fd_record = Some clause })


let of_fd (file_clauses: Cobol_ptree.file_fd_clause with_loc list) =
  List.fold_left begin fun acc { payload = clause; loc } ->
    match (clause: Cobol_ptree.file_fd_clause) with
    | FileRecord    o -> on_fd_record_clause acc (o &@ loc)
    | _ -> acc
  end no_fd_clauses file_clauses


(* --- *)

let translate_picture_clause ~data_config
    { payload = Cobol_ptree.{ picture_string;
                              picture_locale = _;
                              picture_depending = _ }; loc = _ } =
  match PIC.(of_string data_config.picture_config ~&picture_string) with
  | Ok pic ->
      Ok (pic &@<- picture_string)
  | Error (errors, _) ->                    (* note: errors are still reversed *)
      PIC.rev_errors_with_loc ~loc:~@picture_string errors |>
      List.fold_left begin fun acc err ->
        Data_error (Picture_error { picture_loc = ~@picture_string;
                                    error = err })
        :: acc
      end [] |>
      Result.error


let sign_config_of_clause (sign_clause: Cobol_ptree.sign_clause) =
  {
    sign_separate = sign_clause.sign_separate_character;
    sign_position = match sign_clause with
      | { sign_position = LeadingSign; _ } -> Leading
      | _ -> Trailing;
  }


let display_usage_from_literal: Cobol_ptree.literal -> usage =
  (* TODO: `Display|`National *)
  let detect_sign i =
    if EzString.starts_with ~prefix:"-" i
    then Display_signed Typeck_config.default_display_sign_config,
         String.length i - 1
    else Display_unsigned,
         String.length i
  in
  function
  | Alphanum { str; _ } ->
      let size = String.length str in
      let picture = PIC.alphanumeric ~size in
      Alphanumeric { picture; size }
  | Boolean { bool_base = `Bool; bool_string } ->
      Display_numeric { picture = PIC.boolean (String.length bool_string);
                        sign = Display_unsigned }
  | Boolean { bool_base = `Hex; bool_string } ->
      Display_numeric { picture = PIC.boolean (String.length bool_string * 4);
                        sign = Display_unsigned }
  | Integer i ->
      let sign, digits = detect_sign i in
      let picture = PIC.fixed_numeric ~sign:(sign <> Display_unsigned) digits 0 in
      Display_numeric { picture; sign }
  | Fixed { fixed_integral; fixed_fractional } ->
      let sign, int_digits = detect_sign fixed_integral in
      let picture =
        PIC.fixed_numeric ~sign:(sign <> Display_unsigned) int_digits
          (String.length fixed_fractional)
      in
      Display_numeric { picture; sign }
  | _ ->                                                            (* TODO... *)
      Alphanumeric { picture = PIC.alphanumeric ~size:1; size = 1 }


let data_error diags e = Data_error e :: diags
let data_warning diags e = Data_warning e :: diags


let guess_picture ~(usage: Cobol_ptree.usage_clause) pic_hint =
  let pic_len =
    match pic_hint with
    | `Picture pic -> PIC.data_size ~&pic
    | `Length pic_len -> pic_len
    | `None -> 1                                (* in case no picture is given *)
  in
  match usage with
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
  | _ ->                        (* TODO: recover with more advanced heuristics *)
      PIC.alphanumeric ~size:pic_len                               (* for now *)


let ensure_picture diags
    ?(only: [`Numeric_category |
             `Numeric_or_alphanum_category |
             `Boolean_class |
             `Nonalpha_class |
             `Any_class] = `Any_class)
    ?(required = false)
    ~item_loc
    ?(usage: Cobol_ptree.usage_clause = Display)
    picture
  =
  let pic = match picture with
    | Some Ok pic ->
        Ok pic
    | Some Error pic ->                         (* grab the raw length for now *)
        Error (`Length (String.length ~&(~&pic.Cobol_ptree.picture_string)))
    | None ->
        Error `None
  in
  match pic, only with
  | Ok pic, (`Numeric_category | `Numeric_or_alphanum_category)
    when PIC.is_numeric ~&pic && not (PIC.is_edited ~&pic) ->
      diags, ~&pic
  | Ok pic, `Numeric_or_alphanum_category
    when PIC.is_alphanum ~&pic ->
      diags, ~&pic
  | Ok pic, `Boolean_class
    when PIC.is_boolean ~&pic ->
      diags, ~&pic
  | Ok pic, `Nonalpha_class
    when PIC.is_numeric ~&pic || PIC.is_boolean ~&pic || PIC.is_national ~&pic ->
      diags, ~&pic
  | Ok pic, `Any_class ->
      diags, ~&pic
  | Ok pic, (`Numeric_category | `Numeric_or_alphanum_category |
             `Boolean_class | `Nonalpha_class as expected) ->
      data_error diags @@
      Incompatible_picture { picture = pic; usage; expected },
      guess_picture ~usage @@ `Picture pic
  | Error pic_len, _ ->
      let diags =
        if required then
          data_error diags @@
          Missing_picture_clause_for_item_with_usage { usage; item_loc }
        else diags
      in
      diags, guess_picture ~usage pic_len


let binary_from_numeric_info ?(min_size_is_short = false) ?(native_truncation = true)
    diags ?picture PIC.TYPES.{ digits; scale = scaling; signed } =
  let byte_size =
    if not min_size_is_short && digits < 3 then Byte_size       (* 1-2 = 1 byte *)
    else if digits < 5 then Short_size                       (* 3-4 = 2 bytes *)
    else if digits < 10 then Long_size                       (* 5-9 = 4 bytes *)
    else if digits < 19 &&     (* 10-19 if unsigned, 10-18 if signed = 8 bytes *)
            signed then Double_size
    else if digits < 20 then Double_size
    else Long_double_size                                         (* 16 bytes *)
  and truncation =
    if native_truncation
    then Truncate_to_native_size
    else Truncate_to_digits { digits }
  in
  diags, Ok (Binary { picture; signed; scaling; byte_size; truncation })


let native_binary_usage diags byte_size ?(signedness = None) ~usage picture =
  let diags =
    match picture with
    | None | Some Error _ ->
        diags
    | Some Ok picture ->
        data_warning diags @@ Ignored_picture_clause { picture; usage }
  in
  diags,
  Ok (Binary { picture = None; byte_size; scaling = 0;
               signed = signedness <> Some Cobol_ptree.Unsigned;
               truncation = Truncate_to_native_size })


let display_usage_from_picture diags ~data_config (picture: picture) =
  match picture.category with
  | Alphabetic _
  | Alphanumeric _
  | National _ ->                                   (* TODO: National_display? *)
      diags, Ok (Alphanumeric { picture; size = PIC.data_size picture })
  | Boolean _
  | FixedNum _
  | FloatNum _ ->
      let sign =
        if Cobol_data.Picture.is_signed_numeric picture
        then Display_signed data_config.display_sign_config
        else Display_unsigned
      in
      diags, Ok (Display_numeric { picture; sign })


let auto_usage diags ~item_loc ?(usage: Cobol_ptree.usage_clause = Display)
    ~data_config picture =
  match usage with
  | Binary ->
      let diags, picture
        = ensure_picture diags ~only:`Numeric_category ~item_loc ~usage
          picture in
      binary_from_numeric_info diags ~picture ~native_truncation:false
        (Result.value ~default:{ digits = 1; scale = 0; signed = false } @@
         PIC.numeric_info picture)
  | Bit ->
      let diags, picture
        = ensure_picture diags ~only:`Boolean_class ~item_loc ~usage
          picture in
      diags, Ok (Bit picture)
  | Display ->
      let diags, picture
        = ensure_picture diags ~only:`Any_class ~item_loc ~usage
          picture in
      display_usage_from_picture diags ~data_config picture
  | National ->
      let diags, picture
        = ensure_picture diags ~only:`Nonalpha_class ~item_loc ~usage
          picture in
      diags, Ok (National picture)
  | PackedDecimal ->
      let diags, picture
        = ensure_picture diags ~only:`Numeric_category ~item_loc ~usage
          picture in
      diags, Ok (Packed_decimal { picture; with_sign_nibble = true })
  | _ ->
      diags, Error None


let display_usage ~item_loc ~data_config ?value_literal ?picture diags =
  match picture, value_literal with
  | None, None ->
      diags, Error None
  | None, Some value ->
      diags, Ok (display_usage_from_literal ~&value)
  | Some _, _ ->
      auto_usage diags ~item_loc picture ~data_config


(** Items with USAGE COMP-5 *)
let range_extended_usage diags ~item_loc given_picture usage =
  let diags, picture =
    ensure_picture diags ~only:`Numeric_or_alphanum_category
      ~item_loc ~required:true ~usage given_picture
  in
  if PIC.is_numeric picture then
    match PIC.numeric_info picture with                 (* TODO: check scale? *)
    | Ok ({ digits; _ } as numeric_info)
      when digits >= 1 && digits <= 18 ->
        binary_from_numeric_info diags ~picture ~min_size_is_short:true
          ~native_truncation:true numeric_info
    | Ok { digits; scale = scaling; signed } ->
        let picture = Result.get_ok @@ Option.get given_picture in
        let feature = Digits { given = digits ; min = 1; max = 18 } in
        data_error diags @@ Invalid_picture_feature { picture; usage; feature },
        if digits < 0
        then Ok (Binary { picture = Some ~&picture;
                          signed; scaling; byte_size = Short_size;
                          truncation = Truncate_to_native_size })
        else Ok (Binary { picture = Some ~&picture;
                          signed; scaling; byte_size = Double_size;
                          truncation = Truncate_to_native_size })
    | Error _ ->
        diags, Error None               (* already reported in `ensure_picture` *)
  else if PIC.is_alphanum picture then
    let byte_size =                (* MF extension (we should emit a warning) *)
      match PIC.data_size picture with
      | 1 -> Byte_size
      | 2 -> Short_size
      | 4 -> Long_size
      | 8 -> Double_size
      | 16 -> Long_double_size
      | n -> Custom_size n
    in
    diags, Ok (Binary { picture = None; signed = false; scaling = 0; byte_size;
                        truncation = Truncate_to_native_size })
  else
    diags, Error None                 (* already reported in `ensure_picture` *)


let packed_decimal_usage diags ~item_loc ~picture comp =
  let diags, picture
    = ensure_picture diags ~item_loc ~only:`Numeric_category ~required:true
      ~usage:PackedDecimal picture
  in                                              (* TODO: check digits <= 18 *)
  match comp with
  | `Comp3 ->                                 (* == Packed_decimal in GnuCOBOL *)
      diags, Ok (Packed_decimal { picture; with_sign_nibble = true })
  | `Comp6 ->                         (* == Packed_decimal without sign nibble *)
      diags, Ok (Packed_decimal { picture; with_sign_nibble = false })


let literal_value diags lit =
  match Cobol_data.Literal.value lit with
  | Ok value ->
      diags, Some value, Some lit
  | Error data_errors ->
      NEL.fold_left diags data_errors
        ~f:(fun diags e -> data_error diags @@ Data_literal_error e),
      None, Some lit


let to_usage_n_value ~item_name ~item_loc ~data_config item_clauses =
  let diags = [] in
  let diags, value, value_literal = match item_clauses.value with
    | Some { payload = ValueTable _; loc = value_loc } ->
        data_error diags @@ Unexpected_table_value_clause { item_name;
                                                            value_loc },
        None, None
    | Some { payload = ValueData lit; _ } ->
        literal_value diags lit
    | None ->
        diags, None, None
  in
  let data_config =
    match item_clauses.sign with
    | Some sign_config ->
        { data_config with
          display_sign_config = sign_config_of_clause ~&sign_config }
    | None ->
        data_config
  in
  let diags, picture =
    match item_clauses.picture with
    | None ->
        diags, None
    | Some picture_clause ->
        match translate_picture_clause ~data_config picture_clause with
        | Ok pic ->
            diags, Some (Ok pic)
        | Error diags' ->
            LIST.append ~loc:__LOC__ diags' diags, Some (Error picture_clause)
  in
  let endian =     (* TODO: set default via FLOAT-BINARY in OPTIONS paragraph *)
    Option.value ~default:Cobol_ptree.HighOrderLeft
  and encoding =  (* TODO: set default via FLOAT-DECIMAL in OPTIONS paragraph *)
    Option.value ~default:Cobol_ptree.DecimalEncoding
  in
  let usage_clause, usage_clause_loc = match item_clauses.usage with
    | Some usage ->
        ~&usage, Some ~@usage
    | None ->                      (* fallback to DISPLAY *)
        Display, None             (* TODO: NATIONAL in case value is a natlit *)
  in
  let usage = usage_clause in
  let diags, usage = match usage with
    | Binary ->
        auto_usage diags ~item_loc ~data_config ~usage picture

    | BinaryChar signedness ->
        native_binary_usage diags Byte_size ~signedness ~usage picture

    | BinaryDouble signedness ->
        native_binary_usage diags Double_size ~signedness ~usage picture

    | BinaryLong signedness ->
        native_binary_usage diags Long_size ~signedness ~usage picture

    | BinaryShort signedness ->
        native_binary_usage diags Short_size ~signedness ~usage picture

    | Bit ->
        auto_usage diags ~item_loc ~data_config ~usage picture

    | Display ->
        display_usage diags ~item_loc ~data_config ?picture ?value_literal

    | FloatBinary32 e ->
        diags, Ok (Float_binary { width = `W32;
                                  endian = endian e })

    | FloatBinary64 e ->
        diags, Ok (Float_binary { width = `W64;
                                  endian = endian e })

    | FloatBinary128 e ->
        diags, Ok (Float_binary { width = `W128;
                                  endian = endian e })

    | FloatDecimal16 { endianness_mode = e;
                       encoding_mode = c } ->
        diags, Ok (Float_decimal { width = `W16;
                                   endian = endian e;
                                   encoding = encoding c })

    | FloatDecimal34 { endianness_mode = e;
                       encoding_mode = c } ->
        diags, Ok (Float_decimal { width = `W34;
                                   endian = endian e;
                                   encoding = encoding c })

    | FloatExtended ->
        diags, Ok Float_extended

    | FloatLong ->
        diags, Ok Float_long

    | FloatShort ->
        diags, Ok Float_short

    | FunctionPointer f ->
        diags, Ok (Function_pointer f)

    | ProcedurePointer ->
        diags, Ok Procedure_pointer

    | Index ->                                 (* TODO: check value \in Z (N?) *)
        diags, Ok Index

    | National -> (* <- TODO: better handling of NATIONAL (partial in GnuCOBOL) *)
        auto_usage diags ~item_loc ~data_config ~usage picture

    | ObjectReference r ->
        diags, Ok (Object_reference r)

    | PackedDecimal ->
        auto_usage diags ~item_loc ~data_config ~usage picture

    | Pointer p ->
        diags, Ok (Pointer p)

    | ProgramPointer p ->
        diags, Ok (Program_pointer p)

    (* TODO: customizable USAGE mapping *)
    | UsagePending `BinaryCLong signedness ->
        native_binary_usage diags C_long_size ~signedness ~usage picture

    | UsagePending `Comp1 ->
        diags, Ok Float_short

    | UsagePending `Comp2 ->
        diags, Ok Float_long

    | UsagePending (`Comp3 | `Comp6 as comp) ->
        packed_decimal_usage diags ~item_loc ~picture comp

    | UsagePending `Comp5 as usage ->
        range_extended_usage diags ~item_loc picture usage

    | Type _
    | UsagePending (`Comp10|`CompN|`Comp0|`Comp15|`CompX|`Comp9) ->
        (* Note: `usage_clause_loc = None` implies `usage_clause = Display`,
           unreachable here. *)
        let usage = usage &@ Option.get usage_clause_loc in
        let warn = Typeck_data_diagnostics.Unsupported_usage { usage } in
        diags, Error (Some warn)
  in
  let diags = match usage, item_clauses.picture with
    | _, None
    | Error _, Some _
    | Ok (Alphanumeric _ |
          Binary _ |                            (* already reported (warning) *)
          Bit _ |
          Display_numeric _ |
          National _ |
          Packed_decimal _), Some _ ->
        diags
    | Ok (Float_binary _ |
          Float_decimal _ |
          Float_extended |
          Float_long |
          Float_short |
          Function_pointer _ |
          Procedure_pointer |
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
