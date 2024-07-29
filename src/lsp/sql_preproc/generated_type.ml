type ref_value =
  | Reference of
      { prefix : string;
        var : string
      }
  | Value of
      { prefix : string;
        var : string
      }

type error_treatment =
  | Error_treatment of
      { prefix : string;
        condition : whenever_condition;
        continuation : whenever_continuation
      }

and whenever_condition =
  | Not_found_whenever
  | SqlError_whenever
  | SqlWarning_whenever

and whenever_continuation =
  | Continue
  | Perform of string
  | Goto of string

type trans_stm =
  | CallStatic of
      { prefix : string;
        fun_name : string;
        ref_value : ref_value list (*Can be empty*)
      }
  | Copy of
      { prefix : string;
        file_name : string
      }
  | Todo of { prefix : string }

type generated_stm =
  | NoChange of { content : string }
  | Added of { content : string }
  | Change of
      { old_stms : string list;
        trans_stm : trans_stm list;
        error_treatment : error_treatment list;
        with_dot : bool
      }

type generated = generated_stm list

module Printer = struct
  let rec pp fmt gen =
    match gen with
    | h :: t -> Format.fprintf fmt "%a%a" pp_gene h pp t
    | [] -> ()

  and pp_gene fmt x =
    match x with
    | NoChange { content } -> Format.fprintf fmt "%s\n" content
    | Added { content } -> Format.fprintf fmt "%s\n" content
    | Change { old_stms; trans_stm; error_treatment; with_dot } ->
      let dot =
        if with_dot then
          "."
        else
          ""
      in
      Format.fprintf fmt "%a\n%a%a%s" pp_old_stms old_stms pp_trans_stm
        trans_stm pp_error_treatment error_treatment dot

  and pp_old_stms fmt x =
    match x with
    | h :: t -> Format.fprintf fmt "      *> REMOVED: %s\n%a" h pp_old_stms t
    | [] -> ()

  and pp_trans_stm fmt x =
    match x with
    | h :: t -> Format.fprintf fmt "%a\n%a" pp_trans_stm_aux h pp_trans_stm t
    | [] -> ()

  and pp_error_treatment fmt x =
    match x with
    | h :: t ->
      Format.fprintf fmt "%a\n%a" pp_error_treatment_aux h pp_error_treatment t
    | [] -> ()

  and pp_trans_stm_aux fmt x =
    match x with
    | CallStatic { prefix; fun_name; ref_value } ->
      Format.fprintf fmt "%sCALL STATIC \"%s\"%a%sEND-CALL" prefix fun_name
        pp_ref_value_list ref_value prefix
    | Copy { prefix; file_name } ->
      Format.fprintf fmt "%sCOPY %s" prefix file_name
    | Todo { prefix } -> Format.fprintf fmt "%sTODO" prefix

  and pp_error_treatment_aux fmt = function
    | Error_treatment { prefix; condition; continuation } -> begin
      let print_continuation fmt continuation =
        match continuation with
        | Continue -> Format.fprintf fmt "CONTINUE"
        | Perform sqlVarToken -> Format.fprintf fmt "PERFORM %s" sqlVarToken
        | Goto sqlVarToken -> Format.fprintf fmt "GOTO %s" sqlVarToken
      in
      match condition with
      | Not_found_whenever ->
        Format.fprintf fmt "%sWHEN SQLCODE = 100\n%s%a" prefix prefix
          print_continuation continuation
      | SqlError_whenever ->
        Format.fprintf fmt "%sWHEN SQLCODE < 0\n%s%a" prefix prefix
          print_continuation continuation
      | SqlWarning_whenever ->
        Format.fprintf fmt "%sWHEN SQLCODE < 0\n%s%a" prefix prefix
          print_continuation continuation
    end

  and pp_ref_value_list fmt x =
    let rec pp_ref_value_list_aux fmt x =
      match x with
      | h :: t -> Format.fprintf fmt "%a\n%a" pp_ref_value h pp_ref_value_list_aux t
      | [] -> ()
    in
    match x with
    | [] -> ()
    | _ -> Format.fprintf fmt " USING\n%a" pp_ref_value_list_aux x

  and pp_ref_value fmt x =
    match x with
    | Reference { prefix; var } ->
      Format.fprintf fmt "%sBY REFERENCE %s" prefix var
    | Value { prefix; var } -> Format.fprintf fmt "%sBY VALUE %s" prefix var
end
