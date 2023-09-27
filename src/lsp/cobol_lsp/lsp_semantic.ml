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

open Cobol_common                                          (* Srcloc, Visitor *)
open Cobol_common.Srcloc.INFIX
open Cobol_parser.Grammar_tokens

type semantic_token = {
  line: int;
  start: int;
  length: int;
  token_type: string;
  token_modifiers: string list;
}

let tokens_types = [
  "type";
  "operator";
  "variable";
  "function";
  "parameter";
  "keyword";
  "string";
  "number";
  "namespace";
  "decorator";
  "modifier";
  "comment";
  (* "class"; *)
  (* "enum"; *)
  (* "interface"; *)
  (* "struct"; *)
  (* "typeParameter"; *)
  (* "property"; *)
  (* "enumMember"; *)
  (* "event"; *)
  (* "method"; *)
  (* "macro"; *)
  (* "regexp"; *)
]

let tokens_modifiers = [
  "declaration";
  "definition";
  "readonly";
  "modification";
  "defaultLibrary";
(*"static";
  "deprecated";
  "abstract";
  "async";
  "documentation";*)
]

let semantic_token lexloc token_type token_modifiers =
  let range = Lsp_position.range_of_lexloc lexloc in
  let line = range.start.line in
  let start = range.start.character in
  let length = range.end_.character - start in
  { line; start; length; token_type; token_modifiers }

type token_type =
  | ProgramName
  | ParagraphName
  | ProcName
  | Parameter
  | DataDecl
  | DataLevel
  | Var
  | VarModif
  | ReportName
  | ExceptionName
  | MnemonicName
  | FileName

let semantic_visitor ~filename =
  let open Cobol_parser.PTree_visitor in
  let open Cobol_ast.Terms_visitor in
  let open Cobol_ast.Operands_visitor in
  let open Cobol_common.Visitor in

  let semantic_token_of lexloc token_type =
    let semantic_token_of lexloc (token_type, token_modifiers) =
      semantic_token lexloc token_type token_modifiers
    in
    semantic_token_of lexloc @@ match token_type with
      | ProgramName -> "string", ["definition"; "readonly"]
      | ParagraphName -> "function", ["definition"]
      | ProcName -> "function", []
      | Parameter -> "parameter", []
      | DataDecl -> "variable", ["declaration"]
      | DataLevel -> "decorator", []
      | Var -> "variable", []
      | VarModif -> "variable", ["modification"]
      | ReportName
      | ExceptionName
      | MnemonicName
      | FileName -> "variable", ["readonly"]
  in
  let add_name' name token_type acc =
    match Srcloc.lexloc_in ~filename ~@name with
    | lexloc -> List.cons (semantic_token_of lexloc token_type) acc
    | exception Invalid_argument _ -> acc
  in
  let rec add_qualname (qn:Cobol_ast.qualname) token_type acc =
    match qn with
    | Name name ->
        add_name' name token_type acc
    | Qual (name, qn) ->
        add_name' name token_type acc |> add_qualname qn token_type
  in
  let add_ident (id:Cobol_ast.ident) token_type acc =
    match id with
    | QualIdent {ident_name; _} -> add_qualname ident_name token_type acc
    | _ -> acc (* TODO *)
  in
  let add_ident' id = add_ident ~&id in
  let add_list add_fun l token_type acc =
    List.fold_left (fun acc n -> add_fun n token_type acc) acc l
  in
  let add_option add_fun v token_type acc =
    match v with
    | None -> acc
    | Some v -> add_fun v token_type acc
  in

  Cobol_parser.PTree_visitor.fold_compilation_group (object (self)
    inherit [semantic_token List.t] Cobol_parser.PTree_visitor.folder

    (* program-name *)
    method! fold_program_unit {program_name; _} acc = acc
      |> add_name' program_name ProgramName
      |> Visitor.do_children
      (* we call do_children, so we must ensure that
         the fold_name' does nothing,
         otherwise, there will be token overlap.

         Or we can override this method fold_program_unit to explicitly
         fold its every child and return Visitor.skip_children x.
         But by doing that for every method(which we need to override),
         we have to write a great amount of code... like rewriting
         the code of Cobol_ast.

      *)

    (*TODO: File/Report section*)

    (* data-name *)
    method! fold_data_name data_name acc =
      match data_name with
      | DataName n -> acc
        |> add_name' n DataDecl
        |> Visitor.skip_children
      | _ ->
        Visitor.do_children acc

    method! fold_rename_item {rename_level; rename_to;
                              rename_renamed; rename_through } acc = acc
      |> add_name' rename_to DataDecl
      (*|> Visitor.do_children*)
      (* We can remove the code below and return do_children directly*)
      |> fold_data_level' self rename_level
      |> fold_qualname self rename_renamed
      |> fold_qualname_opt self rename_through
      |> Visitor.skip_children

    method! fold_condition_name_item { condition_name_level;
                                       condition_name;
                                       condition_name_values;
                                       condition_name_alphabet;
                                       condition_name_when_false } acc = acc
      |> add_name' condition_name DataDecl
      (*|> Visitor.do_children *)
      |> fold_data_level' self condition_name_level
      |> fold_list ~fold:fold_condition_name_value self condition_name_values
      |> fold_name'_opt self condition_name_alphabet
      |> fold_literal_opt self condition_name_when_false
      |> Visitor.skip_children

    method! fold_data_clause dc acc =
      match dc with
      | DataRedefines name -> acc
          |> add_name' name Var
          |> Visitor.skip_children
      | _ ->
          Visitor.skip_children acc (*Not implmented*)

    (* data-level *)
    (* TODO: condition_name ??*)
    method! fold_data_level' dl acc = acc
      |> add_name' dl DataLevel
      |> Visitor.skip_children

    (* paragraph name *)
    method! fold_paragraph { paragraph_name; paragraph_is_section;
                             paragraph_segment; paragraph_sentences } acc =
      ignore paragraph_is_section; acc
      |> add_option add_name' paragraph_name ParagraphName
      (*|> Visitor.do_children*)
      |> fold_integer_opt self paragraph_segment
      |> fold_list ~fold:(fun v -> v#continue_with_statements') self paragraph_sentences
      |> Visitor.skip_children

    (* procedure using *)
    method! fold_using_by_reference { using_by_reference;
                                      using_by_reference_optional } acc = acc
      |> add_name' using_by_reference Parameter
      (*|> Visitor.do_children*)
      |> fold_bool self using_by_reference_optional
      |> Visitor.skip_children

    method! fold_using_clause = function
      | UsingByReference _ ->
          Visitor.do_children
      | UsingByValue l -> fun x -> x
        |> add_list add_name' l Parameter
        |> Visitor.skip_children

    (* inline call of function *)
    method! fold_inline_call { call_fun; call_args } acc = acc
      |> add_name' call_fun ProcName
      |> fold_list ~fold:fold_effective_arg self call_args
      |> Visitor.skip_children

    (* Statement *)
    (* distinguish
      1 variable
      2 variable modified
      3 procedure-name
      4 report-name/file-name/exception-name/mnemonic-name *)
    (*TODO: maybe finer analysis*)

    method! fold_accept' {payload = accept_stmt; _} acc =
      match accept_stmt with
      | AcceptFromDevice { item; device_item } -> acc
          |> add_name' device_item MnemonicName
          (*|> Visitor.do_children*)
          |> fold_ident' self item
          |> Visitor.skip_children
      | AcceptGeneric _
      | AcceptTemporal _
      | AcceptMsgCount _
      | AcceptAtScreen _
      | AcceptFromEnv _ ->
          Visitor.do_children acc

    method! fold_allocate' {payload = { allocate_kind;
                                        allocate_initialized;
                                        allocate_returning }; _ } acc = acc
      |> fold_allocate_kind self allocate_kind
      |> fold_bool self allocate_initialized
      |> add_option add_ident' allocate_returning VarModif
      |> Visitor.skip_children

    (*TODO: Alter *)

    method! fold_call' {payload = { call_prefix; call_using;
                                    call_returning;
                                    call_error_handler }; _} acc = acc
      |> fold_call_prefix self call_prefix
      |> fold_list ~fold:fold_call_using_clause' self call_using
      |> add_option add_ident' call_returning VarModif
      |> fold_option ~fold:fold_call_error_handler self call_error_handler
      |> Visitor.skip_children

    (*TODO: Cancel *)

    method! fold_close_phrase { close_item; close_format} acc = acc
      |> add_name' close_item FileName
      (*|> Visitor.do_children*)
      |> fold_option ~fold:fold_close_format self close_format
      |> Visitor.skip_children

    (* Add Compute Divide Multiply Subtract *)
    method! fold_rounded_ident { rounded_ident; rounded_rounding } acc = acc
      |> add_ident rounded_ident VarModif
      |> fold_rounding self rounded_rounding
      |> Visitor.skip_children

    method! fold_delete' {payload = { delete_targets; delete_retry;
                                      delete_on_invalid_key }; _} acc = acc
      |> add_name' delete_targets FileName
      (*|> Visitor.do_children*)
      |> fold_option ~fold:fold_retry_clause self delete_retry
      |> fold_dual_handler self delete_on_invalid_key
      |> Visitor.skip_children

    method! fold_display' { payload; _ } acc =
      Visitor.do_children_and_then acc @@ match payload with
      | DisplayDevice { upon = Some { payload = DisplayUponName n; _ }; _ } ->
          add_name' n MnemonicName
      | _ ->
          Fun.id

    (*TODO: Exit *)

    method! fold_free' names acc = acc
      |> add_list add_name' ~&names VarModif
      |> Visitor.skip_children

    method! fold_generate' name acc = acc
      |> add_name' ~&name VarModif
      |> Visitor.skip_children

    method! fold_goto' {payload = goto_target; _} acc = acc
      |> add_qualname goto_target ProcName
      |> Visitor.skip_children

    method! fold_goto_depending' {payload = { goto_depending_targets;
                                              goto_depending_on }; _} acc = acc
      |> add_list add_qualname goto_depending_targets ProcName
      |> fold_ident self goto_depending_on
      |> Visitor.skip_children

    method! fold_initialize' {payload = { init_items; init_filler; init_category;
                                          init_replacings; init_to_default }; _} acc = acc
      |> add_list add_ident init_items VarModif
      |> fold_bool self init_filler
      |> fold_option ~fold:fold_init_category self init_category
      |> fold_list ~fold:fold_init_replacing self init_replacings
      |> fold_bool self init_to_default
      |> Visitor.skip_children

    method! fold_initiate' { payload = names; _} acc = acc
      |> add_list add_name' names ReportName
      |> Visitor.skip_children

    method! fold_tallying { tallying_target; tallying_clauses } acc = acc
        |> add_qualname tallying_target.ident_name VarModif
        |> fold_list ~fold:fold_tallying_clause' self tallying_clauses
        |> Visitor.skip_children

    method! fold_inspect' { payload = { inspect_item; inspect_spec }; _} acc = acc
      |> add_ident inspect_item VarModif
      |> fold_inspect_spec self inspect_spec
      |> Visitor.skip_children

    (*TODO: Invoke *)

    method! fold_move' {payload = move_stmt; _} acc =
      match move_stmt with
      | MoveSimple { from; to_ } -> acc
          |> fold_ident_or_literal self from
          |> add_list add_ident to_ VarModif
          |> Visitor.skip_children
      | MoveCorresponding { from; to_ } -> acc
          |> fold_ident self from
          |> add_list add_ident to_ VarModif
          |> Visitor.skip_children

    method! fold_named_file_option {named_file_name;
                                    named_file_option} acc = acc
      |> add_name' named_file_name FileName
      (*|> Visitor.do_children*)
      |> fold_option ~fold:fold_file_option self named_file_option
      |> Visitor.skip_children

    method! fold_perform_target perform_target acc =
      match perform_target with
      | PerformOutOfLine { procedure_start; procedure_end } -> acc
          |> add_qualname procedure_start ProcName
          |> add_option add_qualname procedure_end ProcName
          |> Visitor.skip_children
      | PerformInline _ ->
          Visitor.do_children acc

    method! fold_varying_phrase { varying_ident; varying_from;
                                  varying_by; varying_until } acc = acc
      |> add_ident varying_ident VarModif
      |> fold_ident_or_numlit self varying_from
      |> fold_option ~fold:fold_ident_or_numlit self varying_by
      |> fold_condition self varying_until
      |> Visitor.skip_children

    method! fold_raise' {payload = raise_stmt; _} acc =
      match raise_stmt with
      | RaiseIdent _ -> Visitor.do_children acc
      | RaiseException name -> acc
          |> add_name' name ExceptionName
          |> Visitor.skip_children

    method! fold_read' {payload = { read_file; read_direction;
                                    read_into; read_lock_behavior;
                                    read_lock; read_key;
                                    read_error_handler }; _} acc = acc
      |> add_name' read_file FileName
      |> fold_option ~fold:fold_read_direction self read_direction
      |> add_option add_ident read_into VarModif
      |> fold_option ~fold:fold_read_lock_behavior self read_lock_behavior
      |> fold_option ~fold:fold_bool self read_lock
      |> fold_option ~fold:fold_qualname self read_key
      |> fold_option ~fold:fold_read_error_handler self read_error_handler
      |> Visitor.skip_children

    (* TODO: RELEASE *)

    method! fold_resume' {payload = qn; _} acc = acc
      |> add_qualname qn ProcName
      |> Visitor.skip_children

    method! fold_return' {payload = {return_file; return_into;
                                     return_at_end}; _} acc = acc
      |> add_name' return_file FileName
      |> add_option add_ident' return_into VarModif
      |> fold_dual_handler self return_at_end
      |> Visitor.skip_children

    method! fold_write_target write_target acc =
      match write_target with
      | WriteTargetName _ -> Visitor.do_children acc
      | WriteTargetFile name -> acc
          |> add_name' name FileName
          |> Visitor.skip_children

    method! fold_search_spec search_spec acc =
      match search_spec with
      | SearchSerial { varying; when_clauses } -> acc
          |> add_option add_ident varying VarModif
          |> fold_list ~fold:fold_search_when_clause' self when_clauses
          |> Visitor.skip_children
      | SearchAll _ -> Visitor.do_children acc

    method! fold_set_switch_spec {set_switch_targets;
                                  set_switch_value } acc = acc
      |> add_list add_ident set_switch_targets MnemonicName (*TODO: ident??*)
      |> fold_on_off self set_switch_value
      |> Visitor.skip_children

    method! fold_set_condition_spec { set_condition_targets;
                                      set_condition_value } acc = acc
      |> add_list add_ident set_condition_targets VarModif
      |> fold_bool self set_condition_value
      |> Visitor.skip_children

    method! fold_set' {payload = set_stmt;_ } acc =
      match set_stmt with
      | SetAmbiguous { targets; set_method; value} -> acc
          |> add_list add_ident targets VarModif
          |> fold_set_ambiguous_method self set_method
          |> fold_expression self value
          |> Visitor.skip_children
      | SetSwitch _ (*TODO*)
      | SetCondition _
      | SetAttribute _
      | SetLocale _
      | SetSavedException ->
          Visitor.do_children acc
      | SetSaveLocale { target; locale } -> acc
          |> add_ident target VarModif
          |> fold_set_save_locale self locale
          |> Visitor.skip_children
      | SetFloatContent { targets; content; sign } -> acc
          |> add_list add_ident targets VarModif
          |> fold_float_content self content
          |> fold_option ~fold:fold_sign self sign
          |> Visitor.skip_children

    method! fold_start' {payload = {start_file; start_position;
                                    start_on_invalid_key }; _} acc = acc
      |> add_name' start_file FileName
      (*|> Visitor.do_children*)
      |> fold_option ~fold:fold_start_position self start_position
      |> fold_dual_handler self start_on_invalid_key
      |> Visitor.skip_children

    method! fold_string_stmt' {payload = {string_sources;
                                          string_target;
                                          string_pointer;
                                          string_on_overflow}; _} acc = acc
      |> add_ident string_target VarModif
      |> fold_list ~fold:fold_string_source self string_sources
      |> fold_option ~fold:fold_ident self string_pointer
      |> fold_dual_handler self string_on_overflow
      |> Visitor.skip_children

    method! fold_terminate' {payload = names; _} acc = acc
      |> add_list add_name' names ReportName
      |> Visitor.skip_children

    method! fold_unlock' {payload = { unlock_file; unlock_record }; _} acc = acc
      |> add_name' unlock_file FileName
      (*|> Visitor.do_children*)
      |> fold_bool self unlock_record
      |> Visitor.skip_children

    method! fold_unstring_target { unstring_target;
                                   unstring_target_delimiter;
                                   unstring_target_count} acc = acc
      |> add_ident unstring_target VarModif
      |> add_option add_ident unstring_target_delimiter VarModif
      |> add_option add_ident unstring_target_count VarModif
      |> Visitor.skip_children

    (*TODO: Validate *)
    (*TODO: Merge, Sort*)

    (* All qualname not colored yet will be marked as normal variable *)
    method! fold_qualname qn acc = acc
      |> add_qualname qn Var
      |> Visitor.skip_children

    end)

(** [make_non_ambigious tokens] returns tokens that do not need to have more analyzing to get their
    type. *)
let make_non_ambigious ~filename tokens = tokens |>
  List.filter_map
    (fun { payload; loc } ->
       try Some (payload, Srcloc.lexloc_in ~filename loc) with _ -> None) |>
  List.filter_map
    (fun (token, lexloc) ->
       match token with
       | WORD _ | WORD_IN_AREA_A _ -> None
       | ALPHANUM _ | ALPHANUM_PREFIX _ ->
          Some (semantic_token lexloc "string" [])
       | BOOLIT _
       | HEXLIT _ | NULLIT _
       | NATLIT _ | SINTLIT _
       | FIXEDLIT _ | FLOATLIT _
       | DIGITS _
       | EIGHTY_EIGHT ->
          Some (semantic_token lexloc "number" [])
       | PICTURE_STRING _ ->
          Some (semantic_token lexloc "type" ["declaration"])
       (* | EQUAL | PLUS | MINUS  *)
       | AMPERSAND | ASTERISK | COLON | DASH_SIGN | DOUBLE_ASTERISK | DOUBLE_COLON
       | EQ | GE | GT | LE | LPAR | LT | NE | PLUS_SIGN | RPAR | SLASH ->
          Some (semantic_token lexloc "operator" [])
       | PARAGRAPH | STATEMENT | PROGRAM |SECTION | DIVISION ->
          Some (semantic_token lexloc "namespace" [])
       | ACCEPT | ACCESS | ADD | ALLOCATE | ALTER | APPLY | ARE | ASSIGN | CALL | CANCEL | CHAIN | CLOSE
       | COMMIT | COMPUTE | CONTINUE | CONTROL | CONTROLS | COPY | COPY_SELECTION | COUNT | CYCLE
       | DELETE | DESTROY | DISABLE | DISP | DISPLAY | DISPLAY_1 | DISPLAY_COLUMNS | DISPLAY_FORMAT
       | DIVIDE | ENABLE | ENSURE_VISIBLE | ENTER | ERASE | ESCAPE | EVALUATE | EXAMINE | EXHIBIT | EXIT
       | FREE | GENERATE | GET | GO | GOBACK | GO_BACK | GO_FORWARD | GO_HOME | GO_SEARCH | IF | IGNORE
       | INITIALIZE | INITIATE | INSPECT | INVOKE | LEAVE | LOCK | LOCK_HOLDING | MERGE | MODIFY | MOVE
       | MULTIPLY | NOTIFY | NOTIFY_CHANGE | OPEN | OUTPUT | OVERRIDE | PARSE | PERFORM | PRINT
       | PRINT_NO_PROMPT | PRINT_PREVIEW | PROCEED | PURGE | RAISE | READ | RECEIVE | REFRESH
       | RELEASE | REPLACE | RERUN | RESERVE | RESET | RESUME | RETRY | RETURN | REWRITE | ROLLBACK
       | SEARCH | SELECT | SELECT_ALL | SEND | SET | SORT | SORT_MERGE | SORT_ORDER | STDCALL | START
       | STEP | STOP | STRING | SUBTRACT | SUPPRESS | TEST | TERMINATE | TRANSFORM | UNLOCK | UNSTRING
       | UPDATE | USE | USE_ALT | USE_RETURN | USE_TAB | VALIDATE | VALIDATE_STATUS | WRAP | WRITE
       | END_ACCEPT | END_ADD | END_CALL | END_COMPUTE | END_DELETE | END_DISPLAY | END_DIVIDE | END_EVALUATE
       | END_IF | END_MULTIPLY | END_PERFORM | END_READ | END_RETURN | END_REWRITE | END_SEARCH | END_START
       | END_STRING | END_SUBTRACT | END_UNSTRING | END_WRITE ->
          Some (semantic_token lexloc "function" ["defaultLibrary";])
       | _ ->
          Some (semantic_token lexloc "keyword" []))

let semantic_tbl = Hashtbl.create 16
let () = List.iteri (fun i elt -> Hashtbl.add semantic_tbl elt i) tokens_types
let index i = Hashtbl.find semantic_tbl i

let tokens_modifiers_bit_flag = Hashtbl.create 8
let () =
  List.iteri (fun i v ->
    Hashtbl.add tokens_modifiers_bit_flag
    v (0b1 lsl i))
  tokens_modifiers

let token_modifiers_flag modifiers =
  List.fold_left
    (fun acc modifier ->
       let modifier_flag = Hashtbl.find tokens_modifiers_bit_flag modifier  in
       acc lor modifier_flag)
    0b0
    modifiers

let data_of_semantic_token semantic_token =
  let data = Array.make 5 0 in
  data.(0) <- semantic_token.line;
  data.(1) <- semantic_token.start;
  data.(2) <- semantic_token.length;
  data.(3) <- index semantic_token.token_type;
  data.(4) <- token_modifiers_flag semantic_token.token_modifiers;
  data

let data_of_semantic_tokens semantic_tokens =
  let data = Array.make (5 * List.length semantic_tokens) 0 in
  ignore @@ List.fold_left
    (fun (idx, last_line, last_start) semantic_token ->
       let token_data = data_of_semantic_token semantic_token in
       let delta_line = token_data.(0) - last_line in
       let delta_start = if delta_line = 0 then
           token_data.(1) - last_start
         else
           token_data.(1)
       in
       data.(5 * idx) <- delta_line;
       data.(5 * idx + 1) <- delta_start;
       data.(5 * idx + 2) <- token_data.(2);
       data.(5 * idx + 3) <- token_data.(3);
       data.(5 * idx + 4) <- token_data.(4);
       (idx + 1, semantic_token.line, semantic_token.start))
    (0, 0, 0)
    semantic_tokens;
  data

let sort_semantic_token first second = (* TODO: use Lexing.position, and then a
                                          comparison on `pos_cnum` only? *)
  let cmp = Stdlib.compare first.line second.line in
  if cmp = 0
  then Stdlib.compare first.start second.start
  else cmp

let data ~filename tokens ptree : int array =
  tokens
  |> make_non_ambigious ~filename
  |> semantic_visitor ~filename ptree
  |> List.fast_sort sort_semantic_token
  |> data_of_semantic_tokens
