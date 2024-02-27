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

(* Mappings to/from tokens and keyword strings are very redundant with what's
   (auto-generated!) in `cobol_parser/text_keywords.ml` (and a little bit of
   manual mapping from `combined_tokens` in `cobol_parser/text_tokenizer.ml`.

   TODO: use the aforementioned associations instead of redefining them
   below. *)

open Indent_type

(*data division *)
let data_context_of_str : string -> data_context = function
  | "."  -> PERIOD
  | "COPY" -> Compiler_directive COPY
  | "REPLACE" -> Compiler_directive REPLACE
  | "FD" -> Entry FD
  | "RD" -> Entry RD
  | "SD" -> Entry SD
  | s when (s >= "1" && s <= "9")
        || (s >= "01" && s <= "99") ->
      let level = Int32.to_int @@ Int32.of_string s in
      Entry (LEVEL level)
  | _ -> No_keyword


(*procedure division*)

(*keyword of statement*)
let keyword_stmt =
  [ (*standard 1989*)
    "ACCEPT"; "ADD"; "ALLOCATE"; "CALL"; "CANCEL"; "CLOSE"; "COMPUTE"; "CONTINUE";
    "DELETE"; "DISPLAY"; "DIVIDE"; "EVALUATE"; "EXIT"; "FREE";
    "GENERATE"; "GO"; "GOBACK"; "IF"; "INITIALIZE"; "INITIATE"; "INSPECT"; "INVOKE";
    "MERGE"; "MOVE"; "MULTIPLY"; "OPEN"; "PERFORM";
    "RAISE"; "READ"; "RELEASE"; "RESUME"; "RETURN"; "REWRITE";
    "SEARCH"; "SET"; "SORT"; "STOP"; "START"; "STRING"; "SUBTRACT"; "SUPPRESS";
    "TERMINATE"; "UNLOCK"; "UNSTRING"; "USE"; "VALIDATE"; "WRITE";
    (*standard 1985*)
    "ALTER"; "DISABLE"; "ENABLE"; "PURGE"; "RECEIVE"; "SEND";]
let is_statement =
  let keyword_stmt_tbl = Hashtbl.create 16 in
  List.iter (fun x -> Hashtbl.add keyword_stmt_tbl x ()) keyword_stmt;
  fun word ->
    Hashtbl.mem keyword_stmt_tbl word


let str_proc_keyword_tbl = Hashtbl.create 16
let keyword_str_tbl = Hashtbl.create 16

let () =
  (*add statement with end-xxx*)
  List.iter
    (fun (a, b) ->
      Open_scope b |> Hashtbl.add str_proc_keyword_tbl a;
      Close_scope b |> Hashtbl.add str_proc_keyword_tbl ("END-"^a);
      Hashtbl.add keyword_str_tbl b a)
    [ ("ACCEPT", ACCEPT); ("ADD", ADD); ("CALL", CALL); ("COMPUTE", COMPUTE);
      ("DELETE", DELETE); ("DISPLAY", DISPLAY); ("DIVIDE", DIVIDE);
      ("EVALUATE", EVALUATE); ("IF", IF); ("MULTIPLY", MULTIPLY);
      ("READ", READ); ("RETURN", RETURN); ("REWRITE", REWRITE);
      ("SEARCH", SEARCH); ("START", START); ("STRING", STRING);
      ("SUBTRACT", SUBTRACT); ("UNSTRING", UNSTRING); ("WRITE", WRITE)];

  List.iter
    (fun (a, b) ->
      Open_scope b |> Hashtbl.add str_proc_keyword_tbl a;
      Hashtbl.add keyword_str_tbl b a)
    [ (*without END-XXX *)
      ("ALLOCATE", ALLOCATE); ("CANCEL", CANCEL); ("CLOSE", CLOSE); ("CONTINUE", CONTINUE);
      ("EXIT", EXIT); ("FREE", FREE); ("GENERATE", GENERATE); ("GO", GO);
      ("GOBACK", GOBACK); ("INITIALIZE", INITIALIZE); ("INITIATE", INITIATE);
      ("INSPECT", INSPECT); ("INVOKE", INVOKE); ("MERGE", MERGE); ("MOVE", MOVE);
      ("OPEN", OPEN); ("RAISE", RAISE); ("RELEASE", RELEASE); ("RESUME", RESUME);
      ("SET", SET); ("SORT", SORT); ("STOP", STOP); ("SUPPRESS", SUPPRESS);
      ("TERMINATE", TERMINATE); ("UNLOCK", UNLOCK); ("USE", USE); ("VALIDATE", VALIDATE);
      (*standard 1985*)
      ("ALTER", ALTER); ("DISABLE", DISABLE); ("ENABLE", ENABLE);
      ("PURGE", PURGE); ("RECEIVE", RECEIVE); ("SEND", SEND);];

  (*add other keyword (phrase/clause inside statement)*)
  List.iter
    (fun (a, b) ->
      Hashtbl.add str_proc_keyword_tbl a (Phrase b);
      Hashtbl.add keyword_str_tbl b a)
    [ ("AFTER", AFTER); ("AT", AT); ("BEFORE", BEFORE); ("BY", BY);
      ("CONVERTING", CONVERTING); ("ELSE", ELSE); ("FROM", FROM);
      ("GIVING", GIVING); ("INTO", INTO); ("REPLACING", REPLACING);
      ("SEQUENCE", SEQUENCE); ("TALLYING", TALLYING); ("THEN", THEN);
      ("TO", TO); ("UNTIL", UNTIL); ("USING", USING); ("VARYING", VARYING);
      ("WHEN", WHEN); ("RAISING", RAISING) ];

  (*add inline_phrase*)
  List.iter
    (fun a -> Hashtbl.add str_proc_keyword_tbl a Inline_phrase)
    [ "INITIALIZED"; "RETURNING"; "REMAINDER";
      "INPUT"; "OUTPUT"; "WITH"; "ADVANCING";
      "IGNORING"; "DEFAULT"; "SHARING"; "LOCK"; "POINTER"];

  (*For now, Other_keyword is only for avoiding bugs in check_argument *)
  List.iter
    (fun a -> Hashtbl.add str_proc_keyword_tbl a Other_keyword)
    [ "ON"; "SIZE"; "ERROR"; "NOT"; "EXCEPTION"; "OVERFLOW";
      "END"; "EXCEPTION"; "INVALID"; "KEY"; "PAGE"; "EOP"];
  (*special case*)
  List.iter (fun (a, b) -> Hashtbl.add str_proc_keyword_tbl a b)
    [ ( ".", PERIOD);
      ("COPY", Compiler_directive COPY);
      ("REPLACE", Compiler_directive REPLACE);
      (*special case*)
      ("PERFORM", Open_scope PERFORM_CLOSED);
      ("END-PERFORM", Close_scope PERFORM)]

(*add other keywords into keyword_str_tbl*)
let () =
  List.iter
    (fun (a, b) -> Hashtbl.add keyword_str_tbl a b)
    [ (COPY, "COPY"); (REPLACING_COPY, "REPLACING_COPY"); (REPLACE, "REPLACE");
      (IDENT_DIV, "IDENT_DIV"); (ENV_DIV, "ENV_DIV"); (SELECT, "SELECT");
      (DATA_DIV, "DATA_DIV"); (DATA_DESC, "DATA_DESC");
      (FD, "FD"); (SD, "SD"); (RD, "RD"); (DATA_DIV_CLAUSE, "DATA_DIV_CLAUSE");
      (PROC_DIV, "PROC_DIV"); (SECTION, "SECTION"); (PARAGRAPH, "PARAGRAPH");
      (DECLARATIVES, "DECLARATIVES"); (COMPILATION_UNIT, "COMPILATION_UNIT");

      (ON_SIZE_ERROR, "ON_SIZE_ERROR"); (NOT_ON_SIZE_ERROR, "NOT_ON_SIZE_ERROR");
      (ON_EXCEPTION, "ON_EXCEPTION"); (NOT_ON_EXCEPTION, "NOT_ON_EXCEPTION");
      (INVALID_KEY, "INVALID_KEY"); (NOT_INVALID_KEY, "NOT_INVALID_KEY");
      (AT_END_OF_PAGE, "AT_END_OF_PAGE"); (NOT_AT_END_OF_PAGE, "NOT_AT_END_OF_PAGE");
      (ON_OVERFLOW, "ON_OVERFLOW"); (NOT_ON_OVERFLOW, "NOT_ON_OVERFLOW");
      (AT_END, "AT_END"); (NOT_AT_END, "NOT_AT_END");
      (PERFORM, "PERFORM"); (PERFORM_CLOSED, "PERFORM");
      (DUMMY_EXCEPTION, "DUMMY_EXCEPTION")]


let proc_context_of_str str =
  match Hashtbl.find_opt str_proc_keyword_tbl str with
  | Some x -> x
  | _ -> No_keyword

let string_of_keyword keyword =
  match Hashtbl.find_opt keyword_str_tbl keyword with
  | Some x -> x
  | _ -> "DEFAULT"


(*not implicitly terminable*)
let keyword_not_imp_terminable = Hashtbl.create 16
let () =
  List.iter (fun a -> Hashtbl.add keyword_not_imp_terminable a ())
  [
    IF; WHEN; PERFORM; THEN; ELSE;
    ON_SIZE_ERROR; NOT_ON_SIZE_ERROR; HELPTOKEN;
    ON_EXCEPTION; NOT_ON_EXCEPTION;
    ON_OVERFLOW; NOT_ON_OVERFLOW;
    AT_END; NOT_AT_END; SEARCH_AT_END;
    AT_END_OF_PAGE; NOT_AT_END_OF_PAGE;
    INVALID_KEY; NOT_INVALID_KEY;

    IDENT_DIV; ENV_DIV; DATA_DIV; PROC_DIV;
    DECLARATIVES; PARAGRAPH; SECTION;
    BEGIN; COMPILATION_UNIT;
    ]
let is_not_imp_terminable = Hashtbl.mem keyword_not_imp_terminable


let keyword_phrase = Hashtbl.create 16
let () =
  List.iter (fun a -> Hashtbl.add keyword_phrase a ())
  [
    USING; REPLACING; TALLYING; SEQUENCE; TO;
    GIVING; AT; INTO; VARYING; AFTER; UNTIL;
    FROM; BY; ARGUMENT
    ]
let is_phrase = Hashtbl.mem keyword_phrase
