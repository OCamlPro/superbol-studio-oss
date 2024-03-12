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
open Terms
open Operands
open Statements

type procedure_division =
  {
    procedure_args: procedure_args option;
    procedure_returning: ident with_loc option;
    procedure_raising_phrases: raising_phrase with_loc list;
    procedure_declaratives: declarative with_loc list;
    procedure_paragraphs: paragraph with_loc list;
  }

and procedure_by_clause =
  | ByReference of by_reference list
  | ByValue of name with_loc list

and procedure_calling_style =
  | ProcedureArgsUsed
  | ProcedureArgsChained

and procedure_args =
  {
    procedure_calling_style: procedure_calling_style with_loc;
    procedure_by_clause: procedure_by_clause with_loc list
  }

and by_reference =
  {
    by_reference: name with_loc;
    by_reference_optional: bool;
  }

and raising_phrase =
  {
    raising: name with_loc;
    raising_factory: bool;
  }

and declarative =
  {
    declarative_name: name with_loc;
    declarative_segment: integer option;
    declarative_use: declarative_use option;
    declarative_sentences: statements with_loc list;
  }

and declarative_use =
  | UseAfterFileException of
      {
        global: bool;
        trigger: use_file_exception_on;
      }
  | UseBeforeReporting of
      {
        global: bool;
        report_group: ident;
      }
  | UseForDebugging of use_for_debugging_target list
  | UseAfterIOException of use_after_exception list
  | UseAfterExceptionObject of name with_loc

and use_for_debugging_target =
  | UseForDebuggingProcedure of
      {
        all: bool;
        procedure: procedure_name with_loc;
      }
  | UseForDebuggingAllProcedures

and use_file_exception_on =
  | UseFileExceptionOnNames of name with_loc list
  | UseFileExceptionOnOpenMode of open_mode

and use_after_exception =
  {
    use_after_exception: name with_loc;
    use_after_exception_on_files: name with_loc list;
  }

and paragraph =
  {
    paragraph_name: name with_loc option;
    paragraph_is_section: bool;
    paragraph_segment: integer option;
    paragraph_sentences: statements with_loc list;
  }
[@@deriving ord]

let pp_paragraph ppf { paragraph_name = pn;
                       paragraph_is_section = pis;
                       paragraph_segment = ps;
                       paragraph_sentences = pss } =
  (match pn with
   | Some n ->
       if pis then Fmt.pf ppf "@[<v 2>" else Fmt.pf ppf "@[<v>";
       pp_with_loc pp_name ppf n;
       if pis then Fmt.pf ppf " SECTION";
       Fmt.(option (sp ++ pp_integer)) ppf ps;
       Fmt.pf ppf ".@ "
   | None -> Fmt.pf ppf "@[<v>");
  Fmt.(list ~sep:(any ".@,@ ") (pp_with_loc pp_statements)) ppf pss;
  if pss != [] then Fmt.pf ppf ".";
  Fmt.pf ppf "@]"

let pp_use_after_exception ppf { use_after_exception = n;
                                 use_after_exception_on_files = af } =
  Fmt.pf ppf "%a%a"
    pp_name' n
    Fmt.(list ~sep:nop (any " FILE " ++ pp_name')) af

let pp_use_file_exception_on ppf = function
  | UseFileExceptionOnNames ns -> Fmt.(list ~sep:sp pp_name') ppf ns
  | UseFileExceptionOnOpenMode om -> pp_open_mode ppf om

let pp_use_for_debugging_target ppf = function
  | UseForDebuggingProcedure { all; procedure } ->
      if all then Fmt.pf ppf "ALL ";
      pp_procedure_name' ppf procedure
  | UseForDebuggingAllProcedures ->
      Fmt.pf ppf "ALL PROCEDURES"

let pp_declarative_use ppf = function
  | UseAfterFileException { global; trigger} ->
      Fmt.pf ppf "USE ";
      if global then Fmt.pf ppf "GLOBAL ";
      Fmt.pf ppf "AFTER EXCEPTION ON %a" pp_use_file_exception_on trigger;
  | UseBeforeReporting { global; report_group } ->
      Fmt.pf ppf "USE ";
      if global then Fmt.pf ppf "GLOBAL ";
      Fmt.pf ppf "BEFORE REPORTING %a" pp_ident report_group
  | UseForDebugging ufdts ->
      Fmt.pf ppf "USE FOR DEBUGGING ON@;<1 2>@[%a@]"
        Fmt.(list ~sep:sp pp_use_for_debugging_target) ufdts
  | UseAfterIOException uaes ->
      Fmt.pf ppf "USE AFTER EXCEPTION CONDITION@;<1 2>@[%a@]"
        Fmt.(list ~sep:sp pp_use_after_exception) uaes
  | UseAfterExceptionObject n ->
      Fmt.pf ppf "USE AFTER EXCEPTION OBJECT %a" pp_name' n

let pp_declarative ppf { declarative_name = n; declarative_segment = s;
                         declarative_use = u; declarative_sentences = ss } =
  pp_name' ppf n;
  let pp_seg = Fmt.(const (option (any " " ++ pp_integer)) s) in
  Fmt.(option (any " SECTION" ++ pp_seg ++
               any ".@;<1 2>" ++ box pp_declarative_use)) ppf u;
  Fmt.pf ppf ".";
  Fmt.(list ~sep:nop (sp ++ pp_with_loc pp_statements ++ any ".")) ppf ss

let pp_declaratives ppf = function
  | [] ->
      ()
  | pd ->
      Fmt.pf ppf "DECLARATIVES.%a@ END DECLARATIVES."
        Fmt.(list ~sep:nop (sp ++ pp_with_loc pp_declarative)) pd

let pp_by_reference ppf { by_reference = n;
                          by_reference_optional = o } =
  if o then Fmt.pf ppf "OPTIONAL ";
  pp_name' ppf n

let pp_procedure_calling_style ppf = function
  | ProcedureArgsUsed -> Fmt.pf ppf "USING"
  | ProcedureArgsChained -> Fmt.pf ppf "CHAINING"

let pp_procedure_by_clause ppf = function
  | ByReference ubrs ->
      Fmt.(list ~sep:sp pp_by_reference) ppf ubrs
  | ByValue ns ->
      Fmt.pf ppf "BY VALUE %a" Fmt.(list ~sep:sp pp_name') ns

let pp_procedure_args ppf
  {procedure_calling_style=style; procedure_by_clause=args} =
  Fmt.pf ppf "%a%a"
    Fmt.(sp ++ pp_with_loc pp_procedure_calling_style) style
    Fmt.(list ~sep:nop (sp ++ pp_with_loc pp_procedure_by_clause)) args

let pp_raising_phrase ppf { raising; raising_factory } =
  if raising_factory then Fmt.pf ppf "FACTORY ";
  pp_name' ppf raising

let pp_procedure_division ppf { procedure_args = pargs;
                                procedure_returning = pur;
                                procedure_raising_phrases = prp;
                                procedure_declaratives = pd;
                                procedure_paragraphs = pp } =
  Fmt.pf ppf "PROCEDURE DIVISION%a%a%a.%a@;<1 2>@[<hv>%a@]"
    Fmt.(option pp_procedure_args) pargs
    Fmt.(option (any "@ RETURNING " ++ pp_with_loc pp_ident)) pur
    Fmt.(if prp == [] then nop
         else any "RAISING " ++
              list ~sep:sp (pp_with_loc pp_raising_phrase)) prp
    Fmt.(if pd != [] then sp ++ pp_declaratives else nop) pd
    Fmt.(list ~sep:sp (pp_with_loc pp_paragraph)) pp
