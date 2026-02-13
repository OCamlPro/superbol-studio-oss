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

type error =
  | Invalid_key_value_pair of string * Conf_ast.value
  | Lexing_error of Conf_lexer.error
  | Syntax_error of Cobol_common.Srcloc.srcloc option * string option
  | Unknown_dialect of string
  | Missing_file of string * string list

let pp_error ppf = function
  | Invalid_key_value_pair (k, v) ->
      Pretty.print ppf "Invalid@ type@ of@ value@ (%a)@ given@ for@ key@ %s"
        Conf_ast.pp_value v k
  | Lexing_error (Unexpected_char (c, lexloc)) ->
      let loc = Cobol_common.Srcloc.raw lexloc in
      Pretty.print ppf "%aUnexpected@ character@ `%c'"
        Cobol_common.Srcloc.pp_srcloc loc c
  | Lexing_error (Unexpected_end_of_string lexloc) ->
      let loc = Cobol_common.Srcloc.raw lexloc in
      Pretty.print ppf "%aUnexpected@ end@ of@ string"
        Cobol_common.Srcloc.pp_srcloc loc
  | Syntax_error (loc, msg) ->
      Pretty.print ppf "%aSyntax@ error%a"
        (Pretty.option Cobol_common.Srcloc.pp_srcloc) loc
        Fmt.(option (any ": " ++ string)) msg
  | Unknown_dialect name ->
      Pretty.print ppf "Unknown@ dialect@ `%s'" name
  | Missing_file (filename, search_path) ->
      Pretty.print ppf "Configuration@ file@ `%s'@ not@ found@ (search@ path:@ \
                        %a)" filename Pretty.path search_path
