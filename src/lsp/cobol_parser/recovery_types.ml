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
(*                                                                        *)
(* Copyright (c) 2013-2022 Frédéric Bour, Thomas Refis and                *)
(*   Simon Castellan.                                                     *)
(*                                                                        *)
(* Permission is hereby granted, free of charge, to any person obtaining  *)
(* a copy of this software and associated documentation files (the        *)
(* "Software"), to deal in the Software without restriction, including    *)
(* without limitation the rights to use, copy, modify, merge, publish,    *)
(* distribute, sublicense, and/or sell copies of the Software, and to     *)
(* permit persons to whom the Software is furnished to do so, subject to  *)
(* the following conditions:                                              *)
(*                                                                        *)
(* The above copyright notice and this permission notice shall be         *)
(* included in all copies or substantial portions of the Software.        *)
(*                                                                        *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,        *)
(* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF     *)
(* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND                  *)
(* NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE *)
(* LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION *)
(* OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION  *)
(* WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.        *)
(*                                                                        *)
(**************************************************************************)

module type PARAMS = sig
  module Parser: MenhirLib.IncrementalEngine.EVERYTHING

  val default_value: pos:Lexing.position -> 'a Parser.symbol -> 'a
  val token_of_terminal: 'a Parser.terminal -> 'a -> Parser.token
  val depth: int array

  type action =
    | Abort
    | R of int
    | S: 'a Parser.symbol -> action
    | Sub of action list
  and decision =
    | Nothing
    | One of action list
    | Select of (int -> action list)

  val recover: int -> decision
end

type ('symbol, 'token) generic_insertion =
  | Symbol of 'symbol
  | Token of 'token

module Driver_types
    (Parser: MenhirLib.IncrementalEngine.EVERYTHING) =
struct
  type insertion = (Parser.xsymbol, Parser.token) generic_insertion
  type 'value candidates =
    {
      final: ('value * 'value assumption list) option;
      candidates: 'value candidate list;
    }
  and 'value assumption =
    {
      insertion: insertion;
      pos: Lexing.position;
    }
  and 'value candidate =
    {
      env: 'value Parser.env;
      visited: 'value operation list;
      assumed: 'value assumption list;
    }
  and 'value operation =
    | Shift of 'value Parser.env * 'value Parser.env
    | Reduce of Parser.production * 'value Parser.env option
end

module type DRIVER = sig
  module Parser: MenhirLib.IncrementalEngine.EVERYTHING
  include module type of Driver_types (Parser)
  val generate
    : 'value Parser.env
    -> 'value candidates
  val attempt
    : 'value candidates
    -> Parser.token * Lexing.position * Lexing.position
    -> [> `Accept of 'value * 'value assumption list | `Fail |
          `Ok of 'value Parser.checkpoint * 'value Parser.env *
                 'value operation list * 'value assumption list ]
end
