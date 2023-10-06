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
open Text.TYPES

type lexing_directive =
  | LexDirSource:
      'k Src_format.source_format with_loc -> lexing_directive         [@@unboxed]

type copy_statement =
  | CDirCopy of
      {
        library: library;
        suppress_printing: bool;
        replacing: replacing with_loc list;
      }
and replace_statement =
  | CDirReplace of
      {
        also: bool;
        replacing: replacing with_loc list;
      }
  | CDirReplaceOff of
      {
        last: bool;
      }
and library =
  {
    libname: fileloc with_loc;
    cbkname: fileloc with_loc option;
  }
and fileloc = [`Word | `Alphanum] * string
and replacing =
  | ReplaceExact of
      {
        repl_from: pseudotext with_loc;
        repl_to: pseudotext with_loc;
      }
  | ReplacePartial of
      {
        repl_subst: partial_subst with_loc;
        repl_to: string with_loc option;
      }
and partial_subst =
  {
    partial_subst_dir: replacing_direction;
    partial_subst_len: int;
    partial_subst_regexp: Str.regexp;
  }
and replacing_direction = Leading | Trailing

type partial_replacing =
  {
    repl_dir: replacing_direction;
    repl_strict: bool;
  }
