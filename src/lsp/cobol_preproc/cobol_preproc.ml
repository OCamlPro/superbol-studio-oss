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

(** {1 Source format} *)

module Src_format = Src_format

(** {1 Text}

  "Text" refers to the source after manipulations by preprocessor statements. *)

type text = Text.text
type comments = Text.comments
module Text = Text
module Text_printer = Text_printer

(** {1 Miscellaneous support modules}  *)

module Src_overlay = Src_overlay
module Copybook = Copybook
module Trace = Preproc_trace
include Trace.TYPES

(** {1 Main entry points for the processor itself} *)

module Options = Preproc_options
include Preproc_engine
