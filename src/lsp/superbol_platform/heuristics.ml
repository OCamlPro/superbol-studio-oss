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

open Cobol_common.Platform.TYPES

let fill buff ~lookup_len (input: Cobol_preproc.Input.t) =
  match input.source with
  | String str ->
      Buffer.add_substring buff str 0 (min lookup_len (String.length str))
  | Channel ic ->
      (try Buffer.add_channel buff ic lookup_len with End_of_file -> ());
      Stdlib.seek_in ic 0                        (* FIXME: may break on pipes *)

let autodetect_format_of_input (input: Cobol_preproc.Input.t)
  : source_format_id(* Cobol_preproc.Src_format.any *) =
  let lookup_len = 20 in                                 (* 20 as in GnuCOBOL *)
  let buff = Buffer.create lookup_len in
  fill buff ~lookup_len input;
  (* TODO: skip any utf-8 BOM while shifting initial pos_cnum. *)
  (* Cobol_preproc.Src_format.to_config @@ *)
  Cobol_preproc.Src_format.guess_from ~contents_prefix:(Buffer.contents buff)

let autodetect_format ?source_contents filename : source_format_id =
  match source_contents with
  | Some str ->
      autodetect_format_of_input @@
      Cobol_preproc.Input.string ~filename str
  | None ->
      let ic = Ez_file.V1.EzFile.open_in filename in
      try
        autodetect_format_of_input @@
        Cobol_preproc.Input.channel ~filename ic
      with e ->
        Stdlib.close_in ic;
        raise e
