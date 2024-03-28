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

module EXEC_LANG = struct
  type t = string
  let compare a b = String.(compare (uppercase_ascii a) (uppercase_ascii b))
end
module EXEC_MAP = Stdlib.Map.Make (EXEC_LANG)

type preproc_options =
  {
    verbose: bool;
    libpath: string list;
    config: Cobol_config.t;
    source_format: Cobol_config.source_format_spec;
    exec_preprocs: exec_preprocessor EXEC_MAP.t;
    env: Preproc_env.t;
  }

and exec_preprocessor =
  | Text_preprocessor of (Text.t -> Text.t)

let default =
  {
    verbose = false;
    libpath = [];
    config = Cobol_config.default;
    source_format = Cobol_config.Auto;
    exec_preprocs = EXEC_MAP.empty;
    env = Preproc_env.empty;
  }
