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

type preproc_options =
  {
    config: Cobol_config.t;
    platform: Cobol_common.Platform.TYPES.platform;
    copybook_lookup_config: Cobol_common.Copybook.TYPES.lookup_config;
    source_format: Cobol_config.source_format_spec;
    mf_comments: bool;                            (** see {!Src_options.t} *)
    env: Preproc_env.t;
  }

let default ~platform =
  {
    copybook_lookup_config = Cobol_common.Copybook.lookup_config [];
    platform;
    config = Cobol_config.default;
    source_format = Cobol_config.Auto;
    mf_comments = false;
    env = Preproc_env.empty;
  }
