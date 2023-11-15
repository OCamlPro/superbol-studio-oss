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

module OLD = struct
  include Old_typeck_engine

  module Env_builder = Old_env_builder
  module Group_builder = Old_group_builder
  module Prog_builder = Old_prog_builder
end

include Typeck_engine
(* module Units = Typeck_units *)
(* module Config = Typeck_config *)
(* module Proc_div = Typeck_proc_div *)
