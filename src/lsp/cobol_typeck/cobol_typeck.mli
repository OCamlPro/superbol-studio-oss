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

(** Type-checking and validation of COBOL compilation groups *)

module OLD: sig
  include module type of Old_typeck_engine

  (** {1 Access to independent builder modules} *)

  module Env_builder = Old_env_builder
  module Group_builder = Old_group_builder
  module Prog_builder = Old_prog_builder
end

include module type of Typeck_engine
