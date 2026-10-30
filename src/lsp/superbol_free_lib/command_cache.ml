(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)


(** Inspecting, filling and reclaiming the caches the LSP server writes. *)

open Ezcmd.V2
open EZCMD.TYPES
open Superbol_cache.Types

module CACHE = Superbol_cache.Cache

let size_n_unit bytes =
  let rec pick n = function
    | _ :: (_ :: _ as units) when n >= 1024. -> pick (n /. 1024.) units
    | suffix :: _ -> n, suffix
    | [] -> n, "B"
  in
  pick (Int64.to_float bytes) ["B"; "kiB"; "MiB"; "GiB"]

let pp_age ppf seconds =
  let days = seconds /. 86400. in
  if days >= 1.
  then Pretty.print ppf "%.0f day(s)"  days
  else if seconds >= 3600.
  then Pretty.print ppf "%.0f hour(s)" (seconds /. 3600.)
  else Pretty.print ppf "%.0f minute(s)" (seconds /. 60.)

(* --- *)

let status ~verbose dir =
  match Caching_commands.cache_for dir with
  | None -> ()
  | Some (root, cache) ->
      match CACHE.stats cache, CACHE.entries cache with
      | Ok stats, Ok entries ->
          Pretty.out "Cache: %s@." root;
          Pretty.out "Format: %s, producer: %s@."
            stats.stats_format_version stats.stats_producer;
          Pretty.out "%d entries, %d values, %a@."
            stats.stats_entries stats.stats_objects
            Fmt.(pair ~sep:(any " ") (fmt "%.1f") string)
            (size_n_unit stats.stats_disk_usage);
          if verbose then begin
            let now = Unix.gettimeofday () in
            List.iter begin fun e ->
              Pretty.out "  %-40s %a  %d dependencies, used %a ago@."
                e.info_label
                Fmt.(pair ~sep:(any " ") (fmt "%6.1f") (fmt "%3s"))
                (size_n_unit e.info_disk_usage)
                (List.length e.info_witnesses)
                pp_age (now -. e.info_last_hit)
            end entries
          end
      | _ ->
          Pretty.error "Could not read the cache at %s@." root

let trim ~older_than ~max_size dir =
  match Caching_commands.cache_for dir with
  | None -> ()
  | Some (root, cache) ->
      (* Not [Never], even without bounds: that would skip reclaiming the
         leftovers of instances that died mid-write. *)
      let policy =
        Bounded { max_age = Option.map (( *. ) 86400.) older_than; max_size }
      in
      match CACHE.prune cache ~policy with
      | Ok removed ->
          Pretty.out "Reclaimed %d entries in %s@." removed root
      | Error _ ->
          Pretty.error "Could not prune the cache at %s@." root

(* --- *)

let dir_arg dir =
  [ [], Arg.Anon (0, fun s -> dir := s),
    EZCMD.info ~docv:"DIR" "Directory within the project (default: `.')" ]

let status_cmd =
  let dir = ref "." and verbose = ref false in
  EZCMD.sub "cache status"
    (fun () -> status ~verbose:!verbose !dir)
    ~args:(dir_arg dir @
           [ ["list-entries"; "l"], Arg.Set verbose,
             EZCMD.info "List every entry, not just the totals" ])
    ~doc:"Report on the cache of a project"
    ~man:[ `S "DESCRIPTION";
           `Blocks [ `P "Show where the cache of the project containing DIR \
                         lives, how much room it takes, and -- with $(b,-l) -- \
                         what each entry holds and when it was last used." ]]

let trim_cmd =
  let dir = ref "." and older_than = ref None and max_size = ref None in
  EZCMD.sub "cache trim"
    (fun () -> trim ~older_than:!older_than ~max_size:!max_size !dir)
    ~args:(dir_arg dir @
           [ ["older-than"],
             Arg.Float (fun d -> older_than := Some d),
             EZCMD.info ~docv:"DAYS"
               "Reclaim entries unused for that many days";
             ["max-size"],
             Arg.String begin fun s -> match Arg_utils.size_of_string s with
               | Ok size -> max_size := Some size
               | Error () -> Pretty.failwith "Not a size: %s" s
             end,
             EZCMD.info ~docv:"SIZE"
               "Keep the cache under that size, reclaiming the entries least \
                recently used.  Accepts a suffix: 500M, 2G, 64k" ])
    ~doc:"Reclaim entries a project's cache no longer needs"
    ~man:[ `S "DESCRIPTION";
           `Blocks [ `P "Reclaim entries of the project containing DIR, along \
                         with every value they alone held.";
                     `P "$(b,--older-than) removes those unused for that many \
                         days; $(b,--max-size) removes the least recently used \
                         until the cache fits.  Both may be given, and with \
                         neither nothing is reclaimed but the leftovers of \
                         instances that died mid-write.  $(b,--older-than 0) \
                         empties the cache." ]]

let analyze_cmd =
  let files = ref [] and ppf = ref Fmt.stderr in
  let common, common_args = Common_args.get () in
  EZCMD.sub "cache analyze"
    (fun () ->
       Caching_commands.analyze_n_report (common ()) ~for_lsp:true
         ~ppf:!ppf !files)
    ~args:(common_args @
           [ [], Arg.Anons (fun list -> files := list),
             EZCMD.info ~docv:"FILE" "Cobol file to analyze";
             ["stdout"], Arg.Unit (fun () -> ppf := Fmt.stdout),
             EZCMD.info "Output errors/warnings to stdout" ])
    ~doc:"Analyze files, reusing and filling the cache of their project"
    ~man:[ `S "DESCRIPTION";
           `Blocks [ `P "Preprocess, parse and typecheck each FILE as the LSP \
                         server would, according to the configuration of the \
                         project it belongs to, and report diagnostics.";
                     `P "Analysis results are looked up in, or else stored into, \
                         the cache of that project, which is shared with the LSP \
                         server: a file analyzed by either is restored by the \
                         other, for as long as neither the file nor anything it \
                         depends upon (copybooks, project configuration) changes.";
                     `P "Options such as $(b,--dialect), $(b,--conf), \
                         $(b,--source-format), $(b,-I), $(b,--ext), $(b,-D), \
                         $(b,--recovery) or $(b,--silence) override the project \
                         configuration; analyses obtained with them are cached \
                         separately, and are not shared with the LSP server.";
                     `P "Files that appear to be copybooks are not analyzed. \
                         With $(b,--progress log-err), tell whether each \
                         analysis was found in the cache." ]]
