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

open Ez_file.V1
open EzFile.OP
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

let pp_size =
  Fmt.(using size_n_unit @@ pair ~sep:(any " ") (fmt "%.1f") string)

let pp_age ppf seconds =
  let days = seconds /. 86400. in
  if days >= 1.
  then Pretty.print ppf "%.0f day(s)"  days
  else if seconds >= 3600.
  then Pretty.print ppf "%.0f hour(s)" (seconds /. 3600.)
  else Pretty.print ppf "%.0f minute(s)" (seconds /. 60.)

(* --- Locating the cache --- *)

(** Where to look for a cache: [dir], and whether it was given as the root of
    the project, rather than as some directory within it. *)
type location =
  {
    dir: string;
    is_root: bool;
    storage_directory: string option;
  }

(* The root of a project is spelled as the LSP server spells it, as the location
   of a cache held in a shared storage directory is derived from it. *)
let rootdir_of ~layout { dir; is_root; _ } =
  let dirname = Project.absolute_dirname dir in
  if is_root
  then Superbol_project.rootdir_at ~dirname
  else Superbol_project.rootdir_for ~layout
      ~filename:(dirname // "_")    (* the project of anything held in [dir] *)

(** [with_cache location f] is [f ~rootdir cache], where [rootdir] is the root
    of the project at [location], and [cache] the location and handle of its
    cache, if any. *)
let with_cache ({ storage_directory; _ } as location) f =
  let setup = Project_commands.lsp_like_setup ?storage_directory () in
  let rootdir =
    try
      Superbol_project.string_of_rootdir @@
      rootdir_of ~layout:setup.layout location
    with Invalid_argument msg ->
      Cobol_common.fatal "%s" msg
  in
  f ~rootdir (Project_commands.cache_for ~setup rootdir)

(* --- JSON --- *)

(* Sizes may exceed what an [int] holds in builds into JavaScript. *)
let json_int64 i = `Intlit (Int64.to_string i)

let json_of_stats stats =
  [ "format", `String stats.stats_format_version;
    "producer", `String stats.stats_producer;
    "entries", `Int stats.stats_entries;
    "values", `Int stats.stats_objects;
    "disk_usage", json_int64 stats.stats_disk_usage ]

let json_of_entry_info e =
  `Assoc [ "key", `String e.info_key;
           "label", `String e.info_label;
           "dependencies", `Int (List.length e.info_witnesses);
           "disk_usage", json_int64 e.info_disk_usage;
           "last_used", `Float e.info_last_hit;
           "value", `String (Digest.to_hex e.info_object);
           "value_size", json_int64 e.info_object_size ]

let output_json ~rootdir ~cache_root fields =
  print_endline @@ Yojson.Safe.to_string @@
  `Assoc ([ "project_root", `String rootdir;
            "cache", Option.fold cache_root ~none:`Null
              ~some:(fun s -> `String s) ] @ fields)

(* --- *)

let stats cache ~cache_root =
  match CACHE.stats cache with
  | Ok stats -> stats
  | Error _ -> Cobol_common.fatal "Could not read the cache at %s" cache_root

let status ~json ~verbose location =
  with_cache location @@ fun ~rootdir -> function
  | None when json ->
      output_json ~rootdir ~cache_root:None []
  | None ->
      Pretty.out "Project %s has no cache@." rootdir
  | Some (cache_root, cache) ->
      let stats = stats cache ~cache_root in
      let entries () =
        match CACHE.entries cache with
        | Ok entries -> entries
        | Error _ ->
            Cobol_common.fatal "Could not read the cache at %s" cache_root
      in
      let now = Unix.gettimeofday () in
      if json then
        output_json ~rootdir ~cache_root:(Some cache_root) @@
        json_of_stats stats @
        [ "now", `Float now ] @
        if verbose
        then
          (* Tail-recursively, as caches may hold tens of thousands of entries,
             and the stack is limited in builds into JavaScript. *)
          [ "items", `List (List.rev @@
                            List.rev_map json_of_entry_info (entries ())) ]
        else []
      else begin
        Pretty.out "Project: %s@." rootdir;
        Pretty.out "Cache: %s@." cache_root;
        Pretty.out "Format: %s, producer: %s@."
          stats.stats_format_version stats.stats_producer;
        Pretty.out "%d entries, %d values, %a@."
          stats.stats_entries stats.stats_objects pp_size
          stats.stats_disk_usage;
        if verbose then
          List.iter begin fun e ->
            Pretty.out "  %-40s %a  %d dependencies, used %a ago@."
              e.info_label
              Fmt.(using size_n_unit @@
                   pair ~sep:(any " ") (fmt "%6.1f") (fmt "%3s"))
              e.info_disk_usage
              (List.length e.info_witnesses)
              pp_age (now -. e.info_last_hit)
          end (entries ())
      end

let trim ~json ~older_than ~max_size location =
  with_cache location @@ fun ~rootdir -> function
  | None when json ->
      output_json ~rootdir ~cache_root:None []
  | None ->
      Pretty.out "Project %s has no cache@." rootdir
  | Some (cache_root, cache) ->
      (* Not [Never], even without bounds: that would skip reclaiming the
         leftovers of instances that died mid-write. *)
      let policy =
        Bounded { max_age = Option.map (( *. ) 86400.) older_than; max_size }
      in
      match CACHE.prune cache ~policy with
      | Ok removed when json ->
          output_json ~rootdir ~cache_root:(Some cache_root) @@
          [ "reclaimed", `Int removed ] @
          json_of_stats (stats cache ~cache_root)
      | Ok removed ->
          Pretty.out "Reclaimed %d entries in %s@." removed cache_root
      | Error _ ->
          Cobol_common.fatal "Could not prune the cache at %s" cache_root

(* --- *)

let location_args () =
  let dir = ref "." and project_root = ref None
  and storage_directory = ref None in
  let args =
    [ [], Arg.Anon (0, fun s -> dir := s),
      EZCMD.info ~docv:"DIR" "Directory within the project (default: `.')";
      ["project-root"; "root"], Arg.String (fun s -> project_root := Some s),
      EZCMD.info ~docv:"DIR"
        "Designate the project whose root is DIR, rather than the one the \
         positional DIR belongs to";
      ["storage-directory"],
      Arg.String (fun s -> storage_directory := Some s),
      EZCMD.info ~docv:"DIR"
        "Use the caches that an LSP server started with the same option uses";
    ]
  in
  (fun () ->
     let dir, is_root = match !project_root with
       | Some root -> root, true
       | None -> !dir, false
     in
     { dir; is_root; storage_directory = !storage_directory }),
  args

let json_arg json =
  ["json"], Arg.Set json,
  EZCMD.info "Output a single JSON object, for use by other programs"

let location_man =
  `P "The project containing DIR is the one whose root is the closest parent \
      directory of DIR that holds a `superbol.toml' file, or else DIR itself.  \
      $(b,--project-root) designates a project by its root instead: Visual \
      Studio Code puts every file of a workspace folder in the project of that \
      folder.  With $(b,--storage-directory), the cache is that of an LSP \
      server started with the same option."

let status_cmd =
  let json = ref false and verbose = ref false in
  let location, location_args = location_args () in
  EZCMD.sub "cache status"
    (fun () -> status ~json:!json ~verbose:!verbose (location ()))
    ~args:(location_args @
           [ ["list-entries"; "l"], Arg.Set verbose,
             EZCMD.info "List every entry, not just the totals";
             json_arg json ])
    ~doc:"Report on the cache of a project"
    ~man:[ `S "DESCRIPTION";
           `Blocks [ `P "Show where the cache of the project containing DIR \
                         lives, how much room it takes, and -- with $(b,-l) -- \
                         what each entry holds and when it was last used.";
                     location_man;
                     `P "With $(b,--json), the totals are given as fields \
                         $(b,entries), $(b,values) and $(b,disk_usage) (in \
                         bytes), along with $(b,now) (in seconds since the \
                         epoch); and with $(b,-l), field $(b,items) lists \
                         every entry, with the date it was $(b,last_used), \
                         and the digest ($(b,value)) and size \
                         ($(b,value_size)) of the value it holds --- values \
                         may be shared between entries.  Field $(b,cache) is \
                         $(b,null) for a project that has no cache." ]]

let trim_cmd =
  let json = ref false and older_than = ref None and max_size = ref None in
  let location, location_args = location_args () in
  EZCMD.sub "cache trim"
    (fun () ->
       trim ~json:!json ~older_than:!older_than ~max_size:!max_size
         (location ()))
    ~args:(location_args @
           [ ["older-than"],
             Arg.Float (fun d -> older_than := Some d),
             EZCMD.info ~docv:"DAYS"
               "Reclaim entries unused for that many days";
             ["max-size"],
             Arg.String begin fun s -> match Arg_utils.size_of_string s with
               | Ok size -> max_size := Some size
               | Error () -> Cobol_common.fatal "Not a size: %s" s
             end,
             EZCMD.info ~docv:"SIZE"
               "Keep the cache under that size, reclaiming the entries least \
                recently used.  Accepts a suffix: 500M, 2G, 64k";
             json_arg json ])
    ~doc:"Reclaim entries a project's cache no longer needs"
    ~man:[ `S "DESCRIPTION";
           `Blocks [ `P "Reclaim entries of the project containing DIR, along \
                         with every value they alone held.";
                     `P "$(b,--older-than) removes those unused for that many \
                         days; $(b,--max-size) removes the least recently used \
                         until the cache fits.  Both may be given, and with \
                         neither nothing is reclaimed but the leftovers of \
                         instances that died mid-write.  $(b,--older-than 0) \
                         empties the cache.";
                     `P "Nothing is reclaimed while another process is \
                         pruning the same cache.";
                     location_man;
                     `P "With $(b,--json), the number of entries reclaimed is \
                         given as field $(b,reclaimed), along with the totals \
                         that $(b,cache status --json) gives." ]]

let client_settings_of_string s =
  match Yojson.Safe.from_string s with
  | `Assoc assoc -> assoc
  | `Null -> []
  | _ | exception Yojson.Json_error _ ->
      Cobol_common.fatal "Expected a JSON object: %s" s

let analyze_cmd =
  let files = ref [] and ppf = ref Fmt.stderr
  and storage_directory = ref None and client_settings = ref None
  and error_status = ref true and show_diagnostics = ref true
  and for_lsp = ref true in
  let common, common_args = Common_args.get () in
  let parallel, parallel_args = Parallel_batches.args () in
  let project_setup, project_args = Project_args.get () in
  let analyze files =
    let setup =
      project_setup () ~setup:
        (Project_commands.lsp_like_setup ()
           ?storage_directory:!storage_directory
           ?client_settings:!client_settings)
    in
    Project_commands.analyze_n_report (common ()) ~for_lsp:!for_lsp ~setup
      ~error_status:!error_status ~show_diagnostics:!show_diagnostics
      ~ppf:!ppf files
  in
  (* Processes that analyze batches of files are given every option that was
     given to this one, except [--jobs] and [--batch-size]; {!Parallel_batches}
     passes the verbosity on with [-v], and colors and margins through the
     environment. *)
  let given_args, args =
    Arg_utils.recording @@
    common_args @ project_args @
    [ [], Arg.Anons (fun list -> files := list),
      EZCMD.info ~docv:"FILE" "Cobol file to analyze";
             ["stdout"], Arg.Unit (fun () -> ppf := Fmt.stdout),
             EZCMD.info "Output errors/warnings to stdout";
             ["no-error-status"], Arg.Clear error_status,
             EZCMD.info "Exit with status 0 even when analyzed files hold \
                         errors (errors that prevent analyzing a file still \
                         make the program exit with a non-zero status)";
             ["no-diagnostics"], Arg.Clear show_diagnostics,
             EZCMD.info "Do not output the diagnostics of analyzed files \
                         (errors that prevent analyzing a file are still \
                         reported)";
             ["no-lsp-artifacts"], Arg.Clear for_lsp,
             EZCMD.info "Leave out what only the LSP server needs (it does \
                         not reuse analyses stored this way)";
             ["storage-directory"],
             Arg.String (fun s -> storage_directory := Some s),
             EZCMD.info ~docv:"DIR"
               "Use the caches that an LSP server started with the same \
                option uses";
             ["client-settings"],
             Arg.String begin fun s ->
               client_settings := Some (client_settings_of_string s)
             end,
             EZCMD.info ~docv:"JSON"
               "Configure projects that have no `superbol.toml' file as an LSP \
                server does when its client sends these settings of the \
                `superbol.cobol' section (e.g. `{\"dialect\": \"ibm\"}')" ]
  in
  EZCMD.sub "cache analyze"
    (fun () ->
       let jobs, batch_size = parallel () in
       (* Processes report progress on the files they analyze as asked, except
          on a line of their own, as their outputs are copied when they end. *)
       let args = List.filter ((<>) "--progress=force") (given_args ()) in
       Parallel_batches.run ~jobs ?batch_size ~command:["cache"; "analyze"]
         ~progress:(common ()).progress_display ~args
         ~sequential:analyze !files)
    ~args:(args @ parallel_args)
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
                     `P "With $(b,--no-lsp-artifacts), what only the LSP \
                         server needs (e.g. semantic tokens, workspace symbols) \
                         is neither computed nor stored.  The LSP server does \
                         not reuse analyses stored this way, whereas analyses \
                         it stored are reused all the same.";
                     `P "An LSP client such as Visual Studio Code may configure \
                         projects that have no configuration file through its \
                         settings: $(b,--client-settings) gives those settings, \
                         and $(b,--storage-directory) designates the caches of \
                         an LSP server started with that option, so that \
                         analyses are shared with that server.";
                     `P "Visual Studio Code decodes files according to its \
                         $(b,files.encoding) setting before sending them to \
                         the server: analyses of files that are not plain \
                         UTF-8 are only shared with the server when \
                         $(b,--encoding) gives that same encoding.";
                     `P "Options such as $(b,--dialect), $(b,--conf), \
                         $(b,--source-format), $(b,-I), $(b,--ext), $(b,-D), \
                         $(b,--recovery) or $(b,--silence) override the project \
                         configuration; analyses obtained with them are cached \
                         separately, and are not shared with the LSP server.";
                     `P "A file that cannot be analyzed (e.g. because it \
                         cannot be read) is reported, and the remaining files \
                         are analyzed all the same.  The exit status is then at \
                         least 2; it is odd whenever some analyzed file holds \
                         errors, unless $(b,--no-error-status) is given.";
                     `P "Files that appear to be copybooks are not \
                         analyzed. With $(b,--progress log-err), tell whether \
                         each analysis was found in the cache.";
                     `P "With $(b,--jobs N), files are split into batches \
                         (see $(b,--batch-size)), each analyzed by a process \
                         of its own, with up to N processes running at once.  \
                         The output of each process is shown as a whole when \
                         it ends, so diagnostics follow the order in which \
                         batches end rather than that of the given files.  The \
                         exit status is computed over all of them.  Builds of \
                         this program into JavaScript analyze files in turn \
                         instead." ]]
