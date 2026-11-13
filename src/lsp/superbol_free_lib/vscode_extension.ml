(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Vscode_json
open Manifest

let vscode_engine = "1.64.0"

let marketplace =
  Manifest.marketplace
    "OCamlPro"
    ~icon:"assets/superbol-128.png"
    ~categories: [
      "Formatters" ;
      "Programming Languages" ;
      "Linters" ;
      "Snippets" ;
      "Debuggers";
      "Other";
    ]

let package =
  Manifest.package
    "SuperBOL"
    ~displayName: "SuperBOL Studio OSS"
    ~description: "Provides a COBOL mode in Visual Studio Code, based on the \
                   SuperBOL LSP server for COBOL, a source level debugger, and \
                   commands for GnuCOBOL 3.2+"
    ~license: "MIT"
    ~version: Version.version
    ~repository: {
      type_ = Some "git" ;
      url = "https://github.com/OCamlPro/superbol-studio-oss"
    }
    ~homepage: "https://superbol.eu"
    ~author: {
      author_name = "SuperBOL at OCamlPro" ;
      author_email = Some "contact@ocamlpro.com"
    }
    ~keywords: [ "cobol" ; "gnucobol" ]
    ~main: "./_dist/superbol-vscode-oss-bundle"
    ~scripts: [
      "compile",
      "make compile";

      "package",
      "make vsix-package";

      "deploy:vsce",
      "make deploy-vsce";

      "deploy:ovsx",
      "make deploy-ovsx";
    ]
    ~dependencies: [
      "@vscode/debugadapter", "^1.61.0";
      "@vscode/debugprotocol", "^1.61.0";

      (* for the debug extension: *)
      "n-readlines", "^1.0.0";

      "vscode-languageclient", "8.0.2";
    ]
    ~devDependencies: [
      "@types/node", "^20.3.2";
      "@types/vscode", "^" ^ vscode_engine;
      "@vscode/test-electron", "^3.1.0";
      "@vscode/vsce", "^2.15.0";
      "esbuild", "^0.15.16";
      "ovsx", "^1.0.2";

      (* for the debug extension: *)
      "typescript", "^5.1.6";
    ]

let cob_extensions_pattern = [ "[cC]{ob,OB,bl,BL,py,PY,bx,BX,bsql}"; "[pP]{co,CO}" ]

let languages =
  [
    Manifest.language "cobol"
      ~aliases: ["COBOL"]
      ~filenamePatterns:(List.map (fun elt -> "*." ^ elt) cob_extensions_pattern)
      ~configuration: "./syntaxes/language-configuration.json";
    Manifest.language "COBOL_GNU_LISTFILE"
      ~aliases: ["LISTFILE"]
      ~extensions: [".lst"]
      ~configuration: "./syntaxes/list-n-dump-configuration.json";
    Manifest.language "COBOL_GNU_DUMPFILE"
      ~aliases: ["DUMPFILE"]
      ~extensions: [".dump"]
      ~configuration: "./syntaxes/list-n-dump-configuration.json";
  ]

let grammars =
  [
    Manifest.grammar ()
      ~language: "cobol"
      ~scopeName: "source.cobol"
      ~path: "./syntaxes/COBOL.tmLanguage.json"
      ~embeddedLanguages: [
        "meta.embedded.block.sql", "sql";
      ];
    Manifest.grammar ()
      ~language: "COBOL_GNU_LISTFILE"
      ~scopeName: "source.gnucobol_listfile"
      ~path: "./syntaxes/listfile.tmLanguage.json";
    Manifest.grammar ()
      ~language: "COBOL_GNU_DUMPFILE"
      ~scopeName: "source.gnucobol_dumpfile"
      ~path: "./syntaxes/dumpfile.tmLanguage.json"
  ]


let debuggers =
  [
    Manifest.debugger "superbol-gdb"
      ~label: "SuperBOL Debugger for GnuCOBOL"
      ~languages: ["cobol"; "COBOL"]
      ~program: "./_dist/superbol-vscode-gdb.js"
      ~runtime: "node"
      ~configurationAttributes:
        (Manifest.any {| {
                "launch": {
                        "required": [],
                        "properties": {
                                "target": {
                                        "type": "string",
                                        "description": "Path to the executable, or name of the module to launch",
                                        "default": "${file}"
                                },
                                "arguments": {
                                        "type": "string",
                                        "description": "Extra arguments for executable",
                                        "default": null
                                },
                                "cwd": {
                                        "type": "string",
                                        "description": "Path to project",
                                        "default": "${workspaceFolder}"
                                },
                                "group": {
                                        "type": "array",
                                        "description": "Compilation groups of executable",
                                        "default": []
                                },
                                "env": {
                                        "type": "object",
                                        "markdownDescription": "Environment variables.\n\nExample: `{ \"COB_LIBRARY_PATH\": \"/usr/lib/cobol/modules\" }`",
                                        "default": null
                                },
                                "coverage": {
                                        "type": "boolean",
                                        "description": "Enable code coverage",
                                        "default": false
                                },
                                "verbose": {
                                        "type": "boolean",
                                        "description": "Debug interactions with gdb",
                                        "default": false
                                },
                                "gdbtty": {
                                        "type": [
                                                "boolean",
                                                "string"
                                        ],
                                        "description": "Enable external display for debug",
                                        "default": false,
                                        "enum": [
                                                true,
                                                false,
                                                "vscode",
                                                "xterm",
                                                "gnome-terminal",
                                                "xfce4-terminal",
                                                "konsole",
                                                "external"
                                        ]
                                },
                                "useCobcrun": {
                                        "type": "boolean",
                                        "markdownDescription": "Debug the target as a module of `cobcrun`",
                                        "default": false
                                },
                                "cobcrunPath": {
                                        "type": "string",
                                        "markdownDescription": "Path to the GnuCOBOL runtime (typically `cobcrun`)",
                                        "default": null
                                },
                                "sourceDirs": {
                                        "type": "array",
                                        "markdownDescription": "Where to find the source code, in addition to the current directory.\n\nExample: `[ \"/usr/share/cobol/sources\" ]`",
                                        "default": []
                                }
                        }
                },
                "attach": {
                        "required": [],
                        "properties": {
                                "target": {
                                        "type": "string",
                                        "description": "Path to the executable",
                                        "default": "${file}"
                                },
                                "arguments": {
                                        "type": "string",
                                        "description": "Extra arguments (only used when the program is restarted)",
                                        "default": null
                                },
                                "cwd": {
                                        "type": "string",
                                        "description": "Path to project",
                                        "default": "${workspaceFolder}"
                                },
                                "group": {
                                        "type": "array",
                                        "description": "Compilation groups of executable",
                                        "default": []
                                },
                                "env": {
                                        "type": "object",
                                        "markdownDescription": "Environment variables.\n\nExample: `{ \"COB_LIBRARY_PATH\": \"/usr/lib/cobol/modules\" }`",
                                        "default": null
                                },
                                "verbose": {
                                        "type": "boolean",
                                        "description": "Debug interactions with gdb",
                                        "default": false
                                },
                                "pid": {
                                        "type": "string",
                                        "description": "PID of the executable",
                                        "default": null
                                },
                                "remoteDebugger": {
                                        "type": "string",
                                        "description": "GDB Server host:port",
                                        "default": null
                                },
                                "useCobcrun": {
                                        "type": "boolean",
                                        "markdownDescription": "Debug the target as a module of `cobcrun`",
                                        "default": false
                                },
                                "cobcrunPath": {
                                        "type": "string",
                                        "markdownDescription": "Path to the GnuCOBOL runtime (typically `cobcrun`)",
                                        "default": null
                                },
                                "sourceDirs": {
                                        "type": "array",
                                        "markdownDescription": "Where to find the source code, in addition to the current directory.\n\nExample: `[ \"/usr/share/cobol/sources\" ]`",
                                        "default": []
                                }
                        }
                }
        } |})
      ~configurationSnippets: [
        Manifest.any {| {
                                "label": "SuperBOL: debug (launch)",
                                "description": "New SuperBOL launch request",
                                "body": {
                                        "name": "${2:SuperBOL: debug (launch)}",
                                        "type": "superbol-gdb",
                                        "request": "launch",
                                        "preLaunchTask": "SuperBOL: build module (debug)",
                                        "useCobcrun": true,
                                        "target": "$${_:{file}}",
                                        "arguments": "",
                                        "cwd": "$${_:{workspaceFolder}}",
                                        "gdbtty": true
                                }
                        } |};
        Manifest.any {| {
                                "label": "SuperBOL: debug (attach local)",
                                "description": "Attach to a local debug session",
                                "body": {
                                        "name": "${2:SuperBOL: debug (attach local)}",
                                        "type": "superbol-gdb",
                                        "request": "attach",
                                        "pid": "${3:0}",
                                        "target": "$${_:{file}}",
                                        "cwd": "$${_:{workspaceFolder}}"
                                }
                        } |};
        Manifest.any {| {
                                "label": "SuperBOL: debug (attach remote)",
                                "description": "Attach to a remote debug session",
                                "body": {
                                        "name": "${2:SuperBOL: debug (attach remote)}",
                                        "type": "superbol-gdb",
                                        "request": "attach",
                                        "remoteDebugger": "${3:host:port}",
                                        "target": "$${_:{file}}",
                                        "cwd": "$${_:{workspaceFolder}}"
                                }
                        } |};
      ]
  ]

let breakpoints =
  [
    Manifest.breakpoint "cobol";
  ]

let configuration =
  let with_superbol_toml_note md =
    md ^ "\n\n*(May be overriden in project-specific `superbol.toml` files)*."
  and with_extra_keyword_commment md =
    (* Be sure to include "COBOL" in descriptions, as users are more likely to
       type "cobol" than "superbol" when searching for extension settings. *)
    md ^ "\n<!-- COBOL -->"
  in
  Manifest.configuration ~title:"SuperBOL" [
    (* Note: entries that belong to the `superbol.cobol` section is transmitted
       to the LSP server, and needs to exactly mirror the `[cobol]` section in
       `superbol.toml`.  There, convention for keys is to use dash-separated
       words in lowercase in order to keep consistency with GnuCOBOL's
       configurations.  *)

    Manifest.PROPERTY.string "superbol.cobol.dialect"
      ~markdownDescription:
        (with_superbol_toml_note
         @@ Pretty.to_string "Default COBOL dialect; `default` is equivalent to `gnucobol`. \
                              The available dialects are %a. \
                              The `-strict` version of a dialect permits using reserved words from \
                              all other dialects as custom COBOL words, as the non-strict version also \
                              reserve these words. \
                              You may also specify an absolute path to a custom GnuCOBOL dialect \
                              configuration file (see `cobc`'s `-conf` option)."
           Fmt.(list ~sep:(any ",@ ") (quote ~mark:"`" string))
           Cobol_config.DIALECT.all_canonical_names)
      ~default:"default"
      ~order:1;

    Manifest.PROPERTY.enum "superbol.cobol.sourceFormat"
      ~title:"Source Reference-format"
      ~markdownDescription:
        (with_superbol_toml_note "Default source reference-format.")
      ~cases:Cobol_config.Options.all_format_names
      ~default:(`String "auto")
      ~order:2;

    Manifest.property "superbol.cobol.sourceFormatByExtension"
      ~title:"Source Format by File Extension"
      ~type_:(`String "object")
      ~additionalProperties:(`O [
          "type", `String "string";
          "enum", `A (List.map (fun s -> `String s)
                        Cobol_config.Options.all_format_names);
        ])
      ~default:(`O [])
      ~markdownDescription:
        (with_superbol_toml_note
           "Per-extension source reference-format overrides. \
            Each entry maps a file extension (without dot, e.g. \
            `cbl`, `cob`) to a source format, taking precedence \
            over the global `#superbol.cobol.sourceFormat#` setting.")
      ~order:3;

    Manifest.PROPERTY.array "superbol.cobol.copybooks"
      ~title:"Copybook Paths"
      ~items:(`O [
          "type", `String "object";
          "required", `A [`String "dir"];
          "properties", `O [
            "dir", `O [
              "type", `String "string";
              "description", `String "Path to copybooks";
            ];
            "file-relative", `O [
              "type", `String "boolean";
            ];
          ];
        ])
      ~default:(`A [                 (* Should match `default_libpath` in
                                        `superbol_project/project_config.ml` *)
          `O [
            "dir", `String ".";
          ];
        ])
      ~markdownDescription:
        (with_superbol_toml_note "List of paths to copybooks.")
      ~order:4;

    Manifest.PROPERTY.strings "superbol.cobol.copyexts"
      ~markdownDescription:
        (with_superbol_toml_note
           "File extensions for copybook resolution")
      ~default:Cobol_common.Copybook.copybook_extensions
      ~order:5;

    Manifest.PROPERTY.strings "superbol.cobol.excludeDirs"
      ~title:"Excluded Directories"
      ~markdownDescription:
        (with_superbol_toml_note
           "Directories that hold no COBOL source file to consider in \
            project-wide operations, such as workspace symbol lookups or the \
            analysis of every COBOL file in the workspace.  Each entry is either \
            a directory name, that matches at any depth (e.g. `node_modules`), \
            or a path relative to the project root when it holds a `/` \
            (e.g. `tests/fixtures`).\n\nNote: files excluded with \
            `#files.exclude#` are also left out of the workspace analysis.")
      ~default:Superbol_project.Config.default_exclude_dirs
      ~order:6;

    (* Paths *)

    Manifest.PROPERTY.string "superbol.cobcPath"
      ~title:"GnuCOBOL Compiler Executable"
      ~default:"cobc"
      ~scope:"machine-overridable"
      ~description:"Path to the GnuCOBOL compiler executable."
      ~order:11;

    (* Flags *)

    Manifest.PROPERTY.bool "superbol.forceSyntaxDiagnostics"
      ~default:false
      ~markdownDescription:
        "Force reporting of syntax diagnostics for dialects other than \
         ``COBOL85``."
      ~order:21;

    Manifest.PROPERTY.bool "superbol.cacheInGlobalStorage"
      ~default:false
      ~markdownDescription:
        (with_extra_keyword_commment
           "Use storage provided by Visual Studio Code for caching. When this \
            setting is set to *false*, the cache related to the contents of a \
            given workspace folder *f* is stored within a directory named \
            *f*`/_superbol/cache`.\n\nSee `#superbol.cache.sizeLimit#` and \
            `#superbol.cache.maxAge#` for how cache files are pruned.")
      ~order:22;

    Manifest.PROPERTY.string "superbol.cache.sizeLimit"
      ~title:"Cache Size Limit"
      ~default:"20G"
      ~markdownDescription:
        (with_extra_keyword_commment
           "Size under which the language server keeps the cache of each \
            project, by reclaiming the entries that were used least recently \
            whenever it writes the cache out (e.g. when it stops).  A number of \
            bytes, with an optional `k`, `M` or `G` suffix for kibibytes, \
            mebibytes or gibibytes (e.g. `500M`, `20G`); leave empty for no \
            limit.")
      ~order:23;

    Manifest.PROPERTY.int "superbol.cache.maxAge"
      ~title:"Cache Entry Maximum Age"
      ~default:0
      ~minimum:0
      ~markdownDescription:
        (with_extra_keyword_commment
           "Number of days after which the language server reclaims cache \
            entries that have not been used, whenever it writes the cache out \
            (e.g. when it stops); `0` keeps them indefinitely.")
      ~order:24;

    (* Workspace analysis *)

    Manifest.PROPERTY.int "superbol.workspaceAnalysis.maxProcesses"
      ~title:"Parallel Analysis Processes"
      ~default:0
      ~minimum:0
      ~markdownDescription:
        (with_extra_keyword_commment
           "Maximum number of processes that the `SuperBOL: Analyze Every COBOL \
            File in the Workspace` command runs at once; `0` stands for one \
            less than the number of available processors.")
      ~order:31;

    Manifest.PROPERTY.int "superbol.workspaceAnalysis.filesPerProcess"
      ~title:"Files per Analysis Process"
      ~default:25
      ~minimum:1
      ~markdownDescription:
        (with_extra_keyword_commment
           "Number of COBOL files that each process launched by the `SuperBOL: \
            Analyze Every COBOL File in the Workspace` command analyzes.")
      ~order:32;

    Manifest.PROPERTY.bool
      "superbol.workspaceAnalysis.publishCachedDiagnosticsOnStartup"
      ~title:"Report Cached Diagnostics on Startup"
      ~default:true
      ~markdownDescription:
        (with_extra_keyword_commment
           "Whenever the language server starts, report the diagnostics of \
            the COBOL files of the workspace that it finds in its caches \
            (e.g. after the `SuperBOL: Analyze Every COBOL File in the \
            Workspace` command has been run in an earlier session).  Files \
            are not analyzed: those that have changed since, or have never \
            been analyzed, are left out until they are opened.")
      ~order:33;

    (* Debugger-specific: *)

    Manifest.PROPERTY.bool
      "superbol.debugger.displayVariableAttributes"
      ~title:"Display Variable Attributes"
      ~default:false
      ~scope:"resource"
      ~markdownDescription:
        (with_extra_keyword_commment
           "Display storage property and field attributes (e.g. size of \
            alphanumerics, digits and scale of numerics).")
      ~order:41;

    Manifest.PROPERTY.null_string "superbol.debugger.libcobPath"
      ~title:"GnuCOBOL Runtime Library"
      ~scope:"machine-overridable"
      ~description:"Path to the GnuCOBOL runtime library file."
      ~order:42;

    Manifest.PROPERTY.string "superbol.debugger.gdbPath"
      ~title:"GNU Debugger Executable"
      ~default:"gdb"
      ~scope:"machine-overridable"
      ~markdownDescription:
        (with_extra_keyword_commment
           "Path to the GNU debugger executable.")
      ~order:43;

    Manifest.PROPERTY.string "superbol.debugger.cobcrunPath"
      ~title:"GnuCOBOL module loader"
      ~default:"cobcrun"
      ~scope:"machine-overridable"
      ~markdownDescription:
        (with_extra_keyword_commment
           "Path to `cobcrun`, the GnuCOBOL module loader.")
      ~order:44;
  ]

let taskDefinitions =
  [
    Manifest.taskDefinition
      "superbol"
      ~properties: [
        Manifest.PROPERTY.bool "forDebug"
          ~description:"Build for debugging";

        Manifest.PROPERTY.bool "forCoverage"
          ~description:"Enable instrumentation for coverage";

        Manifest.PROPERTY.bool "executable"
          ~description:"Build an executable program instead of a module"
          ~default:false;

        Manifest.PROPERTY.null_string "cobcPath"
          ~title:"GnuCOBOL Compiler Executable"
          ~markdownDescription:
            "Path to the GnuCOBOL compiler executable; when `null` or \"\", \
             defaults to the value of \"superbol.cobcPath\" from the workspace \
             configuration, if defined, to \"cobc\" otherwise.";

        Manifest.PROPERTY.null_string "listingsTarget"
          ~title:"Output file or directory for preprocessed program listings"
          ~markdownDescription:
            "Path to a directory where preprocessed program listings are \
             generated; no listing is saved when `null`.";

        Manifest.PROPERTY.array "extraArgs"
          ~description:"Additional arguments passed to `cobc`";
      ]
  ]

let configurationDefaults =
  [
    "[cobol]",
    Defaults [
      "files.autoGuessEncoding", `Bool false;
      "editor.insertSpaces", `Bool true;
      "editor.formatOnType", `Bool false;
      "editor.autoIndent", `String "full";
      "editor.detectIndentation", `Bool false;
      (* TODO: see if we can tune that directly from the client, checking the
         configured format beforehand. *)
      (* "editor.rulers", `A [ *)
      (*   `Float 6.; *)
      (*   `Float 7.; *)
      (*   `Float 72.; *)
      (* ]; *)
      "editor.semanticHighlighting.enabled", `Bool true;
      "editor.wordSeparators", `String "`~!#$%^&*()=+[{]}\\|;:'\",.<>/?";
    ];
    "[sql]",
    Defaults [
      "editor.wordSeparators", `String "`~!@#$%^&*()=+[{]}\\|;:'\",.<>/?";
    ]
  ]

let problemPatterns =
  [
    Manifest.problemPattern
      (Some "^(.*): ?(\\d+): (error|warning): ([^[]*)(\\[(.*)\\])?$")
      ~name:"gnucobol"
      ~file:1
      ~location:2
      ~severity:3
      ~message:4
      ~code:6;
    Manifest.problemPattern
      (Some "^(.*):(\\d+):\\s?(warning|Warnung|[wW]aarschuwing|[aA]lerta|avertissement|упозорење)\\s?:([^[]*)(\\[(.*)\\])?$")
      ~name:"gnucobol-warning"
      ~file:1
      ~location:2
      ~message:4
      ~code:6;
    Manifest.problemPattern
      (Some "^(.*): ?(\\d+):\\s?(error|Fehler|[fF]out|[eE]rrores|[eE]rrores|erreur|грешка)\\s?:\\s?([^[]*)(\\[(.*)\\])?$")
      ~name:"gnucobol-error"
      ~file:1
      ~location:2
      ~message:4
      ~code:6;
    Manifest.problemPattern
      (Some "^(.*): ?(\\d+): (note|Anmerkung|[nN]ota): ([^[]*)(\\[(.*)\\])?$")
      ~name:"gnucobol-note"
      ~file:1
      ~location:2
      ~message:4
      ~code:6;
  ]

let problemMatchers =
  [
    Manifest.problemMatcher ()
      ~name:"gnucobol"
      ~owner:"cobol"
      ~fileLocation:["autodetect"]
      ~pattern:[Manifest.ProblemName "$gnucobol"]
      ~source:"GnuCOBOL";
    Manifest.problemMatcher ()
      ~name:"gnucobol-warning"
      ~owner:"cobol"
      ~fileLocation:["autodetect"]
      ~pattern:[Manifest.ProblemName "$gnucobol-warning"]
      ~severity:"warning"
      ~source:"GnuCOBOL";
    Manifest.problemMatcher ()
      ~name:"gnucobol-error"
      ~owner:"cobol"
      ~fileLocation:["autodetect"]
      ~pattern:[Manifest.ProblemName "$gnucobol-error"]
      ~severity:"error"
      ~source:"GnuCOBOL";
    Manifest.problemMatcher ()
      ~name:"gnucobol-note"
      ~owner:"cobol"
      ~fileLocation:["autodetect"]
      ~pattern:[Manifest.ProblemName "$gnucobol-note"]
      ~severity:"info"
      ~source:"GnuCOBOL";
  ]


(* A product icon, the same in light and dark themes. *)
let codicon name =
  let icon = "$(" ^ name ^ ")" in
  Manifest.{ icon_light = icon; icon_dark = icon }

let commands =
  [
    Manifest.command ()
      ~command:"superbol.server.restart"
      ~title:"Restart Language Server"
      ~category:"SuperBOL";
    Manifest.command ()
      ~command:"superbol.write.project.config"
      ~title:"Write Project Configuration"
      ~category:"SuperBOL"
    (* ~enablement:"!inDebugMode" *);
    Manifest.command ()
      ~command:"superbol.analyze.workspace"
      ~title:"Analyze Every COBOL File in the Workspace"
      ~category:"SuperBOL";
    Manifest.command ()
      ~command:"superbol.cache.show"
      ~title:"Show Caches"
      ~category:"SuperBOL";
    Manifest.command ()
      ~command:"superbol.cache.refresh"
      ~title:"Refresh Caches"
      ~category:"SuperBOL"
      ~icon:(codicon "refresh");
    Manifest.command ()
      ~command:"superbol.cache.prune"
      ~title:"Prune Cache…"
      ~category:"SuperBOL"
      ~icon:(codicon "trash");
    Manifest.command ()
      ~command:"superbol.cache.pruneUnusedSince"
      ~title:"Remove These Entries and Older Ones…"
      ~category:"SuperBOL"
      ~icon:(codicon "trash");
    Manifest.command ()
      ~command:"superbol.copybooks.retrieve"
      ~title:"Retrieve Copybook Directories"
      ~category:"SuperBOL";
    Manifest.command ()
      ~command:"superbol.coverage.show"
      ~title:"Show Coverage"
      ~category:"SuperBOL";
    Manifest.command ()
      ~command:"superbol.coverage.hide"
      ~title:"Hide Coverage"
      ~category:"SuperBOL";
    Manifest.command ()
      ~command:"superbol.coverage.reload"
      ~title:"Update Coverage"
      ~category:"SuperBOL";
    Manifest.command ()
      ~command:"superbol.cfg.open"
      ~title:"Show Control-flow"
      ~category:"SuperBOL";
    Manifest.command ()
      ~command:"superbol.cfg.open.arc"
      ~title:"Show Control-flow as an Arc-diagram"
      ~category:"SuperBOL";
  ]

let menus =
  [
    "editor/context",
    [menu ~command:"superbol.cfg.open" ~group:"superbol"
       ~when_:"editorTextFocus && editorLangId == 'cobol'" ();
     menu ~command:"superbol.cfg.open.arc" ~group:"superbol"
       ~when_:"editorTextFocus && editorLangId == 'cobol'" ()];
    "view/title",
    [menu ~command:"superbol.cache.refresh" ~group:"navigation"
       ~when_:"view == superbol.cacheView" ();
     menu ~command:"superbol.cache.prune" ~group:"navigation"
       ~when_:"view == superbol.cacheView" ()];
    "view/item/context",
    [menu ~command:"superbol.cache.prune" ~group:"inline"
       ~when_:"view == superbol.cacheView && \
               viewItem == superbolCacheFolder" ();
     menu ~command:"superbol.cache.pruneUnusedSince" ~group:"inline"
       ~when_:"view == superbol.cacheView && \
               viewItem == superbolCacheBucket" ()];
    "commandPalette",
    (* Only from the view, as it takes one of its elements *)
    [menu ~command:"superbol.cache.pruneUnusedSince" ~when_:"false" ()];
  ]

let views =
  [
    "explorer",
    [Manifest.view () ~id:"superbol.cacheView" ~name:"SuperBOL Caches"
       ~icon:"$(database)" ~visibility:"collapsed"
       ~when_:"workbenchState != empty"];
  ]

let contributes =
  Manifest.contributes ()
    ~languages
    ~grammars
    ~debuggers
    ~breakpoints
    ~configuration
    ~taskDefinitions
    ~configurationDefaults
    ~semanticTokenScopes: [
      `O [
        "language", `String "cobol";
        "scopes", `O [
          (* Here, we map semantic token types to textmate scope names that are
             defined in `COBOL.tmLanguage.json`: this aligns the theming
             attributes that are associated with the former types with the ones
             that are associated with the latter. *)
          "keyword", `A [`String "keyword.verb.cobol"];
          "macro", `A [`String "keyword.control.directive.cobol"];
        ];
      ]
    ]
    ~problemPatterns
    ~problemMatchers
    ~commands
    ~tomlValidation: [
      Manifest.tomlValidation
        ~fileMatch:"superbol.toml"
        (* TODO: change this address to a more permanent one; also, substitute `master` for a version tag *)
        ~url:"https://raw.githubusercontent.com/OCamlPro/superbol-studio-oss/master/schemas/superbol-schema-1.2.0.json";
    ]
    ~menus
    ~views

let manifest =
  Manifest.vscode
    package
    ~marketplace
    ~engines: ("^" ^ vscode_engine)
    ~activationEvents: (
      "onLanguage:cobol" (* Note: optional since VS Code 1.74, as in
                             `contributes`) *)
      (* XXX: should we really expect mixed-case file extensions like
         `prog.coB`? *)
     ::(List.map (fun elt -> "workspaceContains:**/*." ^ elt) cob_extensions_pattern)
      @ [
      "workspaceContains:{_superbol,superbol.toml}";
      (* "onDebug"; *) (* <- not relevant yet *)
    ])
    ~extensionKind: [
      "workspace";                                 (* <- run on the workspace *)
    ]
    ~contributes
    ~extensionDependencies: [
      "JacquesLucke.gcov-viewer";
    ]
