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
    ~icon:"images/superbol-128.png"
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
    ~description: "Provides a COBOL mode in VSCode, based on the SuperBOL LSP \
                   server for COBOL"
    ~license: "MIT"
    ~version: Version.version
    ~repository: {
      type_ = Some "git" ;
      url = "https://github.com/OCamlPro/superbol-studio-oss"
    }
    ~homepage: "https://get-superbol.com"
    ~author: {
      author_name = "SuperBOL at OCamlPro" ;
      author_email = Some "contact@ocamlpro.com"
    }
    ~keywords: [ "cobol" ; "gnucobol" ]
    ~main: "./_dist/superbol-vscode-platform-bundle"
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
      "vscode-languageclient", "8.0.2";
      "polka", "^1.0.0-next.22";
      "sirv", "^2.0.2";

      (* for the debug extension: *)
      "n-readlines", "^1.0.0";
    ]
    ~devDependencies: [
      "@types/vscode", "^" ^ vscode_engine;
      "esbuild", "^0.15.16";
      "fs-extra", "^10.0.1";
      "mocha", "^9.2.2";
      "npm-run-all", "^4.1.5";
      "ovsx", "^0.1.0";
      "prettier", "^2.5.1";
      "@vscode/vsce", "^2.15.0";
      "@vscode/test-electron", "^1.6.1";

      (* for the debug extension: *)
      "typescript", "^5.1.6";
      "@types/node", "^20.3.2";
    ]

let contributes =
  Manifest.contributes ()
    ~languages: [
      Manifest.language "cobol"
        ~aliases: [ "COBOL" ]
        ~filenamePatterns: [ "*.cbl"; "*.cob" ]
    ]
    ~debuggers: [
      Manifest.debugger "gdb"
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
                                        "description": "Path to debugged executable",
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
                                "gdb-path": {
                                        "type": "string",
                                        "description": "Path to gdb",
                                        "default": "gdb"
                                },
                                "libcob-path": {
                                        "type": "string",
                                        "description": "Path to libcob",
                                        "default": null
                                },
                                "group": {
                                        "type": "array",
                                        "description": "Compilation groups of executable",
                                        "default": []
                                },
                                "env": {
                                        "type": "object",
                                        "description": "Environment variables",
                                        "default": null
                                },
                                "coverage": {
                                        "type": "boolean",
                                        "description": "Enable code coverage",
                                        "default": true
                                },
                                "verbose": {
                                        "type": "boolean",
                                        "description": "Debug GDB",
                                        "default": false
                                }
                        }
                },
                "attach": {
                        "required": [],
                        "properties": {
                                "target": {
                                        "type": "string",
                                        "description": "Path to executable",
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
                                "gdb-path": {
                                        "type": "string",
                                        "description": "Path to gdb",
                                        "default": "gdb"
                                },
                                "libcob-path": {
                                        "type": "string",
                                        "description": "Path to libcob",
                                        "default": null
                                },
                                "group": {
                                        "type": "array",
                                        "description": "Compilation groups of executable",
                                        "default": []
                                },
                                "env": {
                                        "type": "object",
                                        "description": "Environment variables",
                                        "default": null
                                },
                                "verbose": {
                                        "type": "boolean",
                                        "description": "Debug GDB",
                                        "default": false
                                },
                                "pid": {
                                        "type": "string",
                                        "description": "PID of the executable",
                                        "default": null
                                },
                                "remote-debugger": {
                                        "type": "string",
                                        "description": "GDB Server host:port",
                                        "default": null
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
                                        "type": "gdb",
                                        "request": "launch",
                                        "preLaunchTask": "SuperBOL: build (debug)",
                                        "target": "$${_:{file}}",
                                        "arguments": "",
                                        "cwd": "$${_:{workspaceFolder}}",
                                        "group": [],
                                        "coverage": true,
                                        "verbose": false
                                }
                        } |};
          Manifest.any {| {
                                "label": "SuperBOL: debug (attach local)",
                                "description": "Attach to a local debug session",
                                "body": {
                                        "name": "${2:SuperBOL: debug (attach local)}",
                                        "type": "gdb",
                                        "request": "attach",
                                        "pid": "${3:0}",
                                        "target": "$${_:{file}}",
                                        "arguments": "",
                                        "cwd": "$${_:{workspaceFolder}}",
                                        "group": [],
                                        "verbose": false
                                }
                        } |};
          Manifest.any {| {
                                "label": "SuperBOL: debug (attach remote)",
                                "description": "Attach to a remote debug session",
                                "body": {
                                        "name": "${2:SuperBOL: debug (attach remote)}",
                                        "type": "gdb",
                                        "request": "attach",
                                        "remote-debugger": "${3:host:port}",
                                        "target": "$${_:{file}}",
                                        "arguments": "",
                                        "cwd": "$${_:{workspaceFolder}}",
                                        "group": [],
                                        "verbose": false
                                }
                        } |};
        ]
    ]
    ~breakpoints: [
      Manifest.breakpoint "cobol";
    ]
    ~configuration:
      (Manifest.configuration ~title:"SuperBOL COBOL"
         [
           Manifest.PROPERTY.array "superbol.cobol.copybooks"
             ~title:"Copybook paths"
             ~description:"List of copybooks paths";

           Manifest.PROPERTY.enum "superbol.cobol.dialect"
             ~cases:Cobol_config.DIALECT.all_canonical_names
             ~description: "Default COBOL dialect; \"default\" is equivalent to \
                            \"gnucobol\""
             ~default:(`String Cobol_config.(DIALECT.to_string Default));

           Manifest.PROPERTY.enum "superbol.cobol.source-format"
             ~description: "Default source reference-format"
             ~cases:Cobol_config.Options.all_format_names
             ~default:(`String "auto");

           (* Paths *)

           Manifest.PROPERTY.string "superbol.lsp-path"
             ~title:"SuperBOL executable"
             ~default:""
             ~description:
               "Name of the `superbol-free` executable if available in PATH; may \
                be an absolute path otherwise. Leave empty to use the bundled \
                `superbol-free`, if available.";

           Manifest.PROPERTY.string "superbol.cobc-path"
             ~title:"GnuCOBOL compiler executable"
             ~default:"cobc"
             ~description:"Path to the GnuCOBOL compiler executable.";

           (* Debugger-specific: *)

           Manifest.PROPERTY.bool
             "superbol.debugger.display-variable-attributes"
             ~default:false
             ~scope:"resource"
             ~description:"Display storage property and field attributes \
                           (e.g. size of alphanumerics, digits and scale of \
                           numerics).";

           Manifest.PROPERTY.string "superbol.debugger.gdb-path"
             ~title:"GNU debugger executable"
             ~default:"gdb"
             ~description:"Path to the GNU debugger executable.";

           Manifest.PROPERTY.string "superbol.debugger.libcob-path"
             ~title:"GnuCOBOL runtime library"
             ~default:"cobc"
             ~description:"Path to the GnuCOBOL runtime library file.";
         ])
    ~taskDefinitions: [
      Manifest.taskDefinition
        "superbol"
        ~properties: [
          Manifest.PROPERTY.bool "for-debug"
            ~description: "Build for debugging";

          Manifest.PROPERTY.null_string "cobc-path"
            ~title:"GnuCOBOL compiler executable"
            ~description:"Path to the GnuCOBOL compiler executable; when `null`, \
                          defaults to the value of \"superbol.cobc-path\" from \
                          the workspace configuration, if defined, to \"cobc\" \
                          otherwise.";

          Manifest.PROPERTY.array "extra-args"
            ~description:"Additional arguments passed to `cobc`";
        ]
    ]
    ~problemPatterns: [
      Manifest.problemPattern
        (Some "^(.*): ?(\\d+): (error|warning): ([^[]*)(\\[(.*)\\])?$")
        ~name:"gnucobol"
        ~file:1
        ~line:2
        ~severity:3
        ~message:4
        ~code:6;
      Manifest.problemPattern
        (Some "^(.*):(\\d+):\\s?(warning|Warnung|[wW]aarschuwing|[aA]lerta|avertissement|упозорење)\\s?:([^[]*)(\\[(.*)\\])?$")
        ~name:"gnucobol-warning"
        ~file:1
        ~line:2
        ~message:4
        ~code:6;
      Manifest.problemPattern
        (Some "^(.*): ?(\\d+):\\s?(error|Fehler|[fF]out|[eE]rrores|[eE]rrores|erreur|грешка)\\s?:\\s?([^[]*)(\\[(.*)\\])?$")
        ~name:"gnucobol-error"
        ~file:1
        ~line:2
        ~message:4
        ~code:6;
      Manifest.problemPattern
        (Some "^(.*): ?(\\d+): (note|Anmerkung|[nN]ota): ([^[]*)(\\[(.*)\\])?$")
        ~name:"gnucobol-note"
        ~file:1
        ~line:2
        ~message:4
        ~code:6;
    ]
    ~problemMatchers:[
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
    ~commands: [
      Manifest.command ()
        ~command:"superbol.server.restart"
        ~title:"Restart Language Server"
        ~category:"SuperBOL"
    ]

let manifest =
  Manifest.vscode
    package
    ~marketplace
    ~engines: ("^" ^ vscode_engine)
    ~activationEvents: [
      "onLanguage:cobol";
      "onDebug";
    ]
    ~contributes
