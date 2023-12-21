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
      "Other";
    ]

let package =
  Manifest.package
    "superbol"
    ~displayName: "SuperBOL Studio OSS"
    ~description: "Provides a COBOL mode in VSCode, based on the SuperBOL LSP \
                   server for COBOL"
    ~license: "MIT"
    ~version: "0.1.0"
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
    ~main: "./_dist/superbol_vscode_platform.bc.js"
    ~scripts: [
      "compile",
      "make compile" ;

      "release",
      "make release" ;

      "package" ,
      "vsce package --out superbol-vscode-platform.vsix --yarn" ;

      "deploy:vsce" ,
      "vsce publish --packagePath superbol-vscode-platform.vsix --yarn" ;

      "deploy:ovsx" ,
      "ovsx publish --yarn"
    ]
    ~dependencies: [
      "@vscode/debugadapter", "^1.61.0" ;
      "@vscode/debugprotocol", "^1.61.0" ;
      "polka" , "^1.0.0-next.22" ;
      "sirv" , "^2.0.2" ;
      "vscode-languageclient" , "8.0.2"
    ]
    ~devDependencies: [
      "@types/vscode" , vscode_engine ;
      "esbuild" , "0.15.16" ;
      "fs-extra" , "10.0.1" ;
      "mocha" , "9.2.2" ;
      "npm-run-all" , "4.1.5" ;
      "ovsx" , "0.1.0-next.97d460c" ;
      "prettier" , "^2.5.1" ;
      "vsce" , "^2.15.0" ;
      "vscode-test" , "1.6.1"
    ]

let contributes =
  Manifest.contributes ()
    ~languages: [
      Manifest.language "cobol"
        ~aliases: [ "COBOL" ]
        ~filenamePatterns: [ "*.cbl"; "*.cob" ]
    ]
    ~debuggers: [
      Manifest.debugger "cobol"
        ~label:"GnuCOBOL Debugger"
        ~languages: [ "cobol" ]
        ~program: "gdb"
        (* TODO unsupported ???
           "args": [
           "--init-eval-command=\"source /usr/local/bin/cobcd.py\""
           ],
        *)
        ~configurationAttributes:
          (Manifest.any
             {| {
          "launch": {
            "type": "cobol",
            "required": [
              "program"
            ],
            "properties": {
              "program": {
                "type": "string",
                "default": "${workspaceFolder}/a.out"
              }
            }
          }
}
         |}
          )
        ~configurationSnippets:
          [
            Manifest.any
              {|
{
 "label": "Debug COBOL",
 "description": "New COBOL debugging configuration",
 "body": {
   "type": "cobol",
   "request": "launch",
   "name": "${2:Launch Program}",
   "program": "${workspaceFolder}/${1:Program}"
  }
}
          |}
          ]
    ]
    ~breakpoints: [ Manifest.breakpoint "COBOL" ]
    ~configuration:
      ( Manifest.configuration ~title:"SuperBOL COBOL"
          [
            Manifest.PROPERTY.bool
              "superbol.globalFormatTakesSelection"
              ~default:false
              ~description:
                "If something is selected, only format the selection" ;

            Manifest.PROPERTY.string "superbol.path"
              ~default:""
              ~description:
                "Name of the `superbol-free` executable if available in PATH; \
                 may be an absolute path otherwise. Leave empty to use the \
                 bundled `superbol-free`, if available."
          ] )
    ~taskDefinitions: [
      Manifest.taskDefinition
        "superbol"
        ~properties: [
          Manifest.PROPERTY.array "copybooks"
            ~description:"The list of copybooks paths" ;

          Manifest.PROPERTY.string "sourceFormat"
            ~description: "The source format of the code" ;

          Manifest.PROPERTY.enum "dialect"
            ~cases:Cobol_config.DIALECT.all_canonical_names
            ~description: "The COBOL dialect used" ;

          Manifest.PROPERTY.bool "forDebugging"
            ~description: "Build for debugging" ;

          Manifest.PROPERTY.array "extensions"
            ~description: "Add cobol file extensions"
        ]
    ]
    ~problemPatterns:[
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
        ~fileLocation:["absolute"]
        ~pattern:[Manifest.ProblemName "$gnucobol"]
        ~source:"GnuCOBOL";
      Manifest.problemMatcher ()
        ~name:"gnucobol-warning"
        ~owner:"cobol"
        ~fileLocation:["absolute"]
        ~pattern:[Manifest.ProblemName "$gnucobol-warning"]
        ~severity:"warning"
        ~source:"GnuCOBOL";
      Manifest.problemMatcher ()
        ~name:"gnucobol-error"
        ~owner:"cobol"
        ~fileLocation:["absolute"]
        ~pattern:[Manifest.ProblemName "$gnucobol-error"]
        ~severity:"error"
        ~source:"GnuCOBOL";
      Manifest.problemMatcher ()
        ~name:"gnucobol-note"
        ~owner:"cobol"
        ~fileLocation:["absolute"]
        ~pattern:[Manifest.ProblemName "$gnucobol-note"]
        ~severity:"info"
        ~source:"GnuCOBOL";
    ]
    ~commands:[
      Manifest.command ()
        ~command:"superbol.server.restart"
        ~title:"Restart Language Server"
        ~category:"superbol"
    ]

let manifest =
  Manifest.vscode
    package
    ~marketplace
    ~engines: ( "^" ^ vscode_engine )
    ~activationEvents: [
      "onLanguage:cobol" ;
      "onDebug"
    ]
    ~contributes
