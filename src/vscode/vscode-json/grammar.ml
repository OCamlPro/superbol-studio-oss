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

(* see https://macromates.com/manual/en/language_grammars *)

type capture_pattern = {
  pattern_include : string ; [@key "include"]
}
[@@deriving json_encoding,show]

type capture = {
  capture_name : string option ;
  capture_patterns : capture_pattern list option ;
}
[@@deriving json_encoding,show]

type captures = (string * capture) list [@assoc]
[@@deriving json_encoding,show]

type endCaptures = captures [@@deriving show]

let endCaptures_enc =
  let open Json_encoding in
  union [
    case string
      (fun _ -> None)
      (fun s -> [ "1", { capture_name = Some s ; capture_patterns = None } ]) ;
    case captures_enc
      (fun s -> Some s)
      (fun s -> s) ;
  ]

type pattern = {
  pat_match : string option ;
  pat_include : string option ;
  pat_name : string option;
  pat_captures : captures option ;
  pat_beginCaptures : captures option ;
  pat_whileCaptures : captures option ;
  pat_endCaptures : endCaptures option ;
  pat_begin : string option ;
  pat_end : string option ;
  pat_patterns : pattern list option ;
  pat_comment : string option ;
  pat_contentName : string option ;
  pat_while : string option ;
}
[@@deriving json_encoding {recursive}, show]

type patterns = (string * pattern) list [@assoc]
[@@deriving json_encoding,show]


type grammar = {
  prefix : string Manifest.list_or_one option;
  body : string Manifest.list_or_one option ;
  fileTypes : string list option ;
  name : string option ;
  scope : string option ;
  scopeName : string option ; (* error ? *)
  patterns : pattern list option ;
  repository : patterns option ;
  description : string option ;
  copyright : string option ; [@key "_copyright"]
  schema : string option ; [@key "$schema"]
  injectionSelector : string option ;
}
[@@deriving json_encoding,show]

let schema = {|{
  "$schema": "http://json-schema.org/schema#",
  "id": "https://raw.githubusercontent.com/spgennard/vscode_cobol/main/schemas/tmlanguage.json",
  "$ref": "#/definitions/root",
  "definitions": {
    "root": {
      "allOf": [
        { "$ref": "#/definitions/grammar" },
        {
          "type": "object",
          "properties": {
            "name": { "type": "string" },
            "scopeName": {
              "description": "this should be a unique name for the grammar, following the convention of being a dot-separated name where each new (left-most) part specializes the name. Normally it would be a two-part name where the first is either text or source and the second is the name of the language or document type. But if you are specializing an existing type, you probably want to derive the name from the type you are specializing. For example Markdown is text.html.markdown and Ruby on Rails (rhtml files) is text.html.rails. The advantage of deriving it from (in this case) text.html is that everything which works in the text.html scope will also work in the text.html.«something» scope (but with a lower precedence than something specifically targeting text.html.«something»).",
              "type": "string",
              "pattern": "^(text|source)(\\.[\\w0-9-]+)+$"
            },
            "foldingStartMarker": {
              "description": "regular expressions that lines (in the document) are matched against. If a line matches one of the patterns (but not both), it becomes a folding marker (see the foldings section for more info).",
              "type": "string"
            },
            "foldingStopMarker": {
              "description": "regular expressions that lines (in the document) are matched against. If a line matches one of the patterns (but not both), it becomes a folding marker (see the foldings section for more info).",
              "type": "string"
            },
            "fileTypes": {
              "description": "this is an array of file type extensions that the grammar should (by default) be used with. This is referenced when TextMate does not know what grammar to use for a file the user opens. If however the user selects a grammar from the language pop-up in the status bar, TextMate will remember that choice.",
              "type": "array",
              "items": { "type": "string" }
            },
            "uuid": { "type": "string" },
            "firstLineMatch": { "type": "string" }
          },
          "required": [ "scopeName" ]
        }
      ]
    },
    "grammar": {
      "type": "object",
        "properties": {
          "patterns": {
            "type": "array",
            "items": { "$ref": "#/definitions/pattern" },
            "default": [ ]
          },
          "repository": {
            "description": "a dictionary (i.e. key/value pairs) of rules which can be included from other places in the grammar. The key is the name of the rule and the value is the actual rule. Further explanation (and example) follow with the description of the include rule key.",
            "type": "object",
            "additionalProperties": {
              "$ref": "#/definitions/pattern"
            }
          }
        },
        "required": [
          "patterns"
        ]
    },
    "captures": {
      "type": "object",
      "patternProperties": {
        "^[0-9]+$": {
          "type": "object",
          "properties": {
            "name": { "$ref": "#/definitions/name" },
            "patterns": {
              "type": "array",
              "items": { "$ref": "#/definitions/pattern" },
              "default": [ ]
            }
          },
          "additionalProperties": false
        }
      },
      "additionalProperties": false
    },
    "pattern": {
      "type": "object",
      "properties": {
        "comment": { "type": "string" },
        "disabled": { "type": "integer", "minimum": 0, "maximum": 1 },
        "include": {
          "description": "this allows you to reference a different language, recursively reference the grammar itself or a rule declared in this file’s repository.",
          "type": "string"
        },
        "match": {
          "description": "a regular expression which is used to identify the portion of text to which the name should be assigned. Example: '\\b(true|false)\\b'.",
          "type": "string"
        },
        "name": {
          "description": "the name which gets assigned to the portion matched. This is used for styling and scope-specific settings and actions, which means it should generally be derived from one of the standard names.",
          "$ref": "#/definitions/name"
        },
        "contentName": {
          "description": "this key is similar to the name key but only assigns the name to the text between what is matched by the begin/end patterns.",
          "$ref": "#/definitions/name"
        },
        "begin": {
          "description": "these keys allow matches which span several lines and must both be mutually exclusive with the match key. Each is a regular expression pattern. begin is the pattern that starts the block and end is the pattern which ends the block. Captures from the begin pattern can be referenced in the end pattern by using normal regular expression back-references. This is often used with here-docs. A begin/end rule can have nested patterns using the patterns key.",
          "type": "string"
        },
        "end": {
          "description": "these keys allow matches which span several lines and must both be mutually exclusive with the match key. Each is a regular expression pattern. begin is the pattern that starts the block and end is the pattern which ends the block. Captures from the begin pattern can be referenced in the end pattern by using normal regular expression back-references. This is often used with here-docs. A begin/end rule can have nested patterns using the patterns key.",
          "type": "string"
        },
        "while": {
          "description": "these keys allow matches which span several lines and must both be mutually exclusive with the match key. Each is a regular expression pattern. begin is the pattern that starts the block and while continues it.",
          "type": "string"
        },
        "captures": {
          "description": "allows you to assign attributes to the captures of the match pattern. Using the captures key for a begin/end rule is short-hand for giving both beginCaptures and endCaptures with same values.",
          "$ref": "#/definitions/captures"
        },
        "beginCaptures": {
          "description": "allows you to assign attributes to the captures of the begin pattern. Using the captures key for a begin/end rule is short-hand for giving both beginCaptures and endCaptures with same values.",
          "$ref": "#/definitions/captures"
        },
        "endCaptures": {
          "description": "allows you to assign attributes to the captures of the end pattern. Using the captures key for a begin/end rule is short-hand for giving both beginCaptures and endCaptures with same values.",
          "$ref": "#/definitions/captures"
        },
        "whileCaptures": {
          "description": "allows you to assign attributes to the captures of the while pattern. Using the captures key for a begin/while rule is short-hand for giving both beginCaptures and whileCaptures with same values.",
          "$ref": "#/definitions/captures"
        },
        "patterns": {
          "description": "applies to the region between the begin and end matches",
          "type": "array",
          "items": { "$ref": "#/definitions/pattern" },
          "default": [ ]
        },
        "applyEndPatternLast": {
          "type": "integer",
          "minimum": 0,
          "maximum": 1
        }
      },
      "dependencies": {
        "begin": { "anyOf": [ "end", "while" ] },
        "end": [ "begin" ],
        "while": [ "begin" ],
        "contentName": [ "begin", "end" ],
        "beginCaptures": { "anyOf": [[ "begin", "end" ], [ "begin", "while" ]] },
        "whileCaptures": [ "begin", "while" ],
        "endCaptures": [ "begin", "end" ],
        "applyEndPatternLast": [ "end" ]
      }
    },
    "name": {
      "anyOf": [
        {
          "type": "string",
          "pattern": "^(([\\w0-9]|\\$\\d+)+(\\.([\\w0-9-]|\\$\\d+)+)*)|(([\\w0-9\\.]*)\\s+([\\w0-9-\\.]*))$"
        },
        {
          "type": "string",
          "enum": [
            "comment",
            "comment.block",
            "comment.block.documentation",
            "comment.line",
            "comment.line.double-dash",
            "comment.line.double-slash",
            "comment.line.number-sign",
            "comment.line.percentage",
            "constant",
            "constant.character",
            "constant.character.escape",
            "constant.language",
            "constant.numeric",
            "constant.other",
            "constant.regexp",
            "constant.rgb-value",
            "constant.sha.git-rebase",
            "emphasis",
            "entity",
            "entity.name",
            "entity.name.class",
            "entity.name.function",
            "entity.name.method",
            "entity.name.section",
            "entity.name.selector",
            "entity.name.tag",
            "entity.name.type",
            "entity.other",
            "entity.other.attribute-name",
            "entity.other.inherited-class",
            "header",
            "invalid",
            "invalid.deprecated",
            "invalid.illegal",
            "keyword",
            "keyword.control",
            "keyword.control.less",
            "keyword.operator",
            "keyword.operator.new",
            "keyword.other",
            "keyword.other.unit",
            "markup",
            "markup.bold",
            "markup.changed",
            "markup.deleted",
            "markup.heading",
            "markup.inline.raw",
            "markup.inserted",
            "markup.italic",
            "markup.list",
            "markup.list.numbered",
            "markup.list.unnumbered",
            "markup.other",
            "markup.punctuation.list.beginning",
            "markup.punctuation.quote.beginning",
            "markup.quote",
            "markup.raw",
            "markup.underline",
            "markup.underline.link",
            "meta",
            "meta.cast",
            "meta.parameter.type.variable",
            "meta.preprocessor",
            "meta.preprocessor.numeric",
            "meta.preprocessor.string",
            "meta.return-type",
            "meta.selector",
            "meta.structure.dictionary.key.python",
            "meta.tag",
            "meta.type.annotation",
            "meta.type.name",
            "metatag.php",
            "storage",
            "storage.modifier",
            "storage.modifier.import.java",
            "storage.modifier.package.java",
            "storage.type",
            "storage.type.cs",
            "storage.type.java",
            "string",
            "string.html",
            "string.interpolated",
            "string.jade",
            "string.other",
            "string.quoted",
            "string.quoted.double",
            "string.quoted.other",
            "string.quoted.single",
            "string.quoted.triple",
            "string.regexp",
            "string.unquoted",
            "string.xml",
            "string.yaml",
            "strong",
            "support",
            "support.class",
            "support.constant",
            "support.function",
            "support.function.git-rebase",
            "support.other",
            "support.property-value",
            "support.type",
            "support.type.property-name",
            "support.type.property-name.css",
            "support.type.property-name.less",
            "support.type.property-name.scss",
            "support.variable",
            "variable",
            "variable.language",
            "variable.name",
            "variable.other",
            "variable.parameter"
          ]
        }
      ]
    }
  }
}
|}
