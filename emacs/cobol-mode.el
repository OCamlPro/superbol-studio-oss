;;; cobol-mode.el --- Mode for editing COBOL code -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2022  Free Software Foundation, Inc.

;; Author: Edward Hart <edward.dan.hart@gmail.com>
;; Maintainer: Edward Hart <edward.dan.hart@gmail.com>
;; Version: 1.1
;; Created: 9 November 2013
;; Keywords: languages
;; Package-Requires: ((cl-lib "0.5"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file should not be confused with Rick Bielawski's cobol-mode.el
;; (http://www.emacswiki.org/emacs/cobol-mode.el), which this mode attempts to
;; supersede.

;; This COBOL mode features syntax highlighting for most modern COBOL dialects,
;; indentation, code skeletons, rulers and basic formatting functions.
;; Highlighting changes with the code format, which can be specified using the
;; M-x customize menu.

;;;; Installation:

;; To install cobol-mode.el, save it to your .emacs.d/ directory and add the
;; following to your .emacs:
;; (autoload 'cobol-mode "cobol-mode" "Major mode for highlighting COBOL files." t nil)

;; To automatically load cobol-mode.el upon opening COBOL files, add this:
;; (setq auto-mode-alist
;;       (append
;;        '(("\\.cob\\'" . cobol-mode)
;;          ("\\.cbl\\'" . cobol-mode)
;;          ("\\.cpy\\'" . cobol-mode))
;;        auto-mode-alist))

;; Finally, I strongly suggest installing auto-complete-mode, which makes typing
;; long keywords and variable names a thing of the past.  See
;; https://github.com/auto-complete/auto-complete.

;;;; Known bugs:

;; * Switching source formats requires M-x customize settings to be changed,
;;   saved and cobol-mode to be unloaded then reloaded.
;; * Copying-and-pasting content in fixed-format sometimes results in content
;;   being pasted in column 1 and spaces inserted in the middle of it.
;; * The indentation code leaves a lot of trailing whitespace.
;; * Periods on their own line are sometimes indented strangely.
;; * String continuation does not work.

;;;; Missing features:

;; * Switch between dialect's reserved word lists via M-x customize (without
;;   unloading cobol-mode).
;; * Allow users to modify easily reserved word lists.
;; * Expand copybooks within a buffer.
;; * String continuation (see above).
;; * Allow users to modify start of program-name area.

;;; News:

;; - A new submenu for skeletons.
;; - cobol-mode is now orphaned :-(
;;   We're looking for a generous soul willing to give it loving care.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup cobol nil
  "Major mode for editing COBOL code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix 'cobol-
  :group 'languages)

(eval-and-compile
(defun cobol--radio-of-list (list)
  "Return radio with the elements of LIST as its arguments."
  (cons 'radio (mapcar #'(lambda (elt) (list 'const elt)) list)))

(defun cobol--val-in-list-p (list)
  "Return a predicate to check whether a value is in the LIST."
  #'(lambda (value) (memq value list))))

(defcustom cobol-declaration-clause-indent 40
  "Column to indent data division declaration clauses to."
  :type 'integer
  :safe #'integerp)

(eval-and-compile
(defconst cobol-formats
  '(fixed-85 fixed-2002 free)
  "The accepted values for `cobol-source-format'.")

(defcustom cobol-source-format 'fixed-85
  "Source format of COBOL source code."
  :type (cobol--radio-of-list cobol-formats)
  :safe (cobol--val-in-list-p cobol-formats)))

;; Ruler
;; Code derived from the Emacs fortran.el, rulers from IBM Rational Developer.

(defcustom cobol-fixed-85-ruler
  "----+-*A-1-B--+----2----+----3----+----4----+----5----+----6----+----7--|-+----\n"
  "Ruler for COBOL-85-style fixed format code."
  :type  'string
  :safe  #'stringp)

(defcustom cobol-fixed-2002-ruler
  "----+-*--1----+----2----+----3----+----4----+----5----+----6----+----7----+----\n"
  "Ruler for COBOL-2002-style fixed format code."
  :type  'string
  :safe  #'stringp)

(defcustom cobol-free-ruler
  "----+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----\n"
  "Ruler for free format code."
  :type  'string
  :safe  'stringp)

(defun cobol-column-ruler ()
  "Insert a column ruler above the current line until the next keystroke.
The next key typed is executed unless it is SPC."
  (interactive)
  (momentary-string-display
   (cond ((eq cobol-source-format 'fixed-85)
          cobol-fixed-85-ruler)
         ((eq cobol-source-format 'fixed-2002)
          cobol-fixed-2002-ruler)
         ((eq cobol-source-format 'free)
          cobol-free-ruler))
   (save-excursion
     (beginning-of-line)
     (if (eq (window-start (selected-window))
             (window-point (selected-window)))
         (line-beginning-position 2)
       (point)))
   nil "Type SPC or any command to erase the ruler."))

(defcustom cobol-mode-hook nil
  "Hook run by `cobol-mode'."
  :type 'hook)

(defun cobol--remove-strings (l1 l2)
  "Return a list of strings in L1 not in L2."
  (cl-set-difference l1 l2 :test #'string-equal))

(defconst cobol-directives
  '("CALL-CONVENTION"
    "D"
    "DEFINE"
    "ELIF"
    "ELSE"
    "END-EVALUATE"
    "END-IF"
    "EVALUATE"
    "FLAG-02"
    "FLAG-85"
    "FLAG-NATIVE-ARITHMETIC"
    "IF"
    "IMP"
    "LEAP-SECOND"
    "LISTING"
    "PAGE"
    "PROPAGATE"
    "SOURCE"
    "TURN"
    "WHEN")
  "List of COBOL compiler directives.")

(defconst cobol-verbs-74
  '("GO TO"
    "NEXT SENTENCE"
    "ACCEPT"
    "ADD"
    "ALTER"
    "CALL"
    "CANCEL"
    "CLOSE"
    "COPY"
    "COMPUTE"
    "DELETE"
    "DISABLE"
    "DISPLAY"
    "DIVIDE"
    "ENABLE"
    "ENTER"
    "EXIT"
    "GENERATE"
    "GO"
    "IF"
    "INITIATE"
    "INSPECT"
    "MERGE"
    "MOVE"
    "MULTIPLY"
    "OPEN"
    "PERFORM"
    "READ"
    "RECEIVE"
    "RELEASE"
    "RESET"
    "RETURN"
    "REWRITE"
    "SEARCH"
    "SELECT"
    "SEND"
    "SET"
    "SORT"
    "START"
    "STOP"
    "STRING"
    "SUBTRACT"
    "SUPPRESS"
    "TERMINATE"
    "UNSTRING"
    "USE"
    "WRITE"))

(defconst cobol-verbs-85
  (append cobol-verbs-74
          '("CONTINUE"
            "EVALUATE"
            "INITIALIZE"
            "REPLACE")))

(defconst cobol-removed-verbs-2002
  '("ALTER" "ENTER"))

(defconst cobol-verbs-2002
  (append (cobol--remove-strings cobol-verbs-85 cobol-removed-verbs-2002)
          '("ALLOCATE"
            "FREE"
            "GOBACK"
            "INVOKE"
            "RAISE"
            "RESUME"
            "UNLOCK"
            "VALIDATE")))

(defconst cobol-removed-verbs-2014
  '("DISABLE"
    "ENABLE"
    "SEND"
    "RECEIVE"))

(defconst cobol-verbs-2014
  (cobol--remove-strings cobol-verbs-2002
                         cobol-removed-verbs-2014))

(defconst cobol-verbs-extensions
  '("DELETE FILE"
    "READY TRACE"
    "RESET TRACE"
    "XML GENERATE"
    "XML PARSE"
    "ATTACH"
    "ALLOW" ; Unisys COBOL-74
    "AWAIT-OPEN" ; Unisys COBOL-74
    "CAUSE" ; Unisys COBOL-74
    "CHAIN"
    "CHANGE" ; Unisys COBOL-74
    "COLOR"
    "COMMIT"
    "CREATE"
    "DECLARE"
    "DELEGATE"
    "DETACH"
    "DISALLOW" ; Unisys COBOL-74
    "ENTRY"
    "EXAMINE"
    "EXEC"
    "EXECUTE"
    "EXHIBIT"
    ;; "LOCK" ; Unisys COBOL-74 ; Treated as keyword
    "MODIFY"
    "NOTE"
    "ON" ; OS/VS Statement ; Keyword <= COBOL-74
    "PROCESS" ; Unisys COBOL-74
    "RESPOND" ; Unisys COBOL-74
    "ROLLBACK"
    ;; "RUN" ; Unisys COBOL-74 ; Treated as keyword
    "SEEK" ; Unisys COBOL-74
    "SERVICE"
    "SYNC" ; <= COBOL-74
    "TRANSFORM"
    "TRY"
    "WAIT"))

(defconst cobol-verbs
  (append cobol-verbs-2014
          cobol-removed-verbs-2002
          cobol-removed-verbs-2014
          cobol-verbs-extensions)
  "List of COBOL verb keywords.")

(defconst cobol-scope-terminators-85
  '("END-ADD"
    "END-CALL"
    "END-COMPUTE"
    "END-DELETE"
    "END-DIVIDE"
    "END-EVALUATE"
    "END-IF"
    "END-MULTIPLY"
    "END-PERFORM"
    "END-READ"
    "END-RECEIVE"
    "END-RETURN"
    "END-REWRITE"
    "END-SEARCH"
    "END-START"
    "END-STRING"
    "END-SUBTRACT"
    "END-UNSTRING"
    "END-WRITE"))

(defconst cobol-scope-terminators-2002
  (append cobol-scope-terminators-85
          '("END-ACCEPT"
            "END-DISPLAY")))

(defconst cobol-removed-scope-terminators-2014
  '("END-RECEIVE"))

(defconst cobol-scope-terminators-2014
  (cobol--remove-strings cobol-scope-terminators-2002
                         cobol-removed-scope-terminators-2014))

(defconst cobol-scope-terminators-xml-tr
  '("END-OPEN"))

(defconst cobol-scope-terminators-extensions
  '("END-CHAIN"
    "END-COLOR"
    "END-DELEGATE"
    "END-EXEC"
    "END-INVOKE"
    "END-MODIFY"
    "END-MOVE"
    "END-SYNC"
    "END-TRY"
    "END-WAIT"
    "END-XML"))

(defconst cobol-scope-terminators
  (append cobol-scope-terminators-2014
          cobol-scope-terminators-xml-tr
          cobol-scope-terminators-extensions))

(defconst cobol-keywords-74
  '("ACCESS"
    "ADVANCING"
    "AFTER"
    "ALL"
    "ALPHABETIC"
    "ALSO"
    "ALTERNATE"
    "AND"
    "ARE"
    "AREA"
    "AREAS"
    "ASCENDING"
    "ASSIGN"
    "AT"
    "AUTHOR"
    "BEFORE"
    "BLANK"
    "BLOCK"
    "BOTTOM"
    "BY"
    "CD"
    "CF"
    "CH"
    "CHARACTER"
    "CHARACTERS"
    "CLOCK-UNITS"
    "COBOL"
    "CODE"
    "CODE-SET"
    "COLLATING"
    "COLUMN"
    "COMMA"
    "COMMUNICATION"
    "COMP"
    "COMPUTATIONAL"
    "CONFIGURATION"
    "CONTAINS"
    "CONTROL"
    "CONTROLS"
    "CORR"
    "CORRESPONDING"
    "COUNT"
    "CURRENCY"
    "DATA"
    "DATE"
    "DATE-COMPILED"
    "DATE-WRITTEN"
    "DAY"
    "DE"
    "DEBUG-CONTENTS"
    "DEBUG-ITEM"
    "DEBUG-LINE"
    "DEBUG-NAME"
    "DEBUG-SUB-1"
    "DEBUG-SUB-2"
    "DEBUG-SUB-3"
    "DEBUGGING"
    "DECIMAL-POINT"
    "DECLARATIVES"
    "ENVIRONMENT"
    "DELIMITED"
    "DELIMITER"
    "DEPENDING"
    "DESCENDING"
    "DESTINATION"
    "DETAIL"
    "DIVISION"
    "DOWN"
    "DUPLICATES"
    "DYNAMIC"
    "EGI"
    "ELSE"
    "EMI"
    "END"
    "END-OF-PAGE"
    "EOP"
    "EQUAL"
    "ERROR"
    "ESI"
    "EVERY"
    "EXCEPTION"
    "EXTEND"
    "FD"
    "FILE"
    "FILE-CONTROL"
    "FILLER"
    "FINAL"
    "FIRST"
    "FOOTING"
    "FOR"
    "FROM"
    "GIVING"
    "GREATER"
    "GROUP"
    "HEADING"
    "I-O"
    "I-O-CONTROL"
    "IDENTIFICATION"
    "INDEX"
    "INDEXED"
    "IN"
    "INDICATE"
    "INITIAL"
    "INPUT"
    "INPUT-OUTPUT"
    "INSTALLATION"
    "INTO"
    "INVALID"
    "IS"
    "JUST"
    "JUSTIFIED"
    "KEY"
    "LABEL"
    "LAST"
    "LEADING"
    "LEFT"
    ; LENGTH is treated as an intrinsic function.
    "LESS"
    "LIMIT"
    "LIMITS"
    "LINAGE"
    "LINAGE-COUNTER"
    "LINE"
    "LINE-COUNTER"
    "LINES"
    "LINKAGE"
    "LOCK"
    "NEXT"
    "MEMORY"
    "MESSAGE"
    "MODE"
    "MODULES"
    "MULTIPLE"
    "NATIVE"
    "NEGATIVE"
    "NO"
    "NOT"
    "NUMBER"
    "NUMERIC"
    "OBJECT-COMPUTER"
    "OCCURS"
    "OF"
    "OFF"
    "OMITTED"
    ;; "ON" ; OS/VS Statement ; Keyword <= COBOL-74
    "OPTIONAL"
    "OR"
    "ORGANIZATION"
    "OUTPUT"
    "OVERFLOW"
    "PAGE"
    "PAGE-COUNTER"
    "PF"
    "PH"
    "PIC"
    "PICTURE"
    "PLUS"
    "POINTER"
    "POSITION"
    "POSITIVE"
    "PRINTING"
    "PROCEDURE"
    "PROCEDURES"
    "PROCEED"
    "PROGRAM"
    "PROGRAM-ID"
    "QUEUE"
    "RANDOM"
    "RD"
    "RECORD"
    "RECORDS"
    "REDEFINES"
    "REEL"
    "REFERENCES"
    "RELATIVE"
    "REMAINDER"
    "REMOVAL"
    "RENAMES"
    "REPLACING"
    "REPORT"
    "REPORTING"
    "REPORT"
    "RERUN"
    "RESERVE"
    "REVERSED"
    "REWIND"
    "RF"
    "RH"
    "RIGHT"
    "ROUNDED"
    "RUN"
    "SAME"
    "SD"
    "SECTION"
    "SECURITY"
    "SEGMENT"
    "SEGMENT-LIMIT"
    "SEND"
    "SENTENCE"
    "SEPARATE"
    "SEQUENCE"
    "SEQUENTIAL"
    "SIZE"
    "SORT-MERGE"
    "SOURCE"
    "SOURCE-COMPUTER"
    "SPECIAL-NAMES"
    "STANDARD"
    "STANDARD-1"
    "STATUS"
    "SUB-QUEUE-1"
    "SUB-QUEUE-2"
    "SUB-QUEUE-3"
    "SYMBOLIC"
    "SYNCHRONIZED"
    "TABLE"
    "TALLYING"
    "TAPE"
    "TERMINAL"
    "TEXT"
    "THAN"
    "THROUGH"
    "THRU"
    "TIME"
    "TIMES"
    "TO"
    "TOP"
    "TRAILING"
    "TYPE"
    "UNIT"
    "UNTIL"
    "UP"
    "UPON"
    "USAGE"
    "USING"
    "VALUE"
    "VALUES"
    "VARYING"
    "WHEN"
    "WITH"
    "WORDS"
    "WORKING-STORAGE"))

(defconst cobol-keywords-85
  (append cobol-keywords-74
          '("ALPHABET"
            "ALPHABETIC-LOWER"
            "ALPHABETIC-UPPER"
            "ALPHANUMERIC"
            "ALPHANUMERIC-EDITED"
            "ANY"
            "BINARY"
            "CLASS"
            "COMMON"
            "CONTENT"
            "CONVERTING"
            "DAY-OF-WEEK"
            "EXTERNAL"
            "FALSE"
            "FUNCTION"
            "GLOBAL"
            "NUMERIC-EDITED"
            "ORDER"
            "OTHER"
            "PACKED-DECIMAL"
            "PADDING"
            "PURGE"
            "REFERENCE"
            "STANDARD-2"
            "TEST"
            "THEN"
            "TRUE")))

(defconst cobol-removed-keywords-2002
  '("AUTHOR"
    "INSTALLATION"
    "DATE-WRITTEN"
    "DATE-COMPILED"
    "SECURITY"
    "MEMORY"
    "RERUN"
    "MULTIPLE"
    "TAPE"
    "LABEL"
    "REVERSED"
    "DEBUG-CONTENTS"
    "DEBUG-ITEM"
    "DEBUG-LINE"
    "DEBUG-NAME"
    "DEBUG-SUB-1"
    "DEBUG-SUB-2"
    "DEBUG-SUB-3"))

(defconst cobol-keywords-2002
  (append (cobol--remove-strings cobol-keywords-85
                                 cobol-removed-keywords-2002)
          '("ACTIVE-CLASS"
            "ADDRESS"
            "ALIGNED"
            "ANYCASE"
            "AS"
            "B-AND"
            "B-NOT"
            "B-OR"
            "B-XOR"
            "BASED"
            "BINARY-CHAR"
            "BINARY-DOUBLE"
            "BINARY-LONG"
            "BINARY-SHORT"
            "BIT"
            "BOOLEAN"
            "CLASS-ID"
            "COL"
            "COLS"
            "COLUMNS"
            "CONDITION"
            "CONSTANT"
            "CRT"
            "CURSOR"
            "DATA-POINTER"
            "DEFAULT"
            "EC"
            "EO"
            "EXCEPTION-OBJECT"
            "FACTORY"
            "FLOAT-EXTENDED"
            "FLOAT-LONG"
            "FLOAT-SHORT"
            "FORMAT"
            "FUNCTION-ID"
            "GET"
            "GROUP-USAGE"
            "INHERITS"
            "INTERFACE"
            "INTERFACE-ID"
            "LAST"
            "LOCAL-STORAGE"
            "LOCALE"
            "METHOD"
            "METHOD-ID"
            "MINUS"
            "NATIONAL"
            "NATIONAL-EDITED"
            "NESTED"
            "OBJECT"
            "OBJECT-REFERENCE"
            "OPTIONS"
            "OVERRIDE"
            "PRESENT"
            "PROGRAM-POINTER"
            "PROPERTY"
            "PROTOTYPE"
            "RAISING"
            "REPOSITORY"
            "RETRY"
            "RETURNING"
            "SCREEN"
            "SHARING"
            "SOURCES"
            "SYSTEM-DEFAULT"
            "TYPEDEF"
            "UNIVERSAL"
            "USER-DEFAULT"
            "VAL-STATUS"
            "VALID"
            "VALIDATE-STATUS")))

(defconst cobol-keywords-finalizer-tr
  '("AUTO-METHOD"))

(defconst cobol-keywords-xml-tr
  '("DOCUMENT"
    "IDENTIFIED"
    "VERSION-XML"))

(defconst cobol-removed-keywords-2014
  '("CD"
    "DEBUGGING"
    "EGI"
    "EMI"
    "ESI"
    "MESSAGE"
    "PADDING"
    "PURGE"
    "QUEUE"
    "SEGMENT"
    "SUB-QUEUE-1"
    "SUB-QUEUE-2"
    "SUB-QUEUE-3"
    "TERMINAL"
    "TEXT"))

(defconst cobol-keywords-2014
  (append (cobol--remove-strings cobol-keywords-2002
                                 cobol-removed-keywords-2014)
          '("FARTHEST-FROM-ZERO"
            "FLOAT-BINARY-32"
            "FLOAT-BINARY-64"
            "FLOAT-BINARY-128"
            "FLOAT-DECIMAL-16"
            "FLOAT-DECIMAL-34"
            "FLOAT-INFINITY"
            "FLOAT-NOT-A-NUMBER"
            "FLOAT-NOT-A-NUMBER-QUIET"
            "FLOAT-NOT-A-NUMBER-SIGNALING"
            "FUNCTION-POINTER"
            "IN-ARITHMETIC-RANGE"
            "NEAREST-TO-ZERO")))

(defconst cobol-keywords-extensions
  '("3-D"
    "ABSENT"
    "ABSTRACT"
    "ACQUIRE"
    "ACTION"
    "ACTIVE-X"
    "ACTUAL"
    "ACCEPT-CLOSE"
    "ACCEPT-OPEN"
    "ADDRESS-ARRAY"
    "ADDRESS-OFFSET"
    "ADJUSTABLE-COLUMNS"
    "AFP-5A"
    "ALIGNMENT"
    "ALLOWING"
    "ANY LENGTH"
    "APPLY"
    "ARGUMENT-NUMBER"
    "ARGUMENT-VALUE"
    "ASSEMBLY-ATTRIBUTES"
    "ASSOCIATED-DATA" ; Unisys
    "ASSOCIATED-DATA-LENGTH" ; Unisys
    "AUTO-DECIMAL"
    "AUTO-HYPHEN-SKIP"
    "AUTO-MINIMIZE"
    "AUTO-RESIZE"
    "AUTO-SKIP"
    "AUTO-SPIN"
    "AUTOTERMINATE"
    "AX-EVENT-LIST"
    "B-EXOR"
    "B-LEFT"
    "B-RIGHT"
    "BACKGROUND-COLOUR"
    "BACKGROUND-HIGH"
    "BACKGROUND-LOW"
    "BACKGROUND-STANDARD"
    "BACKWARD"
    "BAR"
    "BASIS"
    "BEEP"
    "BEGINNING"
    "BINARY-INT"
    "BINARY-LONG-LONG"
    "BIND"
    "BITMAP"
    "BITMAP-END"
    "BITMAP-HANDLE"
    "BITMAP-NUMBER"
    "BITMAP-RAW-HEIGHT"
    "BITMAP-RAW-WIDTH"
    "BITMAP-SCALE"
    "BITMAP-START"
    "BITMAP-TIMER"
    "BITMAP-TRAILING"
    "BITMAP-WIDTH"
    "BLINKING"
    "BLOB"
    "BLOB-FILE"
    "BLOB-LOCATOR"
    "BOLD"
    "BOX"
    "BOXED"
    "BROWSING"
    "BULK-ADDITION"
    "BUSY"
    "BUTTONS"
    "C01"
    "C02"
    "C03"
    "C04"
    "C05"
    "C06"
    "C07"
    "C08"
    "C09"
    "C10"
    "C11"
    "C12"
    "CALENDAR-FONT"
    "CALLED"
    "CANCEL-BUTTON"
    "CATCH"
    "CBL"
    "CBL-CTR"
    "CCOL"
    "CELL"
    "CELL-COLOR"
    "CELL-DATA"
    "CELL-FONT"
    "CELL-PROTECTION"
    "CELLS"
    "CENTERED"
    "CENTERED-HEADINGS"
    "CENTURY-DATE"
    "CENTURY-DAY"
    "CHAINING"
    "CHANGED"
    "CHAR-VARYING"
    "CHART"
    "CHECK-BOX"
    "CHECKING"
    "CLASS-ATTRIBUTES"
    "CLASS-CONTROL"
    "CLASS-OBJECT"
    "CLEAR-SELECTION"
    "CLINE"
    "CLINES"
    "CLOB"
    "CLOB-FILE"
    "CLOB-LOCATOR"
    "CLOSE-DISPOSITION" ; Unisys
    "CMP" ; Unisys
    "COERCION"
    "COLORS"
    "COLOUR"
    "COLOURS"
    "COLUMN-COLOR"
    "COLUMN-DIVIDERS"
    "COLUMN-FONT"
    "COLUMN-HEADINGS"
    "COLUMN-PROTECTION"
    "COM-REG"
    "COMBO-BOX"
    "COMMAND-LINE"
    "COMMITMENT"
    "COMP-0"
    "COMP-1"
    "COMP-2"
    "COMP-3"
    "COMP-4"
    "COMP-5"
    "COMP-6"
    "COMP-N"
    "COMP-X"
    "COMPRESSION"
    "COMPUTATIONAL-0"
    "COMPUTATIONAL-1"
    "COMPUTATIONAL-2"
    "COMPUTATIONAL-3"
    "COMPUTATIONAL-4"
    "COMPUTATIONAL-5"
    "COMPUTATIONAL-6"
    "COMPUTATIONAL-N"
    "COMPUTATIONAL-X"
    "CONDITION-VALUE"
    "CONNECT-TIME-LIMIT" ; Unisys
    "CONSOLE"
    "CONSTRAIN"
    "CONSTRAINTS"
    "CONTROL-AREA"
    "CONTROL-POINT" ; Unisys
    "CONTROLS-UNCROPPED"
    "CONVENTION"
    "CONVERSION"
    "CONVERT"
    "COPY-SELECTION"
    "CORE-INDEX"
    "CP" ; Unisys
    "CREATING"
    "CRT-UNDER"
    "CRUNCH" ; Unisys
    "CSIZE"
    "CSP"
    "CURSOR-COL"
    "CURSOR-COLOR"
    "CURSOR-FRAME-WIDTH"
    "CURSOR-ROW"
    "CURSOR-X"
    "CURSOR-Y"
    "CUSTOM-ATTRIBUTE"
    "CUSTOM-PRINT-TEMPLATE"
    "CYL-INDEX"
    "CYL-OVERFLOW"
    "DASHED"
    "DATA-COLUMNS"
    "DATA-TYPES"
    "DATABASE-KEY"
    "DATABASE-KEY-LONG"
    "DATE-ENTRY"
    "DATE-RECORD"
    "DBCLOB"
    "DBCLOB-FILE"
    "DBCLOB-LOCATOR"
    "DBCS"
    "DEBUG"
    "DECIMAL"
    "DEFAULT-BUTTON"
    "DEFAULT-FONT"
    "DEFINITION"
    "DELEGATE-ID"
    "DESTROY"
    "DICTIONARY"
    "DISC"
    "DISJOINING"
    "DISK"
    "DISP"
    "DISPLAY-1"
    "DISPLAY-COLUMNS"
    "DISPLAY-FORMAT"
    "DISPLAY-ST"
    "DIVIDER-COLOR"
    "DIVIDERS"
    "DONT-PARTICIPATE" ; Unisys
    "DOT-DASH"
    "DOTTED"
    "DOUBLE"
    "DRAG-COLOR"
    "DRAW"
    "DROP"
    "DROP-DOWN"
    "DROP-LIST"
    "EBCDIC"
    "ECHO"
    "EGCS"
    "EJECT"
    "ELEMENTARY"
    "EMPTY-CHECK"
    "ENABLED"
    "ENCRYPTION"
    "ENDING"
    "ENGRAVED"
    "ENSURE-VISIBLE"
    "ENTRY-FIELD"
    "ENTRY-REASON"
    "ENUM"
    "ENUM-ID"
    "ENVIRONMENT-NAME"
    "ENVIRONMENT-VALUE"
    "EQUALS"
    "ESCAPE"
    "ESCAPE-BUTTON"
    "EVENT"
    "EVENT-LIST"
    "EVENT-POINTER"
    "EXCEEDS"
    "EXCEPTION-VALUE"
    "EXCESS-3"
    "EXCLUDE-EVENT-LIST"
    "EXCLUSIVE"
    "EXPAND"
    "EXTENDED"
    "EXTENDED-SEARCH"
    "EXTENSION"
    "EXTERNAL-FORM"
    "EXTERNALLY-DESCRIBED-KEY"
    "FH--FCD"
    "FH--KEYDEF"
    "FILE-ID"
    "FILE-LIMIT"
    "FILE-LIMITS"
    "FILE-NAME"
    "FILE-POS"
    "FILL-COLOR"
    "FILL-COLOR2"
    "FILL-PERCENT"
    "FINALLY"
    "FINISH-REASON"
    "FIXED"
    "FIXED-FONT"
    "FIXED-WIDTH"
    "FLAT"
    "FLAT-BUTTONS"
    "FLOAT"
    "FLOATING"
    "FONT"
    "FOREGROUND-COLOUR"
    "FRAME"
    "FRAMED"
    "FULL-HEIGHT"
    "GETTER"
    "GO-BACK"
    "GO-FORWARD"
    "GO-HOME"
    "GO-SEARCH"
    "GRAPHICAL"
    "GRID"
    "GRIP"
    "GROUP-VALUE"
    "HANDLE"
    "HAS-CHILDREN"
    "HEADING-COLOR"
    "HEADING-DIVIDER-COLOR"
    "HEADING-FONT"
    "HEAVY"
    "HEIGHT"
    "HEIGHT-IN-CELLS"
    "HELP-ID"
    "HIDDEN-DATA"
    "HIGH"
    "HIGH-COLOR"
    "HORIZONTAL"
    "HOT-TRACK"
    "HSCROLL"
    "HSCROLL-POS"
    "ICON"
    "ID"
    "IGNORE"
    "INDEPENDENT"
    "INDEXED"
    "INDEXER"
    "INDEXER-ID"
    "INDIC"
    "INDICATOR"
    "INDICATORS"
    "INDIRECT"
    "INHERITING"
    "INQUIRE"
    "INTERRUPT"
    "INSERT"
    "INSERT-ROWS"
    "INSERTION-INDEX"
    "INSTANCE"
    "INTERNAL"
    "INVOKED"
    "ITEM"
    "ITEM-BOLD"
    "ITEM-ID"
    "ITEM-TEXT"
    "ITEM-TO-ADD"
    "ITEM-TO-DELETE"
    "ITEM-TO-EMPTY"
    "ITEM-VALUE"
    "ITERATOR"
    "ITERATOR-ID"
    "JOINED"
    "JOINING"
    "KANJI"
    "KEPT"
    "KEY-YY"
    "KEYBOARD"
    "LABEL-OFFSET"
    "LARGE-FONT"
    "LAST-ROW"
    "LAYOUT-DATA"
    "LAYOUT-MANAGER"
    "LEADING-SHIFT"
    "LEAVE"
    "LEFT-JUSTIFY"
    "LEFT-TEXT"
    "LEFTLINE"
    "LENGTH-CHECK"
    "LENGTH OF"
    "LIN"
    "LINES-AT-ROOT"
    "LINK"
    "LIST"
    "LIST-BOX"
    "LM-RESIZE"
    "LANGUAGE"
    "LOCAL" ; Unisys
    "LOCKING"
    "LONG-DATE"
    "LONG-VARBINARY"
    "LONG-VARCHAR"
    "LOW"
    "LOW-COLOR"
    "LOWER"
    "LOWER-BOUND" ; Unisys
    "LOWER-BOUNDS" ; Unisys
    "LOWERED"
    "MASS-UPDATE"
    "MASTER-INDEX"
    "MAX-HEIGHT"
    "MAX-LINES"
    "MAX-PROGRESS"
    "MAX-SIZE"
    "MAX-TEXT"
    "MAX-VAL"
    "MAX-WIDTH"
    "MDI-CHILD"
    "MDI-FRAME"
    "MEDIUM-FONT"
    "MENU"
    "MESSAGES"
    "METACLASS"
    "MIN-HEIGHT"
    "MIN-LINES"
    "MIN-SIZE"
    "MIN-VAL"
    "MIN-WIDTH"
    "MMDDYYYY" ; Unisys
    "MODAL"
    "MODELESS"
    "MODIFIED"
    "MONITOR-POINTER"
    "MORE-DATA" ; Unisys
    "MORE-LABELS"
    "MULTILINE"
    "MUTEX-POINTER"
    "MYJOB" ; Unisys
    "MYSELF" ; Unisys
    "NAME"
    "NAMED"
    "NAVIGATE-URL"
    "NCHAR"
    "NET-EVENT-LIST"
    "NEW"
    "NEWABLE"
    "NEXT-ITEM"
    "NO-AUTO-DEFAULT"
    "NO-AUTOSEL"
    "NO-BOX"
    "NO-CELL-DRAG"
    "NO-CLOSE"
    "NO-DIVIDERS"
    "NO-ECHO"
    "NO-F4"
    "NO-FOCUS"
    "NO-GROUP-TAB"
    "NO-KEY-LETTER"
    "NO-SEARCH"
    "NO-TAB"
    "NO-UPDOWN"
    "NOMINAL"
    "NOTIFY"
    "NOTIFY-CHANGE"
    "NOTIFY-DBLCLICK"
    "NOTIFY-SELCHANGE"
    "NSTD-REELS"
    "NUM-COL-HEADINGS"
    "NUM-ROW-HEADINGS"
    "NUM-ROWS"
    "NUMERIC-DATE" ; Unisys
    "NUMERIC-FILL"
    "NUMERIC-TIME" ; Unisys
    "O-FILL"
    "OBJECT-ID"
    "OBJECT-STORAGE"
    "OC" ; Unisys
    "ODT-INPUT-PRESENT" ; Unisys
    "OK-BUTTON"
    "OOSTACKPTR"
    "OPERATOR"
    "OPERATOR-ID"
    "OTHERWISE"
    "OVERLAP-LEFT"
    "OVERLAP-TOP"
    "OVERLAPPED"
    "OVERLINE"
    "OWN" ; Unisys
    "PAGE-SETUP"
    "PAGE-SIZE"
    "PAGED"
    "PANEL-INDEX"
    "PANEL-STYLE"
    "PANEL-TEXT"
    "PANEL-WIDTHS"
    "PARAMS"
    "PARENT"
    "PARSE"
    "PARTIAL"
    "PARTICIPATE" ; Unisys
    "PASSWORD"
    "PC" ; Unisys
    "PERMANENT"
    "PIXEL"
    "PIXELS"
    "PLACEMENT"
    "POP-UP"
    "POSITION-SHIFT"
    "POSITIONING"
    "PREFIXING"
    "PRINT"
    "PRINT-CONTROL"
    "PRINT-NO-PROMPT"
    "PRINT-PREVIEW"
    "PRINT-SWITCH"
    "PRINTER"
    "PRINTER-1"
    "PRIOR"
    "PRIORITY"
    "PRIVATE"
    "PROCEDURE-POINTER"
    "PROCESSING"
    "PROGRESS"
    "PROMPT"
    "PROPERTIES"
    "PROPERTY-ID"
    "PROPERTY-VALUE"
    "PROTECTED"
    "PUBLIC"
    "PUSH-BUTTON"
    "QUERY-INDEX"
    "RADIO-BUTTON"
    "RAISED"
    "READ-OK" ; Unisys
    "READ-ONLY"
    "READING"
    "READY"
    "REAL" ; Unisys
    "RECORD-DATA"
    "RECORD-OVERFLOW"
    "RECORD-TO-ADD"
    "RECORD-TO-DELETE"
    "RECORDING"
    "REDEFINE"
    "REDEFINITION"
    "REF" ; Unisys
    "REFRESH"
    "REGION-COLOR"
    "REJECT-OPEN"
    "RELOAD"
    "REMARKS"
    "REORG-CRITERIA"
    "REPEATED"
    "REREAD"
    "RESET"
    "RESET-GRID"
    "RESET-LIST"
    "RESET-TABS"
    "RESIDENT"
    "RESIZABLE"
    "RESTRICTED"
    "RESULT-SET-LOCATOR"
    "RETURN-CODE"
    "RIGHT-ALIGN"
    "RIGHT-JUSTIFY"
    "RIMMED"
    "ROLLING"
    "ROW-COLOR"
    "ROW-COLOR-PATTERN"
    "ROW-DIVIDERS"
    "ROW-FONT"
    "ROW-HEADINGS"
    "ROW-PROTECTION"
    "ROWID"
    "S01"
    "S02"
    "S03"
    "S04"
    "S05"
    "SAVE-AS"
    "SAVE-AS-NO-PROMPT"
    "SCROLL"
    "SCROLL-BAR"
    "SEARCH-OPTIONS"
    "SEARCH-TEXT"
    "SELECT-ALL"
    "SELECTION-INDEX"
    "SELECTION-TEXT"
    "SELECTIVE"
    "SELF-ACT"
    "SELFCLASS"
    "SEMAPHORE-POINTER"
    "SEPARATION"
    "SETTER"
    "SHADING"
    "SHADOW"
    "SHIFT-IN"
    "SHIFT-OUT"
    "SHORT-DATE"
    "SHOW-LINES"
    "SHOW-NONE"
    "SHOW-SEL-ALWAYS"
    ;; SIGN is treated as an intrinsic function.
    "SIGNED-INT"
    "SIGNED-LONG"
    "SIGNED-SHORT"
    "SKIP1"
    "SKIP2"
    "SKIP3"
    "SMALL-FONT"
    "SORT-CONTROL"
    "SORT-CORE-SIZE"
    "SORT-FILE-SIZE"
    "SORT-MESSAGE"
    "SORT-MODE-SIZE"
    "SORT-OPTION"
    "SORT-ORDER"
    "SORT-RETURN"
    "SORT-TAPE"
    "SORT-TAPES"
    "SPACE-FILL"
    "SPINNER"
    "SQL"
    "SQUARE"
    "STANDARD-3"
    "START-X"
    "START-Y"
    "STARTING"
    "STATIC"
    "STATIC-LIST"
    "STATIONLIST"
    "STATUS-BAR"
    "STATUS-TEXT"
    "STDCALL"
    "STOP-BROWSER"
    "STYLE"
    "SUBFILE"
    "SUBWINDOW"
    "SUFFIXING"
    ;; SUM is an intrinsic function.
    "SW0"
    "SW1"
    "SW2"
    "SW3"
    "SW4"
    "SW5"
    "SW6"
    "SW7"
    "SW8"
    "SW9"
    "SW10"
    "SW11"
    "SW12"
    "SW13"
    "SW14"
    "SW15"
    "SWITCH-0"
    "SWITCH-1"
    "SWITCH-2"
    "SWITCH-3"
    "SWITCH-4"
    "SWITCH-5"
    "SWITCH-6"
    "SWITCH-7"
    "SWITCH-8"
    "SWITCH-9"
    "SWITCH-10"
    "SWITCH-11"
    "SWITCH-12"
    "SWITCH-13"
    "SWITCH-14"
    "SWITCH-15"
    "SYSIN"
    "SYSIPT"
    "SYSLST"
    "SYSOUT"
    "SYSPCH"
    "SYSPUNCH"
    "SYSTEM"
    "SYSTEM-INFO"
    "TAB"
    "TAB-CONTROL"
    "TAB-TO-ADD"
    "TAB-TO-DELETE"
    "TAG-KEY" ; Unisys
    "TAG-SEARCH" ; Unisys
    "TALLY"
    "TAPES"
    "TASK"
    "TEMPORARY"
    "TERMINAL-INFO"
    "TERMINATION-VALUE"
    "THREAD"
    "THREAD-LOCAL"
    "THREAD-LOCAL-STORAGE"
    "THREAD-POINTER"
    "THUMB-POSITION"
    "TILED-HEADINGS"
    "TIME-OF-DAY"
    "TIME-OUT"
    "TIME-RECORD"
    "TIMEOUT"
    "TIMER" ; Unisys
    "TIMESTAMP"
    "TIMESTAMP-OFFSET"
    "TIMESTAMP-OFFSET-RECORD"
    "TIMESTAMP-RECORD"
    "TITLE"
    "TITLE-BAR"
    "TITLE-POSITION"
    "TODAYS-DATE"
    "TODAYS-NAME"
    "TOOL-BAR"
    "TOTALED"
    "TOTALING"
    "TRACE"
    "TRACK-AREA"
    "TRACK-LIMIT"
    "TRACK-THUMB"
    "TRACKS"
    "TRADITIONAL-FONT"
    "TRAILING-SHIFT"
    "TRAILING-SIGN"
    "TRANSACTION"
    "TRANSPARENT"
    "TRANSPARENT-COLOR"
    "TREE-VIEW"
    "UNDERLINED"
    "UNEQUAL"
    "UNFRAMED"
    "UNITS"
    "UNSIGNED-INT"
    "UNSIGNED-LONG"
    "UNSIGNED-SHORT"
    "UNSORTED"
    "UPDATE"
    "UPPER"
    "UPSI-0"
    "UPSI-1"
    "UPSI-2"
    "UPSI-3"
    "UPSI-4"
    "UPSI-5"
    "UPSI-6"
    "UPSI-7"
    "URGENT"
    "USE-ALT"
    "USE-RETURN"
    "USE-TAB"
    "USER"
    "USER-COLORS"
    "USER-GRAY"
    "USER-WHITE"
    "VA" ; Unisys
    "VALUE-FORMAT"
    "VALUETYPE"
    "VALUETYPE-ID"
    "VARBINARY"
    "VARIABLE"
    "VARIANT"
    "VERTICAL"
    "VERY-HEAVY"
    "VIRTUAL-WIDTH"
    "VISIBLE"
    "VPADDING"
    "VSCROLL"
    "VSCROLL-BAR"
    "VSCROLL-POS"
    "VTOP"
    "WEB-BROWSER"
    "WHERE"
    "WIDTH"
    "WIDTH-IN-CELLS"
    "WINDOW"
    "WRAP"
    "WRITE-ONLY"
    "WRITE-VERIFY"
    "WRITING"
    "XML"
    "XML-CODE"
    "XML-EVENT"
    "XML-NTEXT"
    "XML-TEXT"
    "YIELDING"
    "ZERO-FILL"))

(defvar cobol-keywords
  (append cobol-keywords-2014
          cobol-removed-keywords-2002
          cobol-removed-keywords-2014
          cobol-keywords-finalizer-tr
          cobol-keywords-xml-tr
          cobol-keywords-extensions
          cobol-scope-terminators
          cobol-removed-scope-terminators-2014)
  "List of COBOL keywords.")

(defconst cobol-context-sensitive-keywords-2002
  '("ARITHMETIC"
    "ATTRIBUTE"
    "AUTO"
    "AUTOMATIC"
    "BACKGROUND-COLOR"
    "BELL"
    "BLINK"
    "BYTE-LENGTH"
    "CENTER"
    "CLASSIFICATION"
    "CYCLE"
    "EC-ALL"
    "EC-ARGUMENT"
    "EC-ARGUMENT-FUNCTION"
    "EC-ARGUMENT-IMP"
    "EC-BOUND"
    "EC-BOUND-IMP"
    "EC-BOUND-ODO"
    "EC-BOUND-OVERFLOW"
    "EC-BOUND-PTR"
    "EC-BOUND-REF-MOD"
    "EC-BOUND-SET"
    "EC-BOUND-SUBSCRIPT"
    "EC-BOUND-TABLE-LIMIT"
    "EC-DATA"
    "EC-DATA-CONVERSION"
    "EC-DATA-IMP"
    "EC-DATA-INCOMPATIBLE"
    "EC-DATA-INTEGRITY"
    "EC-DATA-PTR-NULL"
    "EC-FLOW"
    "EC-FLOW-GLOBAL-EXIT"
    "EC-FLOW-GLOBAL-GOBACK"
    "EC-FLOW-IMP"
    "EC-FLOW-RELEASE"
    "EC-FLOW-REPORT"
    "EC-FLOW-RETURN"
    "EC-FLOW-SEARCH"
    "EC-FLOW-USE"
    "EC-FUNCTION"
    "EC-FUNCTION-PTR-INVALID"
    "EC-FUNCTION-PTR-NULL"
    "EC-I-O"
    "EC-I-O-AT-END"
    "EC-I-O-EOP"
    "EC-I-O-EOP-OVERFLOW"
    "EC-I-O-FILE-SHARING"
    "EC-I-O-IMP"
    "EC-I-O-INVALID-KEY"
    "EC-I-O-LINAGE"
    "EC-I-O-LOGIC-ERROR"
    "EC-I-O-PERMANENT-ERROR"
    "EC-I-O-RECORD-OPERATION"
    "EC-IMP"
    ;; EC-IMP-suffix is matched separately by
    ;; cobol-implementor-user-exception-re.
    "EC-LOCALE"
    "EC-LOCALE-IMP"
    "EC-LOCALE-INCOMPATIBLE"
    "EC-LOCALE-INVALID"
    "EC-LOCALE-INVALID-PTR"
    "EC-LOCALE-MISSING"
    "EC-LOCALE-SIZE"
    "EC-OO"
    "EC-OO-CONFORMANCE"
    "EC-OO-EXCEPTION"
    "EC-OO-IMP"
    "EC-OO-METHOD"
    "EC-OO-NULL"
    "EC-OO-RESOURCE"
    "EC-OO-UNIVERSAL"
    "EC-ORDER"
    "EC-ORDER-IMP"
    "EC-ORDER-NOT-SUPPORTED"
    "EC-OVERFLOW"
    "EC-OVERFLOW-IMP"
    "EC-OVERFLOW-STRING"
    "EC-OVERFLOW-UNSTRING"
    "EC-PROGRAM"
    "EC-PROGRAM-ARG-MISMATCH"
    "EC-PROGRAM-ARG-OMITTED"
    "EC-PROGRAM-CANCEL-ACTIVE"
    "EC-PROGRAM-IMP"
    "EC-PROGRAM-NOT-FOUND"
    "EC-PROGRAM-PTR-NULL"
    "EC-PROGRAM-RECURSIVE-CALL"
    "EC-PROGRAM-RESOURCES"
    "EC-RAISING"
    "EC-RAISING-IMP"
    "EC-RAISING-NOT-SPECIFIED"
    "EC-RANGE"
    "EC-RANGE-IMP"
    "EC-RANGE-INDEX"
    "EC-RANGE-INSPECT-SIZE"
    "EC-RANGE-INVALID"
    "EC-RANGE-PERFORM-VARYING"
    "EC-RANGE-PTR"
    "EC-RANGE-SEARCH-INDEX"
    "EC-RANGE-SEARCH-NO-MATCH"
    "EC-REPORT"
    "EC-REPORT-ACTIVE"
    "EC-REPORT-COLUMN-OVERLAP"
    "EC-REPORT-FILE-MODE"
    "EC-REPORT-IMP"
    "EC-REPORT-INACTIVE"
    "EC-REPORT-LINE-OVERLAP"
    "EC-REPORT-NOT-TERMINATED"
    "EC-REPORT-PAGE-LIMIT"
    "EC-REPORT-PAGE-WIDTH"
    "EC-REPORT-SUM-SIZE"
    "EC-REPORT-VARYING"
    "EC-SCREEN"
    "EC-SCREEN-FIELD-OVERLAP"
    "EC-SCREEN-IMP"
    "EC-SCREEN-ITEM-TRUNCATED"
    "EC-SCREEN-LINE-NUMBER"
    "EC-SCREEN-STARTING-COLUMN"
    "EC-SIZE"
    "EC-SIZE-ADDRESS"
    "EC-SIZE-EXPONENTIATION"
    "EC-SIZE-IMP"
    "EC-SIZE-OVERFLOW"
    "EC-SIZE-TRUNCATION"
    "EC-SIZE-UNDERFLOW"
    "EC-SIZE-ZERO-DIVIDE"
    "EC-SORT-MERGE"
    "EC-SORT-MERGE-ACTIVE"
    "EC-SORT-MERGE-FILE-OPEN"
    "EC-SORT-MERGE-IMP"
    "EC-SORT-MERGE-RELEASE"
    "EC-SORT-MERGE-RETURN"
    "EC-SORT-MERGE-SEQUENCE"
    "EC-STORAGE"
    "EC-STORAGE-IMP"
    "EC-STORAGE-NOT-ALLOC"
    "EC-STORAGE-NOT-AVAIL"
    "EC-USER"
    ;; EC-USER-suffix is matched separately by
    ;; cobol-implementor-user-exception-re.
    "EC-VALIDATE"
    "EC-VALIDATE-CONTENT"
    "EC-VALIDATE-FORMAT"
    "EC-VALIDATE-IMP"
    "EC-VALIDATE-RELATION"
    "EC-VALIDATE-VARYING"
    "EOL"
    "EOS"
    "ENTRY-CONVENTION"
    "ERASE"
    "EXPANDS"
    "FOREGROUND-COLOR"
    "FOREVER"
    "FULL"
    "HIGHLIGHT"
    "IGNORING"
    "IMPLEMENTS"
    "INITIALIZED"
    "INTRINSIC"
    "LC_ALL"
    "LC_COLLATE"
    "LC_CTYPE"
    "LC_MESSAGES"
    "LC_MONETARY"
    "LC_NUMERIC"
    "LC_TIME"
    "LOWLIGHT"
    "MANUAL"
    "MULTIPLE" ; <= COBOL-74
    "NEGATIVE-INFINITY"
    "NONE"
    "NORMAL"
    "NUMBERS"
    "ONLY"
    "PARAGRAPH"
    "POSITIVE-INFINITY"
    "PREVIOUS"
    "RECURSIVE"
    "RELATION"
    "REQUIRED"
    "REVERSE-VIDEO"
    "SECONDS"
    "SECURE"
    "STATEMENT"
    "STEP"
    "STRONG"
    "SYMBOL"
    "UCS-4"
    "UNDERLINE"
    "UNSIGNED"
    "UTF-8"
    "UTF-16"
    ;; XML is treated as a reserved word per IBM implementations.
    "YYYYDDD"
    "YYYYMMDD"))

(defconst cobol-context-sensitive-keywords-finalizer-tr
  '("EC-OO-FINALIZABLE"
    "FINALIZER"))

(defconst cobol-context-sensitive-keywords-xml-tr
  '("CHECK"
    "DISCARD"
    "DOCUMENTATION"
    "DTD"
    "EC-DATA-INFINITY"
    "EC-DATA-NEGATIVE-INFINITY"
    "EC-DATA-NOT-A-NUMBER"
    "EC-XML"
    "EC-XML-CODESET"
    "EC-XML-CODESET-CONVERSION"
    "EC-XML-COUNT"
    "EC-XML-DOCUMENT-TYPE"
    "EC-XML-IMPLICIT-CLOSE"
    "EC-XML-INVALID"
    "EC-XML-NAMESPACE"
    "EC-XML-STACKED-OPEN"
    "EC-XML-RANGE"
    "ELEMENT"
    "NAMESPACE"
    "RAW"
    "SCHEMA"
    "STACK"
    "VALIDITY"))

(defconst cobol-context-sensitive-keywords-2014
  (append cobol-context-sensitive-keywords-2002
          '("AWAY-FROM-ZERO"
            "BINARY-ENCODING"
            "CAPACITY"
            "DECIMAL-ENCODING"
            "EC-FUNCTION-ARG-OMITTED"
            "EC-FUNCTION-NOT-FOUND"
            "EC-OO-ARG-OMITTED"
            "FLOAT-BINARY"
            "FLOAT-DECIMAL"
            "HIGH-ORDER-LEFT"
            "HIGH-ORDER-RIGHT"
            "INTERMEDIATE"
            "NEAREST-AWAY-FROM-ZERO"
            "NEAREST-EVEN-INTERMEDIATE"
            "NEAREST-TOWARD-ZERO"
            "PREFIXED"
            "PROHIBITED"
            "ROUNDING"
            "SHORT"
            "SIGNED"
            "STANDARD-BINARY"
            "STANDARD-DECIMAL"
            "TOWARD-GREATER"
            "TOWARD-LESSER"
            "TRUNCATION")))

(defconst cobol-context-sensitive-extensions
  '("TRUNCATED" ; Unisys
    ))

(defvar cobol-context-sensitive-keywords
  (append cobol-context-sensitive-keywords-2014
          cobol-context-sensitive-keywords-finalizer-tr
          cobol-context-sensitive-keywords-xml-tr
          cobol-context-sensitive-extensions)
  "List of context-sensitive COBOL keywords.")

(defconst cobol-intrinsics-85
  '("ACOS"
    "ANNUITY"
    "ASIN"
    "ATAN"
    ;; BYTE-LENGTH is treated as a context-sensitive word.
    "CHAR"
    "COS"
    "CONCATENATE"
    "CURRENT-DATE"
    "DATE-OF-INTEGER"
    "DAY-OF-INTEGER"
    "FACTORIAL"
    "INTEGER"
    "INTEGER-OF-DATE"
    "INTEGER-OF-DAY"
    "INTEGER-PART"
    "LENGTH"
    "LOG"
    "LOG10"
    "LOWER-CASE"
    "MAX"
    "MEAN"
    "MEDIAN"
    "MIDRANGE"
    "MIN"
    "MOD"
    "NUMVAL"
    "NUMVAL-C"
    "ORD"
    "ORD-MAX"
    "ORD-MIN"
    "PRESENT-VALUE"
    ;; RANDOM is treated as a keyword
    "RANGE"
    "REM"
    "REVERSE"
    "SIGN" ; Keyword <= COBOL-74
    "SIN"
    "SQRT"
    "STANDARD-DEVIATION"
    "SUM" ; Keyword <= COBOL-74
    "TAN"
    "UPPER-CASE"
    "VARIANCE"
    "WHEN-COMPILED"))

(defconst cobol-intrinsics-2002
  (append cobol-intrinsics-85
          '("ABS"
            "BOOLEAN-OF-INTEGER"
            "CHAR-NATIONAL"
            "DATE-TO-YYYYMMDD"
            "DAY-TO-YYYYDDD"
            "DISPLAY-OF"
            "E"
            "EXCEPTION-FILE"
            "EXCEPTION-FILE-N"
            "EXCEPTION-LOCATION"
            "EXCEPTION-LOCATION-N"
            "EXCEPTION-STATEMENT"
            "EXCEPTION-STATUS"
            "EXP"
            "EXP10"
            "FRACTION-PART"
            "HIGHEST-ALGEBRAIC"
            "INTEGER-OF-BOOLEAN"
            "LOCALE-COMPARE"
            "LOCALE-DATE"
            "LOCALE-TIME"
            "LOWEST-ALGEBRAIC"
            "NATIONAL-OF"
            "NUMVAL-F"
            "PI"
            "STANDARD-COMPARE"
            "TEST-DATE-YYYYMMDD"
            "TEST-DAY-YYYYDDD"
            "TEST-NUMVAL"
            "TEST-NUMVAL-C"
            "TEST-NUMVAL-F"
            "YEAR-TO-YYYY")))

(defconst cobol-intrinsics-2014
  (append cobol-intrinsics-2002
          '("COMBINED-DATETIME"
            "FORMATTED-CURRENT-DATE"
            "FORMATTED-DATE"
            "FORMATTED-DATETIME"
            "FORMATTED-TIME"
            "INTEGER-OF-FORMATTED-DATE"
            "LOCALE-TIME-FROM-SECONDS"
            "SECONDS-FROM-FORMATTED-TIME"
            "SECONDS-PAST-MIDNIGHT"
            "TEST-FORMATTED-DATETIME"
            "TRIM")))

(defconst cobol-intrinsics-extensions
  '("ADDR"
    "CURRENCY-SYMBOL"
    "LENGTH-AN"
    "MODULE-CALLER-ID"
    "MODULE-DATE"
    "MODULE-FORMATTED-DATE"
    "MODULE-ID"
    "MODULE-PATH"
    "MODULE-SOURCE"
    "MONETARY-DECIMAL-POINT"
    "MONETARY-THOUSANDS-SEPARATOR"
    "NUMERIC-DECIMAL-POINT"
    "NUMERIC-THOUSANDS-SEPARATOR"
    "STORED-CHAR-LENGTH"
    "SUBSTITUTE"
    "SUBSTITUTE-CASE"
    "ULENGTH"
    "UPOS"
    "USUBSTR"
    "USUPPLEMENTARY"
    "UVALID"
    "UWIDTH"))

(defvar cobol-intrinsics
  (append cobol-intrinsics-2014
          cobol-intrinsics-extensions)
  "List of COBOL standard functions.")

(defconst cobol-symbolic-literals-74
  '("HIGH-VALUE"
    "HIGH-VALUES"
    "LOW-VALUE"
    "LOW-VALUES"
    "QUOTE"
    "QUOTES"
    "SPACE"
    "SPACES"
    "ZERO"
    "ZEROES"
    "ZEROS"))

(defconst cobol-symbolic-literals-85
  cobol-symbolic-literals-74)

(defconst cobol-symbolic-literals-2002
  (append cobol-symbolic-literals-85
          '("NULL"
            "SELF"
            "SUPER")))

(defconst cobol-symbolic-literals-2014
  cobol-symbolic-literals-2002)

(defconst cobol-symbolic-literals-extensions
  '("NULLS"))

(defvar cobol-symbolic-literals
  (append cobol-symbolic-literals-2014
          cobol-symbolic-literals-extensions)
  "List of COBOL symbolic literals.")

(defface cobol-verb
  '((t (:inherit font-lock-keyword-face :weight bold)))
  "Face for COBOL verbs.")

(defface cobol-context-sensitive
  '((t (:inherit font-lock-keyword-face)))
  "Face for context-sensitive COBOL words.")

;;; Highlighting regexps

(defconst cobol--fixed-form-sequence-area-re
  "^.\\{1,6\\}"
  "Regexp matching the fixed-form sequence area.")

(eval-and-compile
(defconst cobol--complete-sequence-area-re
  "^.\\{6\\}"
  "Regexp matching a complete sequence area.")

(defconst cobol--fixed-comment-indicators
  "*/"
  "String containing COBOL fixed-form comment indicator characters.")

(defconst cobol--fixed-form-comment-re
  (concat cobol--complete-sequence-area-re
          "\\(["
          cobol--fixed-comment-indicators
          "]\\)")
  "Regexp matching a fixed-form source comment.")

(defconst cobol--continuation-or-debugging-indicator-re
  (concat cobol--complete-sequence-area-re
          "\\([d-]\\)")
  "Regexp matching a continuation or debugging line indicator.")

(defconst cobol--non-fixed-comment-indicators-re
  (concat "[^" cobol--fixed-comment-indicators "]")
  "Regexp matching non-fixed-form-comment-indicator characters.")

(defconst cobol--fixed-non-comment-sequence-area-re
  (concat cobol--complete-sequence-area-re
          cobol--non-fixed-comment-indicators-re)
  "Regexp matching the sequence area of a non-comment fixed-form line."))

(defconst cobol--fixed-non-comment-grouped-sequence-area-re
  (concat "\\(" cobol--fixed-form-sequence-area-re "\\)")
  "Regexp matching the sequence area of a non-comment fixed-form line in a
group.")

(defconst cobol--fixed-form-areas-02-re
  cobol--fixed-non-comment-grouped-sequence-area-re
  "Regexp matching the ignored fixed-forms area in COBOL 2002 for non-comment
lines.")

(defconst cobol--fixed-form-areas-85-re
  (concat cobol--fixed-non-comment-grouped-sequence-area-re
          ".\\{0,66\\}\\(.*\\)")
  "Regexp matching the ignored fixed-form areas up to COBOL-85 for non-comment
lines.")

(defconst cobol--fixed-form-wrong-indicator-re
  (concat cobol--fixed-form-sequence-area-re "\\([^-\\*/d$]\\)")
  "Regexp matching incorrect indicators in fixed-form code.")

(defconst cobol--free-form-comment-re
  "\\*>.*"
  "Regexp matching a free-form source comment.")

(eval-and-compile
(defconst cobol--optional-whitespace-re
  "[ 	]*" ; Space and tab
  "Regexp matching optional whitespace.
\\w isn't used to avoid matching newlines.")

(defconst cobol--optional-leading-whitespace-line-re
  (if (not (eq cobol-source-format 'free))
      (concat cobol--fixed-non-comment-sequence-area-re
              cobol--optional-whitespace-re)
    (concat "^" cobol--optional-whitespace-re))
  "Regexp matching a line perhaps starting with whitespace.")

(defun cobol--with-opt-whitespace-line (&rest strs)
  "Return STRS concatenated after `cobol--optional-leading-whitespace-line-re'."
  (apply #'concat cobol--optional-leading-whitespace-line-re strs)))

(defconst cobol--free-form-comment-line-re
  (cobol--with-opt-whitespace-line cobol--free-form-comment-re)
  "Regexp matching a free form comment line.")

(defconst cobol--identifier-re
  "\\s-+\\(\\w+\\)"
  "Regexp matching an identifier in a separate group preceded by whitespace.")

(defconst cobol--mf-set-directive
  (cobol--with-opt-whitespace-line "\\$SET\\s-+\\w+")
  "Regexp matching MF compiler directive with optional whitespace.")

(defconst cobol--mf-compiler-directive-re
  (if (not (eq cobol-source-format 'free))
      (concat cobol--fixed-form-sequence-area-re
              ;; FIXME: cobol--mf-set-directive starts with "^" so it can't
              ;; match after cobol--fixed-form-sequence-area-re!
              cobol--mf-set-directive)
    (concat "^" cobol--mf-set-directive))
  "Regexp matching Micro Focus compiler directives.")

(defconst cobol--standard-constant-re
  (cobol--with-opt-whitespace-line "0?1" cobol--identifier-re  "\\s-+CONSTANT")
  "Regexp matching constants declared as specified by the 2002 standard.")

(defconst cobol--mf-constant-re
  (cobol--with-opt-whitespace-line "78" cobol--identifier-re)
  "Regexp matching constants declared as specified by Micro Focus.")

(eval-and-compile
(defconst cobol--directive-indicator-re
  ">> ?"
  "Regexp matching a valid directive indicator."))

(defconst cobol--define-directive-re
  (cobol--with-opt-whitespace-line cobol--directive-indicator-re
                                  "DEFINE"
                                  cobol--identifier-re)
  "Regexp matching values defined by the pre-processor.")

(defconst cobol--descriptor-level-re
  "[FRS]D"
  "Regexp matching file/report/sort descriptor \"level numbers\".")

(defconst cobol--record-descriptor-re
  (cobol--with-opt-whitespace-line cobol--descriptor-level-re cobol--identifier-re)
  "Regexp matching file/report/sort record associations.")

(defconst cobol--typedef-definition-re
  (cobol--with-opt-whitespace-line "0?1" cobol--identifier-re ".+TYPEDEF")
  "Regexp matching typedefs.")

(defconst cobol--level-number-re
  "[[:digit:]]\\{1,2\\}"
  "Regexp matching level numbers.")

(defconst cobol--variable-declaration-re
  (cobol--with-opt-whitespace-line cobol--level-number-re cobol--identifier-re)
  "Regexp matching standard variable declarations.")

(defconst cobol--mf-declare-variable-re
  (cobol--with-opt-whitespace-line "DECLARE" cobol--identifier-re)
  "Regexp matching variable declarations using DECLARE verb used in Managed
COBOL.")

(defconst cobol--id-and-name-re
  "-ID\\.?\\s-*\\(\\w+\\)"
  "Regexp matching a construct ID and the name of the declared construct.")

(defun cobol--create-id-re (re)
  "Create an id regexp using RE."
  (cobol--with-opt-whitespace-line re cobol--id-and-name-re))

(defun cobol--create-end-marker-re (re)
  "Create an end marker regexp using RE."
  (cobol--with-opt-whitespace-line "END\\s-+" re cobol--identifier-re))

(defconst cobol--standard-function-types
  '("FUNCTION" "METHOD" "PROGRAM")
  "List containing the names of standard constructs similar to functions.")

(defconst cobol--mf-function-types
  '("ITERATOR" "OPERATOR" "PROPERTY")
  "List containing the names of constructs similar to functions created by Micro
Focus.")

(defconst cobol--function-types-re
  (regexp-opt (append cobol--standard-function-types cobol--mf-function-types))
  "Regexp matching the names of constructs similar to functions.")

(defconst cobol--function-id-name-re
  (cobol--create-id-re (remove "PROPERTY" cobol--function-types-re))
  "Regexp matching the id and name of a function or similar.")

(defconst cobol--function-end-marker-re
  (cobol--create-end-marker-re cobol--function-types-re)
  "Regexp matching the end marker of a function or similar.")

(defconst cobol--standard-type-types
  '("CLASS" "INTERFACE")
  "List containing the standard type construct names.")

(defconst cobol--mf-type-types
  '("DELEGATE" "ENUM" "INDEXER" "VALUETYPE")
  "List containing the names of type constructs added by Micro Focus.")

(defconst cobol--type-types-re
  (regexp-opt (append cobol--standard-type-types cobol--mf-type-types))
  "Regexp matching type construct names.")

(defconst cobol--type-id-name-re
  (cobol--create-id-re cobol--type-types-re)
  "Regexp matching the id and name of a type.")

(defconst cobol--type-end-marker-re
  (cobol--create-end-marker-re cobol--type-types-re)
  "Regexp matching the end marker of a type.")

(defconst cobol--mf-property-id-name-re
  (concat "PROPERTY" cobol--id-and-name-re cobol--identifier-re)
  "Regexp matching the id, name and type of a property using MF's PROPERTY-ID
syntax.")

(defconst cobol--procedure-re
  (cobol--with-opt-whitespace-line "\\(\\w+\\)\\(\\s-+SECTION\\)?\\.")
  "Regexp matching the declaration of a procedure.
Note that this matches DECLARATIVES.")

(defconst cobol--select-file-re
  (cobol--with-opt-whitespace-line
   "SELECT\\(\\s-+OPTIONAL\\)?"
   cobol--identifier-re)
  "Regexp matching the declaration of a file.")

(defconst cobol--pic-type-re
  "PIC\\(TURE\\)?\\(\\s-+IS\\)?\\s-+\\(\\([-$*+,./[:digit:]()ABENPSVXZ]\\|CR\\|DB\\)+?\\)\\(\\s-\\|\\.?
\\)"
  "Regexp matching the PICTURE clause of a variable.")

(defconst cobol--string-literal-type-re
  "\\([ZBN]X?\\|[GHLX]\\)\\(\"\\|\'\\)"
  "Regexp matching the type of a string-style literal.")

(defconst cobol--function-call-re
  "\\(\\w+\\)("
  "Regexp matching a function call.")

(defun cobol--create-specifier-type-re (types)
  "Create a specifier id regexp for the list of type names TYPES."
  (cobol--with-opt-whitespace-line
   "\\("
   (mapconcat #'identity types "\\|")
   "\\)"
   cobol--identifier-re))

(defconst cobol--repository-function-type-clause-re
  (cobol--create-specifier-type-re cobol--standard-function-types)
  "Regexp matching a REPOSITORY specifier clause for function types.")

(defconst cobol--repository-type-type-clause-re
  (cobol--create-specifier-type-re cobol--standard-type-types)
  "Regexp matching a REPOSITORY specifier clause for type types.")

(defconst cobol--mf-invoked-class-re
  (concat "TYPE" cobol--identifier-re)
  "Regexp matching a class being INVOKED.")

(defconst cobol--implementer-user-exception-re
  "EC-\\(IMP\\|USER\\)-\\w+"
  "Regexp matching an implementor- or user-defined exception condition.")

(defconst cobol--scope-terminator-re
  (cobol--with-opt-whitespace-line (regexp-opt cobol-scope-terminators 'words))
  "Regexp matching a scope terminator.")

(defconst cobol--phrases-with-double-indent-after
  "\\(IF\\|EVALUATE\\|WHEN\\|ELSE\\|PERFORM\\s-+\\(VARYING\\|UNTIL\\|\\(WITH\\s-+\\)?TEST\\|.+?\\s-+TIMES\\)\\)"
  "Regexp matching phrases whose conditions/clauses are indented twice.")

(defconst cobol--containing-statement-or-phrase-re
  (cobol--with-opt-whitespace-line
   "\\("
   cobol--phrases-with-double-indent-after
   "\\|\\(NOT\\s-+\\)?\\(\\(AT\\s-+\\)?END\\(-OF-PAGE\\)?\\>\\|\\(ON\\s-+\\)?\\(OVERFLOW\\|EXCEPTION\\|ESCAPE\\|SIZE\\s-+ERROR\\)\\|INVALID\\s-+KEY\\)\\)")
  "Regexp matching statements/phrases that contain nested statements.")

(defconst cobol--verb-re
  (cobol--with-opt-whitespace-line (regexp-opt cobol-verbs 'words))
  "Regexp matching a verb.")

(defconst cobol--non-id-groups
  ;; AUTO-METHOD is part of the Finalizer TR.
  '("AUTO-METHOD" "DECLARATIVES" "FACTORY" "OBJECT" "METHOD")
  "Groups which do not take a (specifiable) ID.")

(defconst cobol--non-id-group-end-marker-re
  (cobol--with-opt-whitespace-line
   "END\\s-+" (regexp-opt cobol--non-id-groups 'words))
  "Regexp matching the end marker of the groups not taking IDs.")

(defconst cobol--end-marker-re
  (concat "\\(" cobol--function-end-marker-re
          "\\|" cobol--type-end-marker-re
          "\\|" cobol--non-id-group-end-marker-re
          "\\)")
  "Regexp matching an end marker.")

(defconst cobol--division-re
  (cobol--with-opt-whitespace-line "\\(IDENTIFICATION\\|ENVIRONMENT\\|DATA\\|PROCEDURE\\)\\s-+DIVISION")
  "Regexp matching division header.")

(defconst cobol--procedure-division-re
  (cobol--with-opt-whitespace-line "PROCEDURE\\s-+DIVISION")
  "Regexp matching the procedure division header.")

(defconst cobol--env-or-data-div-sections-re
  (cobol--with-opt-whitespace-line
   (regexp-opt '("CONFIGURATION" "INPUT-OUTPUT" "FILE" "WORKING-STORAGE" "LOCAL-STORAGE" "LINKAGE" "REPORT" "SCREEN"))
   "\\s-+SECTION.")
  "Regexp matching the sections of the environment and data divisions.")

(defconst cobol--generic-declaration-re
  (cobol--with-opt-whitespace-line
   "\\("
   cobol--descriptor-level-re
   "\\|"
   cobol--level-number-re
   "\\)"
   cobol--identifier-re)
  "Regexp matching any declaration.")

(defconst cobol--blank-line-re
  (cobol--with-opt-whitespace-line "\\.?$")
  "Regexp matching a blank line with optional period.")

;;; Font lock

(defun cobol--fixed-format-p ()
  "Return whether the current source format is fixed."
  (memq cobol-source-format '(fixed-85 'fixed-2002)))

;; This is required for indentation to function, because the initial sequence
;; area is marked as a comment, not whitespace.
(defun cobol-back-to-indentation ()
  "Move point to the first non-whitespace character on this line.
If in fixed-form code, the sequence area and indicators are skipped.
Code copied from the Emacs source."
  (interactive "^")
  (beginning-of-line 1)
  (when (cobol--fixed-format-p)
    (forward-char 7))
  (skip-syntax-forward " " (line-end-position))
  ;; Move back over chars that have whitespace syntax but have the p flag.
  (backward-prefix-chars))

(defun cobol--font-lock-sequence-area (end)
  "Mark text in the sequence area as comments from point up to END."
  (when (cobol--fixed-format-p)
    (while (and (< (point) end)
                (re-search-forward "^.\\{1,6\\}" end t))
      (put-text-property (match-beginning 0) (point)
                         'face font-lock-comment-face)))
  nil)

(eval-when-compile
  (defconst cobol--syntax-propertize-indicator-area
    (syntax-propertize-precompile-rules
     (cobol--fixed-form-comment-re (1 "<"))
     (cobol--continuation-or-debugging-indicator-re (1 ".")))
    "Syntax rules to mark fixed-form comments as comments.")

  (defconst cobol--syntax-propertize-program-name-area
    (syntax-propertize-precompile-rules
     ;; TODO: Override open strings
     ("^.\\{72\\}\\(.\\)" (1 "<")))
    "Syntax rule to mark text in the program name area as comments.")

  (defconst cobol--syntax-propertize-page-directive
    (syntax-propertize-precompile-rules
     ((cobol--with-opt-whitespace-line cobol--directive-indicator-re
                                       "PAGE\\([ 	]\\)")
      (1 "<")))
    "Syntax rule to mark text after >>PAGE as a comment.")

  (defconst cobol--syntax-propertize-adjacent-quotes
    (syntax-propertize-precompile-rules
     ("\"\"\\|''"
      (0 (ignore
          ;; Move to first quote.
          (backward-char 2)
          (if (nth 3 (syntax-ppss))
              (progn
                (put-text-property (point) (1+ (point))
                                   'syntax-table (string-to-syntax "\\"))
                ;; Move back to past the escaped quotes.
                (forward-char 2))
            ;; If the first quote began a string, then the next quote may be the
            ;; first character in another escaped quote sequence.
            (forward-char 1))))))
    "Syntax rule to mark the first of adjacent quotes.
It marks the first of \"\" or '' as an escape character."))

(defun cobol--syntax-propertize-function (beg end)
  "Syntax propertize awkward COBOL features (fixed-form comments, indicators
and ignored areas) between points BEG and END."
  ;; TO-DO: Propertize continuation lines.
  (funcall
   (pcase cobol-source-format
     (`fixed-85 (syntax-propertize-rules
                 cobol--syntax-propertize-indicator-area
                 cobol--syntax-propertize-program-name-area
                 cobol--syntax-propertize-page-directive
                 cobol--syntax-propertize-adjacent-quotes))
     (`fixed-2002 (syntax-propertize-rules
                   cobol--syntax-propertize-indicator-area
                   cobol--syntax-propertize-page-directive
                   cobol--syntax-propertize-adjacent-quotes))
     (_ (syntax-propertize-rules
         cobol--syntax-propertize-page-directive
         cobol--syntax-propertize-adjacent-quotes)))
   beg end))

;; Change to defconst so it reloads on something?
(defvar cobol-font-lock-defaults
  `((;; Sequence area
     (cobol--font-lock-sequence-area)

     ;; Directives
     ( ,(concat cobol--directive-indicator-re
                "\\(" (regexp-opt cobol-directives) "\\>\\)")
       . font-lock-preprocessor-face)
     ( ,cobol--mf-compiler-directive-re . font-lock-preprocessor-face)

     ;; TO-DO: Highlight reserved words in directives as reserved words

     ;; Standard language features.
     ( ,(regexp-opt cobol-verbs 'words) . 'cobol-verb)
     ( ,(regexp-opt cobol-keywords 'words) . font-lock-keyword-face)
     ( ,(regexp-opt cobol-context-sensitive-keywords 'words)
       . 'cobol-context-sensitive)
     ( ,cobol--implementer-user-exception-re . 'cobol-context-sensitive)
     ( ,(regexp-opt cobol-intrinsics 'words) . font-lock-builtin-face)

     ;; Constants
     ( ,(regexp-opt cobol-symbolic-literals 'words) . font-lock-constant-face)
     ( ,cobol--standard-constant-re
       (1 'font-lock-constant-face))
     ( ,cobol--mf-constant-re
       (1 'font-lock-constant-face))
     ( ,cobol--define-directive-re
       (1 'font-lock-constant-face))

     ;; PIC Type
     ( ,cobol--pic-type-re
       (3 'font-lock-type-face))

     ;; Functions
     ( ,cobol--function-call-re
       (1 'font-lock-function-name-face))

     ;; REPOSITORY clauses
     ( ,cobol--repository-function-type-clause-re
       (2 'font-lock-function-name-face))
     ( ,cobol--repository-type-type-clause-re
       (2 'font-lock-type-face))

     ;; File declarations
     ( ,cobol--select-file-re
       (2 'font-lock-type-face))

     ;; File/Report/Sort record associations
     ( ,cobol--record-descriptor-re
       (1 'font-lock-type-face))

     ;; Typedef
     ( ,cobol--typedef-definition-re
       (1 'font-lock-type-face))

     ;; Variables
     ( ,cobol--variable-declaration-re
       (1 'font-lock-variable-name-face))
     ( ,cobol--mf-declare-variable-re
       (1 'font-lock-variable-name-face))

     ;; Construct IDs
     ( ,cobol--function-id-name-re
       (1 'font-lock-function-name-face))
     ( ,cobol--type-id-name-re
       (1 'font-lock-type-face))
     ( ,cobol--mf-property-id-name-re
       (1 'font-lock-variable-name-face)
       (2 'font-lock-type-face))

     ;; Construct end markers
     ( ,cobol--function-end-marker-re
       (1 'font-lock-function-name-face))
     ( ,cobol--type-end-marker-re
       (1 'font-lock-type-face))

     ;; Invoked classes
     ( ,cobol--mf-invoked-class-re
       (1 'font-lock-type-face))

     ;; Procedures
     ( ,cobol--procedure-re
       (1 'font-lock-function-name-face))

     ( ,cobol--string-literal-type-re
       (1 'font-lock-string-face)))
    nil
    t
    nil
    nil))

;;; Skeletons

(defvar cobol-skeleton-alist nil
  "Alist of code templates.
You can extend this alist to your heart's content.  For each additional
template NAME in the list, declare a keyboard macro or function (or
interactive command) called `cobol-skeleton-NAME'.
If `cobol-skeleton-NAME' is a function it takes no arguments and should
insert the template at point; if this is a command it may accept any
sensible interactive call arguments; keyboard macros can't take
arguments at all.")


(defmacro cobol--def-skeleton (name doc interactor &rest elements)
  (declare (indent 1) (doc-string 2))
  (let ((fsym (intern (concat "cobol-skeleton-" name)))
        (printname (replace-regexp-in-string "-" " " (upcase name))))
    `(progn
       (add-to-list 'cobol-skeleton-alist ',(cons printname fsym))
       ;; (define-abbrev sml-skel-abbrev-table ,name "" ',fsym :system t)
       (define-skeleton ,fsym
         ,doc
         ,interactor
         ,@elements))))

(defun cobol-skeleton--menu (_menu)
  (mapcar (lambda (x) (vector (car x) (cdr x) t))
	  cobol-skeleton-alist))

(cobol--def-skeleton "if-else"
  "Insert an IF - ELSE - END-IF block." nil
  > "IF " (skeleton-read "Condition: ") > \n
  > _ \n
  "ELSE" > \n
  > \n
  "END-IF" > \n)

(cobol--def-skeleton "if"
  "Insert an IF - END-IF block." nil
  > "IF " (skeleton-read "Condition: ") > \n
  > _ \n
  "END-IF" > \n)

(cobol--def-skeleton "perform-times"
  "Insert a PERFORM - TIMES - END-PERFORM block." nil
  > "PERFORM " (skeleton-read "Number: ") " TIMES" > \n
  > _ \n
  "END-PERFORM" > \n)

(cobol--def-skeleton "perform-varying"
  "Insert a PERFORM VARYING - FROM - BY - UNTIL - END-PERFORM block."
  nil
  > "PERFORM VARYING "
  (skeleton-read "Variable: ")
  " FROM "
  (skeleton-read "Start: ")
  " BY "
  (skeleton-read "Step: ")
  " UNTIL "
  (skeleton-read "Condition: ") > \n
  > _ \n
  "END-PERFORM" > \n)

(defun cobol-when-with-also (prompt num-also)
  "Create a WHEN clause skeleton with provided PROMPT and NUM-ALSO ALSOs."
  `(,prompt "WHEN " str
    ,@(let ((clauses nil))
        (dotimes (_ num-also)
          (push `(" ALSO " (skeleton-read ,prompt)) clauses))
        clauses)
    > \n > _ \n))

(defvar cobol--num-conds)

(cobol--def-skeleton "evaluate"
  "Insert an EVALUATE - END-EVALUATE block."
  "Variable/TRUE: "
  ;; This is set like so because cobol--num-conds is incremented even when no str is supplied.
  '(setf cobol--num-conds -1)
  > "EVALUATE " str ("Variable/TRUE: "
                     '(setf cobol--num-conds (1+ cobol--num-conds))
                     " ALSO " str)
  > \n
  (cobol-when-with-also "Value/Condition: " cobol--num-conds)
  "END-EVALUATE")

(cobol--def-skeleton "program"
  "Insert an empty PROGRAM."
  "Program name: "
  > "IDENTIFICATION DIVISION." > \n
  "PROGRAM-ID. " str "." > \n
  > \n
  "DATA DIVISION." > \n
  "WORKING-STORAGE SECTION." > \n
  > _ \n
  "PROCEDURE DIVISION." > \n
  > \n
  "END PROGRAM " str "." > \n)

(cobol--def-skeleton "function"
  "Insert an empty FUNCTION."
  "Function name: "
  > "IDENTIFICATION DIVISION." > \n
  "FUNCTION-ID. " str "." > \n
  > \n
  "DATA DIVISION." > \n
  "LOCAL-STORAGE SECTION." > \n
  > \n
  "LINKAGE SECTION." > \n
  > _ \n
  "PROCEDURE DIVISION RETURNING ." > \n
  > \n
  "END FUNCTION " str "." > \n)

(cobol--def-skeleton "method"
  "Insert an empty METHOD."
  "Method name: "
  > "IDENTIFICATION DIVISION." > \n
  "METHOD-ID. " str "." > \n
  > \n
  "DATA DIVISION." > \n
  "LOCAL-STORAGE SECTION." > \n
  > _ \n
  "PROCEDURE DIVISION." > \n
  > \n
  "END METHOD " str "." > \n)

(cobol--def-skeleton "class"
  "Insert an empty CLASS."
  "Class name: "
  > "IDENTIFICATION DIVISION." > \n
  "CLASS-ID. " str "." > \n
  > _ \n
  "FACTORY." > \n
  "END FACTORY." > \n
  > \n
  "OBJECT." > \n
  "END OBJECT." > \n
  "END CLASS " str "." > \n)

(cobol--def-skeleton "interface"
  "Insert an empty INTERFACE."
  "Interface name: "
  > "IDENTIFICATION DIVISION." > \n
  "INTERFACE-ID. " str "." > \n
  > _ \n
  "FACTORY." > \n
  "END FACTORY." > \n
  > \n
  "OBJECT." > \n
  "END OBJECT." > \n
  "END INTERFACE " str "." > \n)

;;; Code formatting

(defconst cobol-formats
  '(upper-case lower-case capitalised-all capitalised-verbs)
  "The different formats supported when formatting COBOL code.")

(defcustom cobol-format-style 'upper-case
  "The type of formatting used when formatting COBOL code."
  :type (cobol--radio-of-list cobol-formats)
  :safe (cobol--val-in-list-p cobol-formats))

(defun cobol-format-word (word)
  "Return WORD formatted according to `cobol-format-style'."
  (cond
   ((eql cobol-format-style 'upper-case)
    (upcase word))
   ((eql cobol-format-style 'lower-case)
    (downcase word))
   ((eql cobol-format-style 'capitalised-all)
    (capitalize word))
   ((eql cobol-format-style 'capitalised-verbs)
    (if (memq word cobol-verbs)
        (capitalize word)
      (downcase word)))))

(defun cobol-format-region (beg end)
  "Format all COBOL words between BEG and END according to
`cobol-format-style'."
  (interactive "*r")
  (cobol-format beg end))

(defun cobol-format-buffer ()
  "Format all COBOL words in the current buffer according to
`cobol-format-style'."
  (interactive "*")
  (cobol-format (point-min) (point-max)))

(defun cobol-format (beg end)
  "Format COBOL code between BEG and END according to `cobol-format-style'."
  (defconst words-to-format
    (append cobol-directives cobol-verbs cobol-keywords cobol-intrinsics
            cobol-symbolic-literals))

  (save-excursion
    (dolist (word words-to-format)
      (let ((ref-point (point-min)))
        (goto-char beg)
        (while (search-forward-regexp (concat "\\<" word "\\>") end t)
          (when (not (let ((state (parse-partial-sexp ref-point (point))))
                       (or (nth 3 state) (nth 4 state))))
            (replace-match (cobol-format-word word) t)))))))

;;; Fixed-form formatting

(defun cobol-insert-in-sequence-area (beg end text)
  "Insert, in the lines between BEG and END, TEXT in the sequence area."
  (interactive "*r\nsText: ")
  (when (> (length text) 6)
    (error "%s is longer than six characters" text))
  (save-excursion
    ;; Find rectangle to insert text in.
    (let (top-left bottom-right)
      ;; Get top left corner of rectangle.
      (goto-char beg)
      (beginning-of-line)
      (when (< (point) beg)
        (forward-line 1))
      (setf top-left (point))
      ;; Get bottom right corner of rectangle.
      (while (and (<= (+ (point) 6) end) (not (eobp)))
        (forward-line 1))
      (forward-line -1)
      (setf bottom-right (+ (point) 6))
      (string-rectangle top-left bottom-right (format "%-6s" text)))))

;;; Indentation
;; Derived (a long time ago) from the wonderful Emacs Mode Tutorial at
;; <http://www.emacswiki.org/emacs/ModeTutorial>.

(defun cobol--code-start ()
  "Return the first column code can go in."
  (if (eq cobol-source-format 'free)
      0
    7))

;;; Misc
(defvar cobol-tab-width 4 "Width of a tab for `cobol-mode'.")

(cl-defun cobol--indent (indent &optional (times 1))
  "Increment INDENT."
  (+ indent (* times cobol-tab-width)))

(defun cobol--current-indentation ()
  "Return the indentation of the current line or -1 if the line is within the
sequence area."
  (let ((indent 
	(save-excursion
	  (goto-char (line-beginning-position))
	  (skip-syntax-forward " " (line-end-position))
	  (- (point) (+ (line-beginning-position) (cobol--code-start)))
	  )))
    indent
    ))

(defun cobol--indent-current ()
  "Return the current indent level indented once."
  (cobol--indent (cobol--current-indentation)))

(defun cobol--search-back (fn)
  "Go back a line at a time, calling FN each time.
If the car of the return value is non-nil, return the cdr."
  (save-excursion
    (cl-do ((ret nil (funcall fn)))
        ((car ret) (cdr ret))
      (forward-line -1))))

(cl-defun cobol--search-back-for-indent (str &key with-whitespace)
  "Return the indent of the previous line starting with the regexp STR (optionally
after whitespace if WITH-WHITESPACE). If that cannot be found, return 0."
  (let ((line-re (concat (when with-whitespace cobol--optional-whitespace-re)
                         str)))
    (cobol--search-back
     #'(lambda () (cond ((bobp)
                         (cons t 0))
                        ((looking-at line-re)
                         (cons t (cobol--current-indentation))))))))

(defun cobol--indent-of-last-div ()
  "Return the indent of the last division."
  (cobol--search-back-for-indent cobol--division-re))

(defun cobol--indent-of-last-div-or-section ()
  "Return the indent of the preceding division or section."
  (cobol--search-back-for-indent "\\w+\\s-+\\(DIVISION\\|SECTION\\)\\." :with-whitespace t))

(defun cobol--indent-of-end-marker-match (group)
  "Return the indent of the start of GROUP."
  (if (memq (upcase group) cobol--non-id-groups)
      (cobol--search-back-for-indent
       (concat group ".") :with-whitespace t)
    (cobol--search-back-for-indent
     (cobol--create-id-re group))))

(defun cobol--match-with-leading-whitespace (re str)
  "Match regexp RE (with optional leading whitespace) against STR."
  (string-match (concat cobol--optional-leading-whitespace-line-re re)
                str))

(defun cobol--match-line-with-leading-whitespace (re)
  "Match regexp RE (with optional leading whitespace) against the current line."
  (cobol--match-with-leading-whitespace re (thing-at-point 'line)))

(defun cobol--get-level-number (declaration)
  "Return the level-number of DECLARATION.
If the declaration does not have a level number, return zero."
  (string-match cobol--generic-declaration-re declaration)
  (string-to-number (match-string 1 declaration)))

(defun cobol--indent-of-group-item (wanted-level-num)
  "Return the indentation of the last item with WANTED-LEVEL-NUM or indented
from the last item of lower level."
  (cobol--search-back
   #'(lambda ()
       (cond ((looking-at cobol--generic-declaration-re)
              (let ((level-num (cobol--get-level-number (thing-at-point 'line))))
                (cond ((eq level-num wanted-level-num)
                       (cons t (cobol--current-indentation)))
                      ((< level-num wanted-level-num)
                       (cons t (cobol--indent-current))))))
             ((bobp)
              (cons t 0))))))

(defun cobol--indent-of-declaration (decl)
  "Return the indentation of the declaration DECL."
  (let ((level-num (cobol--get-level-number decl)))
    (if (or (>= 1 level-num) (eq 77 level-num) (eq 66 level-num))
        ;; If elementary item or FD/SD/RD.
        (cobol--search-back-for-indent cobol--division-re)
      ;; Find indent of item with same level or add-indent to previous item of
      ;; lower level. (This means 88 levels will always be indented to the
      ;; previous item.)
      (cobol--indent-of-group-item level-num))))

(defun cobol--indent-from-previous ()
  "Return what the indent of the current line should be based on previous
lines."
  (cobol--search-back
   #'(lambda ()
       (cond ((looking-at cobol--env-or-data-div-sections-re)
              (cons t (cobol--current-indentation)))
             ((or (looking-at cobol--containing-statement-or-phrase-re)
                  (looking-at cobol--procedure-re)
                  (looking-at cobol--procedure-division-re))
              (cons t (cobol--indent-current)))
             ((or (looking-at cobol--verb-re)
                  (looking-at cobol--scope-terminator-re)
                  (looking-at cobol--type-end-marker-re)
                  (looking-at cobol--function-end-marker-re)
                  (looking-at cobol--division-re)
                  (looking-at cobol--generic-declaration-re))
              (cons t (cobol--current-indentation)))
             ((bobp)
              (cons t 0))))))

(defun cobol--phrase-with-not (phrase)
  "Return regexp matching line with optional NOT and PHRASE."
  (cobol--with-opt-whitespace-line "\\(NOT\\s-+\\)?" phrase))

(defun cobol--at-phrase (phrase)
  "Return regexp matching PHRASE with optional AT and NOT."
  (cobol--phrase-with-not (concat "\\(AT\\s-+\\)?" phrase)))

(defun cobol--on-phrase (phrase)
  "Return regexp matching PHRASE with optional ON and NOT."
  (cobol--phrase-with-not (concat "\\(ON\\s-+\\)?" phrase)))

(defun cobol--statements-with-phrase (str)
  "Return a list of statements taking the phrase STR."
  (cond ((string-match (cobol--with-opt-whitespace-line "WHEN")
                       str)
         '("EVALUATE" "SEARCH"))
        ((string-match (cobol--at-phrase "END-OF-PAGE") str)
         '("WRITE"))
        ((string-match (cobol--at-phrase "END") str)
         ;; An AT END clause is added to OPEN in the XML TR.
         '("OPEN" "READ" "RETURN" "SEARCH"))
         ((string-match (cobol--on-phrase "OVERFLOW") str)
          '("CALL" "STRING" "UNSTRING"))
         ((string-match (cobol--on-phrase "EXCEPTION") str)
          '("ACCEPT" "CALL" "DISPLAY"))
         ((string-match (cobol--on-phrase "ESCAPE") str)
          '("ACCEPT")) ; MF/ACUCOBOL extension
         ((string-match (cobol--on-phrase "SIZE\\s-+ERROR") str)
          '("ADD" "COMPUTE" "DIVIDE" "MULTIPLY" "SUBTRACT"))
         ((string-match (cobol--phrase-with-not "INVALID\\s-+KEY") str)
          '("DELETE" "READ" "REWRITE" "START"))
         (t
          (error "Invalid phrase"))))

(defun cobol--scope-terminator-statement (scope-terminator)
  "Return the statement contained in SCOPE-TERMINATOR."
  (cobol--match-with-leading-whitespace "END-\\(\\w+\\)" scope-terminator)
  (match-string 1 scope-terminator))

(defun cobol--first-word (str)
  "Return the first word in STR."
  (cobol--match-with-leading-whitespace "\\(\\w+\\)" str)
  (match-string 1 str))

(defun cobol--go-to-open-statement (statements)
  "Go to the last open (unterminated) statement in STATEMENTS."
  (let* ((statements-re (regexp-opt statements t))
         (valid-statement-re (cobol--with-opt-whitespace-line statements-re))
         (valid-scope-terminator-re cobol--scope-terminator-re)
         found)
    (while (not found)
      (forward-line -1)
      (cond ((looking-at valid-statement-re)
             ;; Check the scope-terminator is not on the same line.
             (let ((scope-terminator
                    (concat "END-" (cobol--first-word (thing-at-point 'line)))))
               (unless (string-match scope-terminator (thing-at-point 'line))
                 (setf found t))))

            ;; Skip past terminated statements
            ((looking-at valid-scope-terminator-re)
             (let ((terminated-statement
                    (cobol--scope-terminator-statement (thing-at-point 'line))))
               (cobol--go-to-open-statement (list terminated-statement))))

            ;; If no statement is found, stop at beginning of buffer.
            ((bobp)
             (setf found t))))))

(defun cobol--indent-of-open-statement (statements)
  "Return the indent of the last open statement in STATEMENTS."
  (save-excursion
    (cobol--go-to-open-statement statements)
    (cobol--current-indentation)))

(defun cobol--indent-of-containing-statement-or-phrase (str)
  "Return the indentation of containing statement/phrase in STR."
  (let ((phrase (upcase (cobol--first-word str))))
    (cond ((or (string-equal phrase "IF")
               (string-equal phrase "EVALUATE")
               (string-equal phrase "PERFORM"))
           (cobol--indent-from-previous))

          ((string-equal phrase "ELSE")
           (cobol--indent-of-open-statement '("IF")))

          (t
           (cobol--indent (cobol--indent-of-open-statement
                           (cobol--statements-with-phrase str)))))))

(defun cobol--get-current-division ()
  "Return the division containing point as a symbol."
  (cobol--search-back
   #'(lambda ()
       (cond ((looking-at cobol--division-re)
              (string-match cobol--division-re (thing-at-point 'line))
              (let ((division (downcase (match-string 1 (thing-at-point 'line)))))
                (cons t (intern division))))

             ((or (looking-at cobol--end-marker-re)
                  (bobp))
              (cons t 'identification))))))

(defun cobol--no-instances-of-after-in-division (instance-re after-re division)
  "Actual implementation of `cobol--no-instances-of'."
  (and (eq division (cobol--get-current-division))
       (cobol--search-back
        #'(lambda ()
            (cond ((looking-at after-re)
                   (cons t t))
                  ((or (looking-at instance-re)
                       (bobp))
                   (cons t nil)))))))

(defmacro cobol--no-instances-of (re after re2 in division)
  "Return non-nil if there are no instances of things matched by RE
between point and the previous instance of RE2.
Return nil if point is not in DIVISION or if nothing is found.
Arguments must be in the form 'RE after RE2 in DIVISION' where
`after' and `in' stand for themselves."
  (cl-assert (and (eq after 'after)
                  (eq in 'in))
             nil
             "Clauses should be in the form 're AFTER re-2 IN division'.")
  `(cobol--no-instances-of-after-in-division ,re ,re2 ,division))

(defun cobol--in-file-control-p ()
  "Return whether point is in the FILE-CONTROL paragraph."
  (cobol--no-instances-of cobol--procedure-re
                         after (cobol--with-opt-whitespace-line "FILE-CONTROL.")
                         in 'environment))

(defun cobol--no-statements-after (re)
  "Return whether there are any statements between point and the previous
instance of RE."
  (cobol--no-instances-of cobol--verb-re
                         after re
                         in 'procedure))

(defun cobol--in-proc-div-param-list-p ()
  "Return whether point is in the procedure division header parameter list."
  (cobol--no-statements-after cobol--procedure-division-re))

(defun cobol--in-if-eval-when-or-perform-cond-p ()
  "Return whether point is in the condition of an IF, EVALUATE or WHEN or in
the clauses of a non-procedural PERFORM."
  (cobol--no-statements-after (cobol--with-opt-whitespace-line
                              cobol--phrases-with-double-indent-after)))

(defun cobol--indent-of-last-statement ()
  "Return the indent of the last statement."
  (cobol--search-back-for-indent cobol--verb-re))

(defun cobol--indent-of-clauses ()
  "Return the indentation for a clause at point."
  (let ((current-division (cobol--get-current-division)))
    (cond ((eq current-division 'identification)
           (cobol--indent-from-previous))

          ((eq current-division 'environment)
           (if (cobol--in-file-control-p)
               ;; Indent clauses of SELECT.
               (cobol--indent (cobol--indent-of-last-statement))
             (cobol--indent-from-previous)))

          ((eq current-division 'data)
           (- cobol-declaration-clause-indent (cobol--code-start)))

          ((eq current-division 'procedure)
           (cond ((cobol--in-proc-div-param-list-p)
                  ;; Indent procedure division parameter list twice.
                  (cobol--indent (cobol--search-back-for-indent cobol--procedure-division-re)
                                2))
                ((cobol--in-if-eval-when-or-perform-cond-p)
                 ;; Indent after IF/EVALUATE/WHEN/non-procedural PEROFRM twice.
                 (cobol--indent (cobol--search-back-for-indent
                                cobol--phrases-with-double-indent-after
                                :with-whitespace t)
                               2))
                ;; Indent once after any other statement.
                (t
                 (cobol--indent (cobol--indent-of-last-statement))))))))

(defun cobol--looking-at-comment-line ()
  "Return whether we are looking at a comment line (using `looking-at')."
  (or (looking-at cobol--free-form-comment-line-re)
      (when (cobol--fixed-format-p)
        (looking-at cobol--fixed-form-comment-re))))

(defun cobol--find-indent-of-line ()
  "Return what the indent of the current line should be."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at cobol--scope-terminator-re)
           (let ((matching-statement
                  (cobol--scope-terminator-statement (thing-at-point 'line))))
             (cobol--indent-of-open-statement (list matching-statement))))

          ((looking-at cobol--procedure-re)
           (cobol--indent-of-last-div-or-section))

          ((looking-at cobol--end-marker-re)
           (cobol--match-line-with-leading-whitespace
            (concat "END" cobol--identifier-re))
           (let ((group (match-string 1 (thing-at-point 'line))))
             (cobol--indent-of-end-marker-match group)))

          ((looking-at cobol--division-re)
           (cobol--indent-of-last-div))

          ((looking-at cobol--generic-declaration-re)
           (cobol--indent-of-declaration (thing-at-point 'line)))

          ((looking-at cobol--containing-statement-or-phrase-re)
           (cobol--indent-of-containing-statement-or-phrase
            (thing-at-point 'line)))

          ((or (cobol--looking-at-comment-line)
               (looking-at cobol--verb-re)
               (looking-at cobol--blank-line-re))
           (cobol--indent-from-previous))

          (t
           (cobol--indent-of-clauses)))))

(defun cobol--indent-point-to-col (col)
  "Indent point to COL."
  ;; FIXME: Use indent-line-to?
  (cond ((< (current-column) col)
         (indent-to col))
        ((> (current-column) col)
         (delete-char (- col (current-column))))))

(defun cobol--set-line-indent (indent)
  "Set the indent of the current line to INDENT."
  (save-excursion
    (let ((line-length (- (line-end-position) (line-beginning-position)))
          (end-of-indent (+ (cobol--code-start) indent)))
      ;; Following lines derived from source of `back-to-indentation'.
      (move-to-column (cobol--code-start))
      (if (>= line-length (cobol--code-start) (current-column))
          (progn
            (skip-syntax-forward " " (line-end-position))
            (backward-prefix-chars))
        (indent-to (cobol--code-start)))

      (cobol--indent-point-to-col end-of-indent))))

(defun cobol--indent-point ()
  "Indent point to the next multiple of `cobol-tab-width' (relative to the
start of area A, if fixed-format)."
  (cobol--indent-point-to-col
   (+ (current-column) (- cobol-tab-width
                          (% (if (cobol--fixed-format-p)
                                 (1+ (current-column))
                               (current-column))
                             cobol-tab-width)))))

(defun cobol-indent-line ()
  "Indent current line as COBOL code."
  (interactive "*")
  (let
      ((expected-indent (cobol--find-indent-of-line))
       (current-indent (cobol--current-indentation))
       )
    (indent-line-to (+ expected-indent (cobol--code-start)))
    ))

(defvar cobol-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap back-to-indentation] #'cobol-back-to-indentation)
    ;;(define-key map (kbd "RET") #'newline-and-indent)
    map))

(easy-menu-define cobol-mode-menu cobol-mode-map "Menu used for `cobol-mode'."
  '("COBOL"
    ("Insert" :filter cobol-skeleton--menu)
    ("Source Format"
     ["Fixed" cobol-fixed-format-mode]
     ["Fixed 2002" cobol-fixed-2002-format-mode]
     ["Free" cobol-free-format-mode]
     )
    ("GnuCOBOL Docs"
     ["GnuCOBOL Manual"
      (lambda () (interactive)
	(browse-url "https://get-superbol.com/gnucobol/manual"))]
     ["GnuCOBOL Programmer's Guide"
      (lambda () (interactive)
	(browse-url "https://get-superbol.com/gnucobol/gnucobpg"))]
     ["GnuCOBOL Quick Reference"
      (lambda () (interactive)
	(browse-url "https://get-superbol.com/gnucobol/gnucobqr"))]
     ["GnuCOBOL Sample Programs"
      (lambda () (interactive)
	(browse-url "https://get-superbol.com/gnucobol/gnucobsp"))]
     )
    ["SuperBOL by OCamlPro"
     (lambda () (interactive) (browse-url "https://get-superbol.com"))]
    ))

(defvar cobol-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?-  "w"   st)
    (modify-syntax-entry ?_  "w"   st)
    (modify-syntax-entry ?*  ". 1" st)
    (modify-syntax-entry ?>  "w 2" st)
    (modify-syntax-entry ?\\ "."   st)
    (modify-syntax-entry ?'  "\""  st)
    (modify-syntax-entry ?\" "\""  st)
    (modify-syntax-entry ?\n ">"   st)
    st))

(defvar ac-ignore-case)

;;;###autoload
(define-derived-mode cobol-mode prog-mode "COBOL"
  "COBOL mode is a major mode for handling COBOL files."

  (set (make-local-variable 'font-lock-defaults) cobol-font-lock-defaults)

  (when cobol-tab-width
    (set (make-local-variable 'tab-width) cobol-tab-width))

  (set (make-local-variable 'indent-tabs-mode) nil)

  (set (make-local-variable 'comment-start-skip)
       "\\(^.\\{6\\}\\*\\|\\*>\\)\\s-* *")
  (set (make-local-variable 'comment-start) "*>")
  (set (make-local-variable 'comment-end) "")

  (set (make-local-variable 'syntax-propertize-function)
       #'cobol--syntax-propertize-function)

  (set (make-local-variable 'column-number-mode) t)

  (set (make-local-variable 'indent-line-function) #'cobol-indent-line)

  ;; Auto complete mode
  (set (make-local-variable 'ac-ignore-case) t)
  )

(define-minor-mode cobol-fixed-format-mode
  "Set fixed-85 COBOL Source Format"
  :init-value nil
  :lighter " Fixed"
  :predicate (cobol-mode)
  (make-local-variable 'cobol-source-format)
  (if cobol-fixed-format-mode
      (progn
	(cobol-free-format-mode -1)
	(cobol-fixed-2002-format-mode -1)
	(setq cobol-source-format 'fixed-85)
	)
    ))

(define-minor-mode cobol-fixed-2002-format-mode
  "Set Fixed 2002 COBOL Source Format"
  :init-value nil
  :lighter " Fixed-2002"
  :predicate (cobol-mode)
  (make-local-variable 'cobol-source-format)
  (if cobol-fixed-2002-format-mode
      (progn
	(cobol-free-format-mode -1)
	(cobol-fixed-format-mode -1)
	(setq cobol-source-format 'fixed-2002)
	)
    ))

(define-minor-mode cobol-free-format-mode "Toggle Free Format"
  :init-value nil
  :lighter " Free"
  :predicate (cobol-mode)
  (make-local-variable 'cobol-source-format)
  (if cobol-free-format-mode
      (progn
	(cobol-fixed-format-mode -1)
	(cobol-fixed-2002-format-mode -1)
	(setq cobol-source-format 'free)
	)
  ))

(add-to-list 'minor-mode-alist
	     '(cobol-free-format-mode " Free"))
(add-to-list 'minor-mode-alist
	     '(cobol-fixed-format-mode " Fixed"))
(add-to-list 'minor-mode-alist
	     '(cobol-fixed-2002-format-mode " Fixed-2002"))

(provide 'cobol-mode)

;;; cobol-mode.el ends here
