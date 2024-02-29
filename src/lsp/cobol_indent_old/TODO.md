TODO:
  - Implement continued literal over multiple lines
  - Bug of tokenizer. Words joined by a "." are not well identified. e.x. According to the lexer, "PROGRAM-ID. HELLO" are 3 TextWords, but "PROGRAM-ID.HELLO." is a single TextWord (which causes error of indenter). (Both are legal in GnuCOBOL)
  - Not satisfied with the `Cobol_preproc.fold_text_lines`, this function has an argument which is the name of file, so when using lsp, every time using the formatting, we must save the file first, it is not convenient.
  - Use a new type (other than `context_kind`) to represent the `scope`
  - Rewrite the `indent_config` (like ocp-indent, we may let the client to decide if it activates some features like alignment of argument)
  - PERFORM statement has two formats, inline or out-of-line. Not easy to distinguish these two formats without parsing. Now, we regard only "PERFORM {word} TIMES ..." "PERFORM WITH ..." "PERFORM TEST ..." "PERFORM UNTIL ..." "PERFORM VARYING ..." as inline format.
    There is a bug, since the `ident` of `"PERFORM ident TIMES ..."` can contain more than 1 word like "PERFORM X (3) TIMES ...".
  - Maybe do the preprocessing to link some successive keywords. ex. "ON" "SIZE" "ERROR" -> "ON_SIZE_ERROR" (the code will be more brief if we do this)
  - Only support standard ISO/IEC 1989:2014
  - Careful check for method/interface/factory/function.... I have not verified if there is difference (of indentation) between these compilation unit.
  - Add check of clause in ENV DIVISION if need be
  - Refine the check of phrase/clause of PROCEDURE/DATA DIVISION
  - Find a better way to solve the keyword conflict "(ON)EXCEPTION"
  - The NEXT SENTENCE phrase inside IF/SEARCH statement, it seems that the GnuCOBOL implements it differently than described in the standard of COBOL, however, I am not sure since I cannot test it in mainframe.(In GnuCOBOL, it allows `IF (conditon) [THEN] NEXT SENTENCE {nested-statement}.` But according to the standard, the THEN branche must be `NEXT SENTENCE` or `nested-statement` but cannot contain both)
