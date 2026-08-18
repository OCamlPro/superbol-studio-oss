      $ SET SOURCEFORMAT"FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID.  COMPLEX_COND.
DATA DIVISION.
WORKING-STORAGE SECTION.
    01 A PIC X.
    01 B PIC X.
    01 C PIC X.
    01 D PIC X.
PROCEDURE DIVISION.
    IF A >= (1 OR B <= 2) CONTINUE.
    IF NOT A CONTINUE.
    IF (NOT A) CONTINUE.
    IF A OR B CONTINUE.
    IF A AND B CONTINUE.
    IF A AND NOT B CONTINUE.
    IF NOT A OR B CONTINUE.
    IF A AND B AND C CONTINUE.
    IF A OR B AND C CONTINUE.
    IF A = 'a' CONTINUE.
    IF A OR A AND B OR C CONTINUE.
    IF NOT (A AND B) CONTINUE.
    IF (A = 'a') CONTINUE.
    IF A = 'a' CONTINUE.
    IF A = 'a' OR (NOT B) CONTINUE.
    IF (A >= B) AND (A <= C) CONTINUE.
    IF (A >= B) AND A <= C CONTINUE.
    IF (A = 'a') AND (B = 'b') CONTINUE.
    IF A = 'a' OR B = 'b' CONTINUE.
    IF A EQUAL TO B AND C CONTINUE.
    IF A EQUAL TO B AND B EQUAL TO 1 CONTINUE.
    IF A = 1 OR 2 CONTINUE.
    IF A = 1 OR 2 OR 2 = B CONTINUE.
    IF A = 1 OR 1 + 1 OR 1 + 1 = B CONTINUE.

*> GnuCOBOL and MF do not agree on this example:
    IF (A = 'a') AND NOT B CONTINUE.
           
*> ISO COBOL2014 examples
    IF a > b AND NOT < c OR d CONTINUE.
    IF a NOT EQUAL b OR c CONTINUE.
    IF NOT a = b OR c CONTINUE.
    IF NOT (a > b OR < c) CONTINUE.
    IF NOT (a NOT > b AND c AND NOT d) CONTINUE.

*> MicroFocus OSVS: https://www.microfocus.com/documentation/reuze/60d/lhpdf60q.htm
    IF a = (1 OR 2) CONTINUE.
    IF a > b OR (c AND d) CONTINUE.
    IF a > (b OR c) AND d CONTINUE.
    IF a (= b OR > c) CONTINUE.
    IF a = b AND (> c OR < d) CONTINUE.
