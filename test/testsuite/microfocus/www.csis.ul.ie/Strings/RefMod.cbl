      $ SET SOURCEFORMAT"FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID.  RefModification.
AUTHOR.  Michael Coughlan.
* An example program using Reference Modification, Intrinsic Functions
* and the INSPECT.
* The program solves a number of tasks suggested by Judy Yaeger in her
* article "Character Manipulation Using COBOL" where she attempted to show
* the limitations of COBOL string handling.


DATA DIVISION.
WORKING-STORAGE SECTION.
01  xStr         PIC X(50) VALUE "   This is the first source string".
01  xStr2        PIC X(32) VALUE "This is the second source string".
01  StrSize      PIC 99 VALUE 32.
01  CharCount    PIC 99 VALUE ZEROS.
01  EndCount     PIC 99 VALUE ZEROS.
01  yStr         PIC X(4) VALUE SPACES.
01  CharPos      PIC 99 VALUE ZEROS.
01  StrLength    PIC 99 VALUE ZEROS.



PROCEDURE DIVISION.
Begin.
*   Task1 substring(xStr, StartPos, Length)
*   Extract a substring from a string given the StartPos and Length.
*   Solution - use reference modification to get the substring.
*   In this example we get 3 characters starting at position 9
    DISPLAY "Task1 = " xStr(9:3)

*   Task2 prefix(xStr,Length)
*   Extract the first Length number of chars from a string
*   Solution - use reference modification starting at position 1.
*   In this example we get the first 7 characters
    DISPLAY "Task2 = " xStr(1:7)

*   Task3 suffix(xStr,Length)
*   Extract the last Length number of chars from a string
*   Solution - use reference modification with start of substring
*   defined as the FullStringLength - SubStringLength + 1
*   In this example we get the last 13 characters.
    MOVE 13 TO StrLength
    DISPLAY "Task3 = " xStr2((StrSize - StrLength) + 1:StrLength)

*   Task4: trimblank(xStr)
*   Remove trailing blanks from a string
*   Solution 1 
*   Use the REVERSE intrinsic function to reverse the string
*   then use the INSPECT tallying to count the number of spaces at the
*   begining of the reversed string.  The substring length is then the
*   FullSringLength - CharCount.
*   Use reference modification of get the substring.
    DISPLAY "Task4 Before = " xStr "<<<<<<"
    MOVE 0 TO CharCount
    INSPECT FUNCTION REVERSE(xStr) TALLYING CharCount
        FOR LEADING SPACES
    DISPLAY "Task4 After = "xStr(1:50 - CharCount) "<<<<<<<".

*   Solution 2
*   Use reference modification and the PERFORM..VARYING to
*   keep reducing the size of the substring until a non space character
*   is encountered.  Then use reference modification to get the substring.
    PERFORM VARYING CharCount FROM 50 BY -1
          UNTIL xStr(CharCount:1) NOT = SPACE
    END-PERFORM
    DISPLAY "Task4 After = "xStr(1:CharCount) "<<<<<<<".


*   Task5 Left_trimblank(xStr)
*   Remove leading blanks from a string.
*   Solution - Use the inspect to count the leading blanks and reference
*   modification to get the substring from the point indicated by CharCount
*   and for FullStrLength - CharCount characters.
    MOVE 1 TO CharCount.
    INSPECT xStr TALLYING CharCount FOR LEADING SPACES
    DISPLAY "Task5 =" xStr(CharCount: 50 - CharCount)

*   Task6 index(xStr,yStr)
*   Find the location of the first occurrence of substring yStr in xStr.
*   Solution - Use the INSPECT..TALLYING to count the characters before
*   the first occurrence of the substring. CharCount has the location.
*   In this example we get the position of the substring "source".
    MOVE 1 TO CharCount
    INSPECT xStr TALLYING CharCount for CHARACTERS
        BEFORE INITIAL "source".
    DISPLAY "Task6 First occurrence is in char position " CharCount

*   Task7 cindex(xStr,yStr)
*   Find the location of the first occurrence of any of the characters 
*   in substring xStr, in string yStr
*   Solution - Use the PERFORM..VARYING and reference modification to
*   locate each of the characters in the yString.  Then use the INSPECT to
*   find the position of each in the xString.  Return whichever is the least.
    MOVE "fred" TO yStr
    MOVE 51 TO EndCount
    PERFORM VARYING CharPos FROM 1 BY 1 UNTIL CharPos > 4
       MOVE 1 TO CharCount
       INSPECT xStr TALLYING CharCount FOR CHARACTERS
            BEFORE INITIAL yStr(CharPos:1)
       IF CharCount < EndCount MOVE CharCount TO EndCount
       END-IF
    END-PERFORM
    DISPLAY "Task7 First occurrence is in char position " EndCount
    DISPLAY "The character is " xStr(EndCount:1)
    STOP RUN.

