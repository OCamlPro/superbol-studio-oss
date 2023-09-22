      $ SET SOURCEFORMAT"FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID.  ReadRelative.
AUTHOR.  MICHAEL COUGHLAN.
* Reads a Relative file directly or in sequence

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
SELECT SupplierFile ASSIGN TO "RELSUPP.DAT"
       ORGANIZATION IS RELATIVE
       ACCESS MODE IS DYNAMIC
       RELATIVE KEY IS SupplierKey
       FILE STATUS IS Supplierstatus.
       
DATA DIVISION.
FILE SECTION.
FD SupplierFile.
01  SupplierRecord.
    88 EndOfFile  VALUE HIGH-VALUES.
    02 SupplierCode            PIC 99.
    02 SupplierName            PIC X(20).
    02 SupplierAddress         PIC X(50).


WORKING-STORAGE SECTION.
01  SupplierStatus             PIC X(2).
    88 RecordFound             VALUE "00".

01  SupplierKey                PIC 99.

01  PrnSupplierRecord.
    02    PrnSupplierCode      PIC BB99.
    02    PrnSupplierName      PIC BBX(20).
    02    PrnSupplierAddress   PIC BBX(50).

01  ReadType                   PIC 9.
    88 DirectRead              VALUE 1.
    88 SequentialRead          VALUE 2.


PROCEDURE DIVISION.
BEGIN.
    OPEN INPUT SupplierFile.
    DISPLAY "Read type : Direct read = 1, Sequential read = 2 --> "
             WITH NO ADVANCING.
    ACCEPT ReadType.

    IF DirectRead
        DISPLAY "Enter supplier code key (2 digits) --> "
                    WITH NO ADVANCING
        ACCEPT SupplierKey
        READ SupplierFile
            INVALID KEY DISPLAY "SUPP STATUS :-", SupplierStatus
        END-READ
        PERFORM DisplayRecord
    END-IF

    IF SequentialRead
        READ SupplierFile NEXT RECORD
            AT END SET EndOfFile TO TRUE
        END-READ
        PERFORM UNTIL EndOfFile
            PERFORM DisplayRecord
            READ SupplierFile NEXT RECORD
                AT END SET EndOfFile TO TRUE
            END-READ
        END-PERFORM
    END-IF

    CLOSE SupplierFile.
    STOP RUN.

DisplayRecord.
    IF RecordFound
        MOVE SupplierCode TO PrnSupplierCode
        MOVE SupplierName TO PrnSupplierName
        MOVE SupplierAddress TO PrnSupplierAddress
        DISPLAY PrnSupplierRecord
    END-IF.
                 
