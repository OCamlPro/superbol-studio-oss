      $ SET SOURCEFORMAT"FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID.  UnstringFileEg.
AUTHOR.  Michael Coughlan.
* Example showing the unpacking of comma separated records
* and the size validation of the unpacked fields.
* In this example we have only implemented unpacking
* an InsertSupplier record.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT VarLengthRecFile ASSIGN TO "VarLen.DAT"
		ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD VarLengthRecFile.
01 VarLenRec.
   88  EndOfFile           VALUE HIGH-VALUES.
   02  TypeCode            PIC XX.
       88 DeleteSupplier   VALUE "1,".
       88 DeleteVideo      VALUE "2,".
       88 InsertVideo      VALUE "3,".
       88 InsertSupplier   VALUE "4,".
       88 ValidTypeCode    VALUE "1,", "2,", "3,","4,".

   02  RemainingRec        PIC X(78).


WORKING-STORAGE SECTION.
01 InsertSupplierRec.
   02 TransType            PIC 9.
   02 TransDate            PIC X(8).
   02 Supplier-Code        PIC XX.
   02 Supplier-Name        PIC X(20).
   02 Supplier-Address     PIC X(50).


* These counts allow us to detect if there are too many chars
* in a particular field.  For instance the date field should be
* 8 characters in size.
01 InsertSupplierCounts.
   02 DateCount            PIC 99.
      88 ValidDate         VALUE 8.
   02 SuppCodeCount        PIC 99.
      88 ValidSuppCode     VALUE 1 THRU 2.
   02 SuppNameCount        PIC 99.
      88 ValidSuppName     VALUE 1 THRU 20.
   02 SuppAdrCount         PIC 99.
      88 ValidSuppAdr      VALUE 1 THRU 50.

01 StringEnd               PIC 99.

PROCEDURE DIVISION.
Begin.
   OPEN INPUT VarLengthRecFile
   READ VarLengthRecFile
      AT END SET EndOfFile TO TRUE
   END-READ
   PERFORM UNTIL EndOfFile
      MOVE ZEROS TO InsertSupplierCounts
*     First find the actual length of the record
      PERFORM VARYING StringEnd FROM 78 BY -1
              UNTIL RemainingRec(StringEnd:1) NOT = SPACE
      END-PERFORM
      IF InsertSupplier
         UNSTRING RemainingRec(1:StringEnd) DELIMITED BY ","
             INTO TransDate        COUNT IN DateCount
                  Supplier-Code    COUNT IN SuppCodeCount
                  Supplier-Name    COUNT IN SuppNameCount
                  Supplier-Address COUNT IN SuppAdrCount
         END-UNSTRING
         PERFORM CheckForErrors
        ELSE
          IF NOT ValidTypeCode
             DISPLAY SPACE
             DISPLAY "Record = " VarLenRec(1:70)
             DISPLAY "Type code is not valid"
          END-IF
      END-IF
      READ VarLengthRecFile
         AT END SET EndOfFile TO TRUE
      END-READ
   END-PERFORM
   CLOSE VarLengthRecFile
   STOP RUN.

CheckForErrors.
   DISPLAY SPACE
   DISPLAY "Record = " VarLenRec(1:70)
   IF NOT ValidDate     DISPLAY "Date Size Error"        END-IF
   IF NOT ValidSuppCode DISPLAY "Supplier Code Error"    END-IF
   IF NOT ValidSuppName DISPLAY "Supplier name Error"    END-IF
   IF NOT ValidSuppAdr  DISPLAY "Supplier address Error" END-IF.
