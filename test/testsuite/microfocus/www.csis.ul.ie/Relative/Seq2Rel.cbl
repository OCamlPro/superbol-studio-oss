      $ SET SOURCEFORMAT"FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID.  Seq2Rel.
AUTHOR.  MICHAEL COUGHLAN.
* Creates a Relative file from a sequential file.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT SupplierFile ASSIGN TO "RELSUPP.DAT"
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS RANDOM
           RELATIVE KEY IS SupplierKey
           FILE STATUS IS Supplierstatus.

    SELECT SupplierFileSeq ASSIGN TO "SEQSUPP.DAT"
	  	   ORGANIZATION IS LINE SEQUENTIAL.
        

DATA DIVISION.
FILE SECTION.

FD  SupplierFile.
01  SupplierRecord.
    02 SupplierCode           PIC 99.
    02 SupplierName           PIC X(20).
    02 SupplierAddress        PIC X(50).


FD  SupplierFileSeq.
01  SupplierRecordSeq.
    88 EndOfFile              VALUE HIGH-VALUES.
    02 SupplierCodeSeq        PIC 99.
    02 SupplierNameSeq        PIC X(20).
    02 SupplierAddressSeq     PIC X(50).


WORKING-STORAGE SECTION.
01  SupplierStatus            PIC X(2).

01  SupplierKey               PIC 99.


PROCEDURE DIVISION.
Begin.
    OPEN OUTPUT SupplierFile.
    OPEN INPUT SupplierFileSeq.
    
    READ SupplierFileSeq 
        AT END SET EndOfFile TO TRUE
    END-READ
    PERFORM UNTIL EndOfFile
       MOVE SupplierCodeSeq TO SupplierKey
       MOVE SupplierRecordSeq TO SupplierRecord
       WRITE SupplierRecord
          INVALID KEY DISPLAY "Supplier status = " SupplierStatus
       END-WRITE
       READ SupplierFileSeq 
            AT END SET EndOfFile TO TRUE
       END-READ
    END-PERFORM.    

    CLOSE  SupplierFile, SupplierFileSeq.
    STOP RUN.
