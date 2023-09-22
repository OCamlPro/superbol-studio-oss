      $ SET SOURCEFORMAT "FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID.  SeqReadIdx.
AUTHOR.  Michael Coughlan.
* Demonstrates how to read an Indexed file sequentially
* on any of its keys

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
   SELECT VideoFile ASSIGN TO "IDXVIDEO.DAT"
          ORGANIZATION IS INDEXED
          ACCESS MODE IS DYNAMIC
          RECORD KEY IS VideoCode
          ALTERNATE RECORD KEY IS VideoTitle
               WITH DUPLICATES
          FILE STATUS IS VideoStatus.

      

DATA DIVISION.
FILE SECTION.
FD VideoFile.
01 VideoRecord.
   88 EndOfFile VALUE HIGH-VALUE.
   02 VideoCode               PIC 9(5).
   02 VideoTitle              PIC X(40).
   02 SupplierCode            PIC 99.

WORKING-STORAGE SECTION.
01   VideoStatus              PIC X(2).

01   RequiredSequence         PIC 9.
     88 VideoCodeSequence     VALUE 1.
     88 VideoTitleSequence    VALUE 2.
           
01 PrnVideoRecord.
   02 PrnVideoCode            PIC 9(5).
   02 PrnVideoTitle           PIC BBBBX(40).
   02 PrnSupplierCode         PIC BBBB99.
 
PROCEDURE DIVISION.
Begin.
   OPEN INPUT VideoFile.

   DISPLAY "Enter key : 1=VideoCode, 2=VideoTitle ->"
      WITH NO ADVANCING.
   ACCEPT RequiredSequence.

*  First we must establish the key-of-reference (KOR).
*  Since the default KOR is the primary key we don't need
*  to do anything special to establish the VideoCode as the KOR.
*  But to read the file in VideoTitle order we must establish
*  the VideoTile as the KOR.  We do this by using the VideoTitle
*  in a direct READ or (as in this case) a START statement.
   IF VideoTitleSequence
      MOVE SPACES TO VideoTitle
      START VideoFile KEY IS GREATER THAN VideoTitle
         INVALID KEY  DISPLAY "VIDEO STATUS :- ", VideoStatus
      END-START
   END-IF   


*  The READ..NEXT RECORD will read the file sequentially
*  as if it ordered on whichever key has been 
*  established as the KOR.
   READ VideoFile NEXT RECORD 
      AT END SET EndOfFile TO TRUE
   END-READ.
   PERFORM UNTIL EndOfFile
      MOVE VideoCode TO PrnVideoCode
      MOVE VideoTitle TO PrnVideoTitle
      MOVE SupplierCode TO PrnSupplierCode
      DISPLAY  PrnVideoRecord
      READ VideoFile NEXT RECORD 
         AT END SET EndOfFile TO TRUE
      END-READ
   END-PERFORM.

   CLOSE VideoFile.
   STOP RUN.

