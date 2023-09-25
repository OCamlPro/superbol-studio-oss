      $ SET SOURCEFORMAT "FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID.  DirectReadIdx.
AUTHOR.  Michael Coughlan.
* Demonstrates how to read an Indexed file directly on 
* any of its keys.

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
   02 VideoCode          PIC 9(5).
   02 VideoTitle         PIC X(40).
   02 SupplierCode       PIC 99.


WORKING-STORAGE SECTION.
01 VideoStatus           PIC X(2).
   88  RecordFound       VALUE "00".

01 RequiredKey           PIC 9.
   88 VideoCodeKey      VALUE 1.
   88 VideoTitleKey     VALUE 2.
           
01 PrnVideoRecord.
   02 PrnVideoCode       PIC 9(5).
   02 PrnVideoTitle      PIC BBBBX(40).
   02 PrnSupplierCode    PIC BBBB99.
   
PROCEDURE DIVISION.
Begin.
   OPEN INPUT VideoFile.

   DISPLAY "Chose key VideoCode = 1,  VideoTitle = 2 ->  "
                     WITH NO ADVANCING.
   ACCEPT RequiredKey.

   IF VideoCodeKey
      DISPLAY "Enter Video Code (5 digits) -> " WITH NO ADVANCING
      ACCEPT VideoCode
      READ VideoFile
         KEY IS VideoCode
         INVALID KEY  DISPLAY "VIDEO STATUS :- ", VideoStatus
      END-READ
   END-IF

   IF VideoTitleKey
      DISPLAY "Enter Video Title (40 chars) -> " WITH NO ADVANCING
      ACCEPT VideoTitle
      READ VideoFile
         KEY IS VideoTitle
         INVALID KEY  DISPLAY "VIDEO STATUS :- ", VideoStatus
      END-READ
   END-IF

   IF RecordFound
      MOVE VideoCode TO PrnVideoCode
      MOVE VideoTitle TO PrnVideoTitle
      MOVE SupplierCode TO PrnSupplierCode
      DISPLAY  PrnVideoRecord
   END-IF.

   CLOSE VideoFile.
   STOP RUN.

