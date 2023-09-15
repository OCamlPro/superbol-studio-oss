      $ SET SOURCEFORMAT "FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID. MergeFiles.
AUTHOR. MICHAEL COUGHLAN.
* Example program demonstrating the use of the MERGE.
* The program merges the file Students.Dat and 
* Transins.Dat to create a new file Students.New.
* A problem with using the MERGE for inserting records is that 
* duplicate records are not detected.
* See the example program - SeqInsert.cbl - for a more traditional 
* approach to inserting records in a Sequential File.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT StudentFile ASSIGN TO "STUDENTS.DAT"
             ORGANIZATION IS LINE SEQUENTIAL.

    SELECT InsertionsFile ASSIGN TO "TRANSINS.DAT"
             ORGANIZATION IS LINE SEQUENTIAL.

    SELECT NewStudentFile    ASSIGN TO "STUDENTS.NEW"
             ORGANIZATION IS LINE SEQUENTIAL.

    SELECT WorkFile ASSIGN TO "WORK.TMP".

DATA DIVISION.
FILE SECTION.
FD  StudentFile.
01  StudentRec             PIC X(30).

FD  InsertionsFile.
01  InsertionRec           PIC X(30).

FD  NewStudentFile.
01  NewStudentRec          PIC X(30).

SD  WorkFile.
01  WorkRec.
    02 WStudentId          PIC 9(7).
    02 FILLER              PIC X(23).

PROCEDURE DIVISION.
Begin.
    MERGE WorkFile
       ON ASCENDING KEY WStudentId
       USING InsertionsFile,  StudentFile
       GIVING NewStudentFile.
    STOP RUN.

