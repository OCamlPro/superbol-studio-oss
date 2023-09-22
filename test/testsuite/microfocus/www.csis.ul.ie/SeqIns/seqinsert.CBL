      $ SET SOURCEFORMAT "FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID. InsertRecords.
AUTHOR. Michael Coughlan.
* This program updates the Students.Dat file with insertions
* taken from the Transins.Dat file to create a new file
* - Students.New - which contains the inserted records.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
      SELECT StudentRecords ASSIGN "STUDENTS.DAT"
             ORGANIZATION IS LINE SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL.

      SELECT TransRecords ASSIGN "TRANSINS.DAT"
             ORGANIZATION IS LINE SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL.

      SELECT NewStudentRecords ASSIGN "STUDENTS.NEW"
             ORGANIZATION IS LINE SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL.


DATA DIVISION.
FILE SECTION.
FD StudentRecords.
01 StudentRecord.
   88 EndOfStudentFile     VALUE HIGH-VALUES.
   02 StudentID            PIC X(7).
   02 FILLER               PIC X(23).

FD TransRecords.
01 TransRecord.
   88 EndOfTransFile       VALUE HIGH-VALUES.
   02 TransStudentID       PIC X(7).
   02 FILLER               PIC X(23).

FD NewStudentRecords.
01 NewStudentRecord        PIC X(30).




PROCEDURE DIVISION.
BEGIN.
    OPEN INPUT StudentRecords
    OPEN INPUT TransRecords
    OPEN OUTPUT NewStudentRecords

    READ StudentRecords
       AT END SET EndOfStudentFile TO TRUE
    END-READ

    READ TransRecords
       AT END SET EndOfTransFile TO TRUE
    END-READ

    PERFORM UNTIL (EndOfStudentFile) AND (EndOfTransFile)
       EVALUATE TRUE
         WHEN (StudentID < TransStudentID)
              WRITE NewStudentRecord FROM StudentRecord
              READ StudentRecords
                 AT END SET EndOfStudentFile TO TRUE
              END-READ

         WHEN (StudentID > TransStudentID)
              WRITE NewStudentRecord FROM TransRecord
              READ TransRecords
                  AT END SET EndOfTransFile TO TRUE
              END-READ

         WHEN (StudentID = TransStudentID)
              DISPLAY "Error - " TransStudentId " already exists in file"
              READ TransRecords
                  AT END SET EndOfTransFile TO TRUE
              END-READ
       END-EVALUATE
    END-PERFORM
    
    CLOSE StudentRecords
    CLOSE TransRecords
    CLOSE NewStudentRecords
    STOP RUN.




