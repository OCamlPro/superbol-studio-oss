      $ SET SOURCEFORMAT"FREE" 
IDENTIFICATION DIVISION. 
PROGRAM-ID.  StudentNumbersReport . 
AUTHOR. Michael Coughlan. 
 
*INPUT      The student record file Students.Dat  Records in this file 
*           are sequenced on ascending Student Number. 
*OUTPUT     Shows the number of student records in the file and the  
*           number of records for males and females. 
*PROCESSING For each record read; 
*               Adds one to the TotalStudents count 
*               IF the Gender is Male   adds one to TotalMales 
*               IF the Gender is Female adds one to TotalFemales 
*           At end of file writes the results to the report file.         

ENVIRONMENT DIVISION. 
INPUT-OUTPUT SECTION. 
FILE-CONTROL. 
    SELECT StudentFile ASSIGN TO "STUDENTS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL. 
    SELECT ReportFile ASSIGN TO "STUDENTS.RPT"
               ORGANIZATION IS LINE SEQUENTIAL. 
 
DATA DIVISION. 
FILE SECTION. 
FD  StudentFile. 
01  StudentDetails.
    88  EndOfStudentFile VALUE HIGH-VALUES.
    02  StudentId        PIC 9(7).
    02  StudentName.
        03 Surname       PIC X(8).
        03 Initials      PIC XX.
    02  DateOfBirth.
        03 YOBirth       PIC 9(4).
        03 MOBirth       PIC 9(2).
        03 DOBirth       PIC 9(2).
   02  CourseCode        PIC X(4).
   02  Gender            PIC X.
       88 Male           VALUE "M", "m". 
 
FD  ReportFile. 
01  PrintLine            PIC X(40). 
 
 
WORKING-STORAGE SECTION. 
01  HeadingLine          PIC X(21) VALUE "  Record Count Report". 
 
01  StudentTotalLine. 
    02  FILLER           PIC X(17) VALUE "Total Students = ". 
    02  PrnStudentCount  PIC Z,ZZ9. 
 
01  MaleTotalLine. 
    02  FILLER           PIC X(17) VALUE "Total Males    = ". 
    02  PrnMaleCount     PIC Z,ZZ9. 

01  FemaleTotalLine. 
    02  FILLER           PIC X(17) VALUE "Total Females  = ". 
    02  PrnFemaleCount   PIC Z,ZZ9. 
 
01  WorkTotals. 
    02  StudentCount     PIC 9(4) VALUE ZERO. 
    02  MaleCount        PIC 9(4) VALUE ZERO. 
    02  FemaleCount      PIC 9(4) VALUE ZERO. 
 
PROCEDURE DIVISION. 
Begin. 
    OPEN INPUT StudentFile 
    OPEN OUTPUT ReportFile 

    READ StudentFile 
      AT END SET EndOfStudentFile TO TRUE 
    END-READ 
    PERFORM UNTIL EndOfStudentFile 
       ADD 1 TO StudentCount 
       IF Male  ADD 1 TO MaleCount 
         ELSE   ADD 1 TO FemaleCount 
       END-IF 
       READ StudentFile 
         AT END SET EndOfStudentFile TO TRUE 
       END-READ 
    END-PERFORM 
 
    PERFORM PrintReportLines 
 
    CLOSE StudentFile, ReportFile 
    STOP RUN. 
 
 
PrintReportLines. 
    MOVE StudentCount TO PrnStudentCount 
    MOVE MaleCount    TO PrnMaleCount 
    MOVE FemaleCount  TO PrnFemaleCount 
 
    WRITE PrintLine FROM HeadingLine 
            AFTER ADVANCING PAGE     
    WRITE PrintLine FROM StudentTotalLine  
            AFTER ADVANCING 2 LINES 
    WRITE PrintLine FROM MaleTotalLine 
            AFTER ADVANCING 2 LINES 
    WRITE PrintLine FROM FemaleTotalLine 
            AFTER ADVANCING 2 LINES. 

