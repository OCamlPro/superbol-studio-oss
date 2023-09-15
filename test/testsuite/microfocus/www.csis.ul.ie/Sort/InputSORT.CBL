      $ SET SOURCEFORMAT"FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID.  InputSort.
AUTHOR.  Michael Coughlan.
* An example program using the SORT and an 
* INPUT PROCEDURE.  The program accepts records
* from the user and RELEASEs them to the work file
* where they are sorted.  This program
* allows student records to be entered in any order but
* produces a file sequenced on ascending StudentId.
 
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT StudentFile ASSIGN TO "SORTSTUD.DAT"
		ORGANIZATION IS LINE SEQUENTIAL.
    SELECT WorkFile ASSIGN TO "WORK.TMP".


DATA DIVISION.
FILE SECTION.
FD StudentFile.
01 StudentDetails      PIC X(30).

* The StudentDetails record has the description shown below.
* But in this program we don't need to refer to any of the items in 
* the record and so we have described it as PIC X(32) 
* 01 StudentDetails
*    02  StudentId       PIC 9(7).
*    02  StudentName.
*        03 Surname      PIC X(8).
*        03 Initials     PIC XX.
*    02  DateOfBirth.
*        03 YOBirth      PIC 9(4).
*        03 MOBirth      PIC 9(2).
*        03 DOBirth      PIC 9(2).
*    02  CourseCode      PIC X(4).
*    02  Gender          PIC X.


SD WorkFile.
01 WorkRec.
   02 WStudentId       PIC 9(7).
   02 FILLER           PIC X(23).


PROCEDURE DIVISION.
Begin.
   SORT WorkFile ON ASCENDING KEY WStudentId
        INPUT PROCEDURE IS GetStudentDetails
        GIVING StudentFile.
   STOP RUN.


GetStudentDetails.
    DISPLAY "Enter student details using template below."
    DISPLAY "Enter no data to end.".
    DISPLAY "Enter - StudId, Surname, Initials, YOB, MOB, DOB, Course, Gender"
    DISPLAY "NNNNNNNSSSSSSSSIIYYYYMMDDCCCCG"
    ACCEPT  WorkRec.
    PERFORM UNTIL WorkRec = SPACES
       RELEASE WorkRec
       ACCEPT WorkRec
    END-PERFORM.
