       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL022A. 
       
       
       ENVIRONMENT DIVISION. 
       
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-AT. 
       OBJECT-COMPUTER. IBM-AT. 
       
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       
       DATA DIVISION.  

       FILE SECTION.
      
       WORKING-STORAGE SECTION. 
       
           01 BFLD SQL TYPE IS BINARY(100).
           01 VBFLD SQL TYPE IS VARBINARY(100).
           
           01 CFLD SQL TYPE IS CHAR(100).

           01 VCFLD PIC X(100) VARYING.      
       
       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 

       PROCEDURE DIVISION. 
 
       000-CONNECT.


       100-MAIN.

 

       200-END.
