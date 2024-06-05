       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL022B. 
       
       
       ENVIRONMENT DIVISION. 
       
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-AT. 
       OBJECT-COMPUTER. IBM-AT. 
       
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       
       DATA DIVISION.  

       FILE SECTION.
      
       WORKING-STORAGE SECTION. 
       
           01 BUFFER1 PIC X(100) VARYING.
           01 BUFFER2 PIC X(32767) VARYING.
           01 BUFFER3 PIC X(32768) VARYING.
           01 BUFFER4 PIC X(50000) VARYING.
       
       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 

       PROCEDURE DIVISION. 
 
       000-CONNECT.


       100-MAIN.

 

       200-END.
