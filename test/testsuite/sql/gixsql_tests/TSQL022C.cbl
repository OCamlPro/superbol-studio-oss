       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL022C. 
       
       
       ENVIRONMENT DIVISION. 
       
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-AT. 
       OBJECT-COMPUTER. IBM-AT. 
       
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       
       DATA DIVISION.  

       FILE SECTION.
      
       WORKING-STORAGE SECTION. 
       
           01 BUFFER1 PIC X(66000) VARYING.
       
       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 

       PROCEDURE DIVISION. 
 
       000-CONNECT.


       100-MAIN.

 

       200-END.
