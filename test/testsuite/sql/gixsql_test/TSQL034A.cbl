       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL034A. 
       
       ENVIRONMENT DIVISION. 
       
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-AT. 
       OBJECT-COMPUTER. IBM-AT. 
       
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       
       DATA DIVISION.  
       
       FILE SECTION.  
       
       WORKING-STORAGE SECTION. 

           COPY 'SQLCA.cpy'.
           COPY 'EMPREC.cpy'.

       PROCEDURE DIVISION. 
 
       000-CONNECT.


       100-MAIN.


       999-END.

