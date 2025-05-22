       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL039A. 
       
       
       ENVIRONMENT DIVISION. 
       
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-AT. 
       OBJECT-COMPUTER. IBM-AT. 
       
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       
       DATA DIVISION.  
       
       FILE SECTION.  
       
       WORKING-STORAGE SECTION. 
       
           01 DATASRC PIC X(64).
           01 DBUSR   PIC X(64).
           01 DBPWD   PIC X(64).
           
       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 
       
       PROCEDURE DIVISION. 
 
            EXEC SQL
                SELECT 1;
            END-EXEC.

            EXEC SQL
                DISCONNECT ALL ;
            END-EXEC.
            
            EXEC SQL
                ROLLBACK;
            END-EXEC.

       100-EXIT. 
             STOP RUN.