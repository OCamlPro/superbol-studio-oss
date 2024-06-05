       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL001A. 
       
       
       ENVIRONMENT DIVISION. 
       
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-AT. 
       OBJECT-COMPUTER. IBM-AT. 
       
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       
       DATA DIVISION.  
       
       FILE SECTION.  
       
       WORKING-STORAGE SECTION. 
       
       EXEC SQL 
        INCLUDE EMPREC 
       END-EXEC. 
       
           01 DATASRC PIC X(64).
           01 DBUSR  PIC X(64).
           01 DBPWD  PIC X(64).
           
           01 T1     PIC 9(3) VALUE 0.  

       EXEC SQL IGNORE

       >>DEFINE MYIDENTIFIER1 AS 1

           01 AUSGABE-FILE-NAME-T          PIC X(50) IS TYPEDEF.
           01 SOME-VERY-LONG-TYPEDEF-NAME  PIC 9999  IS TYPEDEF.
           01 AUSGABE-FILE-NAME-2T         IS TYPEDEF.
              05 FILLER    PIC 9999.
              05 DETAIL-NO USAGE SOME-VERY-LONG-TYPEDEF-NAME.
      *    
           01  MESSAGE-TEXT-2T IS TYPEDEF.
             02 AUSGABE-FILE-NAME     USAGE AUSGABE-FILE-NAME-T.
             02 FILLER REDEFINES AUSGABE-FILE-NAME.
                 05 FILLER PIC 9999.
             02 AUSGABE-FILE-NAME-2   USAGE AUSGABE-FILE-NAME-2T.
             02 FILLER                USAGE AUSGABE-FILE-NAME-T.
      *    
           01  MESSAGE-TEXT-2 EXTERNAL USAGE MESSAGE-TEXT-2T.
           
           77 OUTPUT-NAME USAGE SOME-VERY-LONG-TYPEDEF-NAME GLOBAL.
           
           01 Z-MESSAGE-T2 USAGE AUSGABE-FILE-NAME-2T.
           01 Z-MESSAGE-T3.
              49 MT3                    USAGE MESSAGE-TEXT-2T.
              49 MT3-REN  REDEFINES MT3 USAGE MESSAGE-TEXT-2T.
           
           77 CALCULUS             PIC S9(15)V9(03) IS TYPEDEF.
           01 USER-TYPE            IS TYPEDEF.
              02 AMOUNT            USAGE CALCULUS.
              02 FILLER            OCCURS 100.
                 05 GRP-AMOUNT     USAGE CALCULUS.
           01 USER-VAR             USAGE USER-TYPE.
       
       END-EXEC.

       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 
       
       PROCEDURE DIVISION. 
 
       000-CONNECT.
         DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
         ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
         DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
         ACCEPT DBUSR FROM ENVIRONMENT-VALUE.
         DISPLAY "DATASRC_PWD" UPON ENVIRONMENT-NAME.
         ACCEPT DBPWD FROM ENVIRONMENT-VALUE.

         EXEC SQL IGNORE
       >>DEFINE MYIDENTIFIER2 AS 2
         END-EXEC.

         EXEC 
            SQL 
            IGNORE
       >>DEFINE MYIDENTIFIER3 AS 3
         END-EXEC.

         EXEC SQL 
            IGNORE DISPLAY 'WHATEVER'.

         END-EXEC.	
         
         EXEC SQL   IGNORE DISPLAY 'WHATEVER 2'. END-EXEC.

         EXEC SQL IGNORE 
         
      >>IF MYIDENTIFIER1 IS DEFINED
           DISPLAY "HELLO WORLD 1"
      >>END-IF  

      >>IF MYIDENTIFIER2 IS DEFINED
           DISPLAY "HELLO WORLD 2"
      >>END-IF  

      >>IF MYIDENTIFIER3 IS DEFINED
           DISPLAY "HELLO WORLD 3"
      >>END-IF  
         
         END-EXEC.

         EXEC 
         SQL
            CONNECT TO :DATASRC USER :DBUSR USING :DBPWD
         END-EXEC.      
         
         DISPLAY 'CONNECT SQLCODE: ' SQLCODE

         IF SQLCODE <> 0 THEN
            GO TO 100-EXIT
         END-IF.

       100-MAIN.

           EXEC SQL
              START TRANSACTION
           END-EXEC.                                                    

           EXEC SQL
               SELECT COUNT(*) INTO :T1 FROM EMPTABLE
           END-EXEC. 

           DISPLAY 'SELECT SQLCODE: ' SQLCODE.
           
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.     

           DISPLAY 'RES: ' T1.    
           
           EXEC SQL CONNECT RESET END-EXEC.

       100-EXIT. 
             STOP RUN.