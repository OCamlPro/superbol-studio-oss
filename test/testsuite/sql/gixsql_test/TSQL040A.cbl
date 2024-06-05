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

      ******************************************************************00020001
      * DCLGEN TABLE(APXT.P_DATA1_TD)                                  *00030001
      *        LIBRARY(PRRKXXXX.P12XXXXX.ABCDEDB2(DPCTP014))           *00040001
      *        ACTION(REPLACE)                                         *00050001
      *        LANGUAGE(COBOL)                                         *00060001
      *        NAMES(P014-)                                            *00070001
      *        QUOTE                                                   *00080001
      *        COLSUFFIX(YES)                                          *00090001
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *00100001
      ******************************************************************00110001
           EXEC SQL DECLARE APXT.P_HISTORY_TD TABLE                     00120001
           ( DATAVAL                        DATE NOT NULL,              00130001
             DATARIL                        DATE NOT NULL,              00140001
             PROGVER                        INTEGER NOT NULL,           00150001
             DATAVERS                       DATE NOT NULL,              00160001
             DATALOAD                       TIMESTAMP NOT NULL,         00170001
             USERNAME                       VARCHAR(15) NOT NULL,       00180001
             CID                            VARCHAR(60) NOT NULL,       00190001
             FILENAME                       VARCHAR(60) NOT NULL        00200001
           ) END-EXEC.                                                  00210001
      ******************************************************************00220001
      * COBOL DECLARATION FOR TABLE AEPT.P_HISTORY_TD                  *00230001
      ******************************************************************00240001
       01  DCLP-HISTORY-TD.                                             00250001
      *                       DATAVAL                                   00260001
           10 P014-DATAVAL         PIC X(10).                           00270001
      *                       DATARIL                                   00280001
           10 P014-DATARIL         PIC X(10).                           00290001
      *                       PROGVER                                   00300001
           10 P014-PROGVER         PIC S9(9) USAGE COMP.                00310001
      *                       DATAVERS                                  00320001
           10 P014-DATAVERS        PIC X(10).                           00330001
      *                       DATALOAD                                  00340001
           10 P014-DATALOAD        PIC X(26).                           00350001
           10 P014-USERNAME.                                            00360001
      *                       USERNAME LENGTH                           00370001
              49 P014-USERNAME-LEN                                      00380001
                 PIC S9(4) USAGE COMP.                                  00390001
      *                       USERNAME                                  00400001
              49 P014-USERNAME-TEXT                                     00410001
                 PIC X(15).                                             00420001
           10 P014-CID.                                                 00430001
      *                       CID LENGTH                                00440001
              49 P014-CID-LEN                                           00450001
                 PIC S9(4) USAGE COMP.                                  00460001
      *                       CID                                       00470001
              49 P014-CID-TEXT                                          00480001
                 PIC X(60).                                             00490001
           10 P014-FILENAME.                                            00500001
      *                       FILENAME LENGTH                           00510001
              49 P014-FILENAME-LEN                                      00520001
                 PIC S9(4) USAGE COMP.                                  00530001
      *                       FILENAME                                  00540001
              49 P014-FILENAME-TEXT                                     00550001
                 PIC X(60).                                             00560001
      ******************************************************************00570001
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *00580001
      ******************************************************************00590001
      *END DPCTP014                                                     00600001
           
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