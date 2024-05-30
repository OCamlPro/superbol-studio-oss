       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL015A. 
       
       
       ENVIRONMENT DIVISION. 
       
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-AT. 
       OBJECT-COMPUTER. IBM-AT. 
       
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       
       DATA DIVISION.  

       FILE SECTION.
      
       WORKING-STORAGE SECTION. 
       
           01 DATASRC     PIC X(64).
           01 DBS         PIC X(64).
           01 DBUSR       PIC X(64).
           01 DBPWD       PIC X(64).

           
       
           78 MY-CONSTANT   VALUE 16.

       LINKAGE SECTION.

           01 L-DYNBUFFER-LEN-1 PIC 9(4).
           01 DYNBUFFER.
              05 FILLER     OCCURS UNBOUNDED
                            DEPENDING ON L-DYNBUFFER-LEN-1
                            PIC X.       
      
           01 L-DYNBUFFER-LEN-2 PIC 9(4).
           01 L-DYNBUFFER.
              05 FILLER     OCCURS 0 TO MY-CONSTANT TIMES
                            DEPENDING ON L-DYNBUFFER-LEN-2
                            PIC X.

           01 Z-MY-ELEMENTS PIC 9(8).
           01 MY-TAB.
              05 MY-NO         PIC  9(009) COMP-5 VALUE ZERO.
              05 MY-TAB-CACHE  OCCURS 100
                 DEPENDING ON Z-MY-ELEMENTS
                 ASCENDING KEY IS MY-ID
                 INDEXED BY I-TAB.
                07 MY-ELEMENT.
                  10 MY-ID         PIC  9(009) COMP-5 VALUE ZERO.
                  10 MY-DATA       PIC  X(02189)      VALUE SPACE.

          01 MY-ELEMENTS-2             PIC  9(003)        VALUE ZERO.
          01 MY-TAB-2.
            05 MY-ID-LAST-2            PIC  9(018) COMP-5 VALUE ZERO.
            05 MY-ID-NEW-2             PIC  9(018) COMP-5 VALUE ZERO.
            05 T98-TAB-CACHE  OCCURS 050
               DEPENDING ON MY-ELEMENTS-2
               ASCENDING KEY MY-ID-2
               INDEXED BY I-TAB.
              07 MY-ELEMENT-2.
                10 MY-ID-2           PIC  9(018) COMP-5 VALUE ZERO.
                10 MY-DATA-2         PIC  X(02125)      VALUE SPACE.
                
          01 MY-ELEMENTS-3             PIC  9(003)        VALUE ZERO.
          01 MY-TAB-3.
            05 MY-ID-LAST-3            PIC  9(018) COMP-5 VALUE ZERO.
            05 MY-ID-NEW-3             PIC  9(018) COMP-5 VALUE ZERO.
            05 T98-TAB-CACHE  OCCURS 050
               DEPENDING MY-ELEMENTS-3
               ASCENDING MY-ID-3
               INDEXED I-TAB.
              07 MY-ELEMENT-3.
                10 MY-ID-3           PIC  9(018) COMP-5 VALUE ZERO.
                10 MY-DATA-3         PIC  X(02125)      VALUE SPACE.      

          01 MY-ELEMENTS-4             PIC  9(003)        VALUE ZERO.
          01 MY-TAB-4.
            05 MY-ID-LAST-4            PIC  9(018) COMP-5 VALUE ZERO.
            05 MY-ID-NEW-4             PIC  9(018) COMP-5 VALUE ZERO.
            05 T98-TAB-CACHE  OCCURS 050
               DEPENDING ON MY-ELEMENTS-4
               ASCENDING KEY IS MY-ID-4
               INDEXED BY I-TAB.
              07 MY-ELEMENT-4.
                10 MY-ID-4           PIC  9(018) COMP-5 VALUE ZERO.
                10 MY-DATA-4         PIC  X(02125)      VALUE SPACE.                
       
       PROCEDURE DIVISION. 
 
       000-CONNECT.
         DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
         ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
         DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
         ACCEPT DBUSR FROM ENVIRONMENT-VALUE.
         DISPLAY "DATASRC_PWD" UPON ENVIRONMENT-NAME.
         ACCEPT DBPWD FROM ENVIRONMENT-VALUE.

         EXEC SQL
            CONNECT TO :DATASRC USER :DBUSR USING :DBPWD
         END-EXEC.      
         
         DISPLAY 'CONNECT SQLCODE: ' SQLCODE

         IF SQLCODE <> 0 THEN
            GO TO 100-EXIT
         END-IF.

       100-MAIN.

         EXEC SQL CONNECT RESET END-EXEC.

       100-EXIT. 
             STOP RUN.

       200-END.
