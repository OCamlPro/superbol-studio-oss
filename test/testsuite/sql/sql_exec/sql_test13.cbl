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
       
           01 DATASRC PIC X(64).
           01 DBUSR  PIC X(64).
           01 DBPWD  PIC X(64).
           
           01 T1     PIC 9(3) VALUE 0.  
       
              PROCEDURE DIVISION. 
 
       000-CONNECT.

           EXEC SQL
              SELECT TXID_CURRENT() INTO :S-TXID
           END-EXEC.
      *
           IF INTERNAL-TXID = S-TXID
              EXIT SECTION
           END-IF.
      *
           EXEC SQL
            BEGIN
                SELECT ID INTO :EMPID FROM emp 
                    WHERE empname = :EMPNAME;
                EXCEPTION
                    WHEN NO_DATA_FOUND THEN
                        RAISE EXCEPTION 
                            'employee % not found', :EMPNAME;
                    WHEN TOO_MANY_ROWS THEN
                        RAISE EXCEPTION 
                            'employee % not unique', :EMPNAME;
            END;
           END-EXEC.
      *
      
           EXEC SQL
              BEGIN
           END-EXEC.
      *
           EXEC SQL
              SELECT TXID_CURRENT() INTO :S-TXID
           END-EXEC.
      *
           MOVE S-TXID TO INTERNAL-TXID.

      *-----------------------------------------------------------*
       DO-COMMIT SECTION.
           EXEC SQL
              COMMIT
           END-EXEC.
      *-----------------------------------------------------------*
       DO-ROLLBACK SECTION.
           EXEC SQL
              ROLLBACK
           END-EXEC.

       100-MAIN.


       200-END.