       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL023B. 
       
       
       ENVIRONMENT DIVISION. 
       
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-AT. 
       OBJECT-COMPUTER. IBM-AT. 
       
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       
       DATA DIVISION.  

       FILE SECTION.
      
       WORKING-STORAGE SECTION. 
       
           01 S-TXID            PIC X(100).
           01 INTERNAL-TXID     PIC X(100).
           01 EMPID             PIC 9(18).
           01 EMPNAME           PIC X(100).
           
           01 DBS               PIC X(100).
       
       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 

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
          EXEC SQL AT :DBS
             ROLLBACK WORK RELEASE
          END-EXEC.

       100-MAIN.

 

       200-END.
