       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL018A. 
       
       
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
           01 DBUSR       PIC X(64).
           01 DBPWD       PIC X(64).

           01 DATA-01      PIC X(64).
           01 DATA-02      PIC X(64).
           01 DATA-03      PIC X(64).
           01 DATA-04      PIC X(64).
           01 DATA-05      PIC X(64).
           01 DATA-06      PIC X(64).
           01 DATA-07      PIC X(64).
           01 DATA-08      PIC X(64).
           01 DATA-09      PIC X(64).
           01 DATA-10      PIC X(64).
           01 DATA-11      PIC X(64).
       

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

           DISPLAY 'CONNECT SQLCODE: ' SQLCODE.
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.

       100-MAIN.

       EXEC SQL
            SELECT
              GENEX.CASE_BARCODE AS CASE_BARCODE,
              GENEX.SAMPLE_BARCODE AS SAMPLE_BARCODE,
              GENEX.ALIQUOT_BARCODE AS ALIQUOT_BARCODE,
              GENEX.HGNC_GENE_SYMBOL AS HGNC_GENE_SYMBOL,
              CLINICAL_INFO.VARIANT_TYPE AS VARIANT_TYPE,
              GENEX.GENE_ID AS GENE_ID,
              GENEX.NORMALIZED_COUNT AS NORMALIZED_COUNT,
              GENEX.PROJECT_SHORT_NAME AS PROJECT_SHORT_NAME,
              CLINICAL_INFO.DEMO__GENDER AS GENDER,
              CLINICAL_INFO.DEMO__VITAL_STATUS AS VITAL_STATUS,
              CLINICAL_INFO.DEMO__DAYS_TO_DEATH AS DAYS_TO_DEATH
            INTO
                :DATA-01, :DATA-02, :DATA-03, :DATA-04,
                :DATA-05, :DATA-06, :DATA-07, :DATA-08,
                :DATA-09, :DATA-10, :DATA-11
            FROM ( 
              SELECT
                CASE_LIST.VARIANT_TYPE AS VARIANT_TYPE,
                CASE_LIST.CASE_BARCODE AS CASE_BARCODE,
                CLINICAL.DEMO__GENDER,
                CLINICAL.DEMO__VITAL_STATUS,
                CLINICAL.DEMO__DAYS_TO_DEATH
              FROM
                (SELECT
                  MUTATION.CASE_BARCODE,
                  MUTATION.VARIANT_TYPE
                FROM
                  ISB-CGC-BQ.TCGA_VERSIONED.SM_HG19_DCC_02 AS MUTATION
                WHERE
                  MUTATION.HUGO_SYMBOL = 'CDKN2A'
                  AND PROJECT_SHORT_NAME = 'TCGA-BLCA'
                GROUP BY
                  MUTATION.CASE_BARCODE,
                  MUTATION.VARIANT_TYPE
                ORDER BY
                  MUTATION.CASE_BARCODE
                  ) AS CASE_LIST
              INNER JOIN
                ISB-CGC-BQ.TCGA.CLINICAL_GDC_CURRENT AS CLINICAL
              ON
                CASE_LIST.CASE_BARCODE = CLINICAL.SUBMITTER_ID ) 
                    AS CLINICAL_INFO
            INNER JOIN
              ISB-CGC-BQ.TCGA_VERSIONED.RNASEQ_HG19_GDC_2017_02 
                AS GENEX
            ON
              GENEX.CASE_BARCODE = CLINICAL_INFO.CASE_BARCODE
            WHERE
              GENEX.HGNC_GENE_SYMBOL IN 
                ('MDM2', 'TP53', 'CDKN1A','CCNE1')
            ORDER BY
              CASE_BARCODE,
              HGNC_GENE_SYMBOL
       END-EXEC.

       EXEC SQL CONNECT RESET END-EXEC.

       100-EXIT. 
             STOP RUN.

       200-END.
