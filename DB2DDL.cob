       IDENTIFICATION DIVISION.
       PROGRAM-ID. DB2DDL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       EXEC SQL INCLUDE SQLCA END-EXEC.

       PROCEDURE DIVISION.
       MAIN-PARA.
           EXEC SQL
               CREATE TABLE ACCOUNT (
                   ACCT_NO INTEGER NOT NULL,
                   CUST_NAME CHAR(30),
                   BALANCE DECIMAL(12,2),
                   PRIMARY KEY (ACCT_NO)
               )
           END-EXEC.
           IF SQLCODE = 0
               DISPLAY 'TABLE CREATED'
           ELSE
               DISPLAY 'CREATE FAILED ' SQLCODE
           END-IF.
           EXEC SQL COMMIT END-EXEC.
           STOP RUN.
