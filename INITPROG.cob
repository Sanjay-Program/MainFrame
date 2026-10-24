       IDENTIFICATION DIVISION.
       PROGRAM-ID. INITPROG.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-REC.
           05 WS-A PIC 9(3).
           05 WS-B PIC X(10).
           05 WS-C PIC 9(5)V99.

       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE 123 TO WS-A.
           MOVE 'HELLO' TO WS-B.
           MOVE 99999 TO WS-C.

           INITIALIZE WS-REC.
           DISPLAY WS-A.
           DISPLAY WS-B.
           DISPLAY WS-C.

           INITIALIZE WS-A WS-B WS-C REPLACING NUMERIC BY 1 ALPHANUMERIC BY 'X'.
           DISPLAY WS-A.
           DISPLAY WS-B.
           STOP RUN.
