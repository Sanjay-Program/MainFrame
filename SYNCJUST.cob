       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYNCJUST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-REC.
           05 WS-A PIC X(3).
           05 WS-B PIC 9(5) SYNCHRONIZED.
           05 WS-C PIC X(5) JUSTIFIED RIGHT.

       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE 'ABC' TO WS-A.
           MOVE 12345 TO WS-B.
           MOVE 'HELLO' TO WS-C.

           DISPLAY WS-A.
           DISPLAY WS-B.
           DISPLAY WS-C.
           STOP RUN.
