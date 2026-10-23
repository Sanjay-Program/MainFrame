       IDENTIFICATION DIVISION.
       PROGRAM-ID. SETIDX.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TABLE.
           05 WS-ROW OCCURS 10 TIMES INDEXED BY WS-IDX.
               10 WS-VAL PIC 9(3).
       01 WS-SEARCH PIC 9(3) VALUE 50.
       01 WS-FOUND PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
               MOVE WS-IDX TO WS-VAL(WS-IDX)
           END-PERFORM.

           SET WS-IDX TO 5.
           DISPLAY WS-VAL(WS-IDX).

           SET WS-IDX UP BY 2.
           DISPLAY WS-VAL(WS-IDX).

           SEARCH WS-ROW
               AT END MOVE 'Y' TO WS-FOUND
               WHEN WS-VAL(WS-IDX) = WS-SEARCH
                   DISPLAY 'FOUND AT ' WS-IDX
           END-SEARCH.
           STOP RUN.
