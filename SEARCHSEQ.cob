       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEARCHSEQ.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TABLE.
           05 WS-ROW OCCURS 5 TIMES INDEXED BY WS-IDX.
               10 WS-KEY PIC 9(3).
               10 WS-DATA PIC X(10).
       01 WS-SEARCH-KEY PIC 9(3) VALUE 4.
       01 WS-FOUND PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE 1 TO WS-KEY(1). MOVE 'A' TO WS-DATA(1).
           MOVE 2 TO WS-KEY(2). MOVE 'B' TO WS-DATA(2).
           MOVE 3 TO WS-KEY(3). MOVE 'C' TO WS-DATA(3).
           MOVE 4 TO WS-KEY(4). MOVE 'D' TO WS-DATA(4).
           MOVE 5 TO WS-KEY(5). MOVE 'E' TO WS-DATA(5).

           SEARCH WS-ROW
               AT END MOVE 'Y' TO WS-FOUND
               WHEN WS-KEY(WS-IDX) = WS-SEARCH-KEY
                   DISPLAY WS-DATA(WS-IDX)
                   MOVE 'Y' TO WS-FOUND
           END-SEARCH.

           IF WS-FOUND = 'N' DISPLAY 'NOT FOUND'.
           STOP RUN.
