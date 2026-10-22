       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOOPS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-I PIC 9(3) VALUE 0.
       01 WS-J PIC 9(3) VALUE 0.
       01 WS-SUM PIC 9(4) VALUE 0.
       01 WS-DONE PIC X VALUE 'N'.
       01 WS-COUNT PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM 5 TIMES
               COMPUTE WS-COUNT = WS-COUNT + 1
               DISPLAY WS-COUNT
           END-PERFORM.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
               COMPUTE WS-SUM = WS-SUM + WS-I
           END-PERFORM.
           DISPLAY 'SUM IS ' WS-SUM.

           MOVE 'N' TO WS-DONE.
           PERFORM LOOP-PARA UNTIL WS-DONE = 'Y'.

           PERFORM LOOP-PARA THRU LOOP-END.

           STOP RUN.

       LOOP-PARA.
           COMPUTE WS-J = WS-J + 1.
           IF WS-J = 3
               MOVE 'Y' TO WS-DONE.
           END-IF.

       LOOP-END.
           DISPLAY 'INNER LOOP VALUE ' WS-J.
