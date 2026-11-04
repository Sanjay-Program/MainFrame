       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXITTEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-I PIC 9(3) VALUE 0.
       01 WS-J PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               PERFORM INNER-PARA
           END-PERFORM.
           DISPLAY 'DONE'.
           STOP RUN.

       INNER-PARA.
           COMPUTE WS-J = WS-J + 1.
           IF WS-J = 2
               EXIT PERFORM.
           END-IF.
           DISPLAY WS-J.
