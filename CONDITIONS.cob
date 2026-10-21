       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONDITIONS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A PIC 9(3) VALUE 10.
       01 WS-B PIC 9(3) VALUE 20.
       01 WS-C PIC 9(3) VALUE 20.
       01 WS-GRADE PIC X VALUE 'A'.
       88 GRADE-A VALUE 'A'.
       88 GRADE-B VALUE 'B'.

       PROCEDURE DIVISION.
       MAIN-PARA.
           IF WS-A < WS-B
               DISPLAY 'A IS LESS THAN B'
           ELSE
               DISPLAY 'A IS NOT LESS THAN B'
           END-IF.

           IF WS-A = 10 AND WS-B = 20
               DISPLAY 'BOTH CONDITIONS TRUE'
           END-IF.

           IF WS-B = WS-C OR WS-A > WS-B
               DISPLAY 'AT LEAST ONE TRUE'
           END-IF.

           EVALUATE WS-GRADE
               WHEN 'A' DISPLAY 'EXCELLENT'
               WHEN 'B' DISPLAY 'GOOD'
               WHEN OTHER DISPLAY 'AVERAGE'
           END-EVALUATE.

           IF GRADE-A
               DISPLAY 'CONDITION NAME A IS TRUE'
           END-IF.

           IF NOT GRADE-B
               DISPLAY 'GRADE IS NOT B'
           END-IF.

           STOP RUN.
