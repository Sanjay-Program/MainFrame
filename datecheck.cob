
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATEVALID.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  INPUT-DATE       PIC X(20).
       01  DATE-LENGTH      PIC 99.
       01  YEAR-VALUE       PIC 9(4).
       01  MONTH-VALUE      PIC 99.
       01  DAY-VALUE        PIC 99.
       01  MAX-DAY          PIC 99.
       01  REM-4            PIC 9.
       01  REM-100          PIC 99.
       01  REM-400          PIC 999.
       01  LEAP-FLAG        PIC X VALUE 'N'.
       01  VALID-FLAG       PIC X VALUE 'Y'.

       PROCEDURE DIVISION.

       MAIN-PARA.
           DISPLAY "ENTER DATE (YYYYMMDD): "
               WITH NO ADVANCING
           ACCEPT INPUT-DATE

           IF INPUT-DATE(1:8) IS NUMERIC
               CONTINUE
           ELSE
               MOVE 'N' TO VALID-FLAG
           END-IF

           IF INPUT-DATE(9:12) NOT = SPACES
               MOVE 'N' TO VALID-FLAG
           END-IF

           IF VALID-FLAG = 'Y'
               MOVE INPUT-DATE(1:4) TO YEAR-VALUE
               MOVE INPUT-DATE(5:2) TO MONTH-VALUE
               MOVE INPUT-DATE(7:2) TO DAY-VALUE

               PERFORM CHECK-MONTH
           END-IF

           IF VALID-FLAG = 'Y'
               IF DAY-VALUE < 1 OR DAY-VALUE > MAX-DAY
                   MOVE 'N' TO VALID-FLAG
               END-IF
           END-IF

           IF VALID-FLAG = 'Y'
               DISPLAY "VALID DATE"
           ELSE
               DISPLAY "INVALID DATE"
           END-IF

           STOP RUN.

       CHECK-MONTH.
           IF MONTH-VALUE < 1 OR MONTH-VALUE > 12
               MOVE 'N' TO VALID-FLAG
           ELSE
               EVALUATE MONTH-VALUE
                   WHEN 1
                   WHEN 3
                   WHEN 5
                   WHEN 7
                   WHEN 8
                   WHEN 10
                   WHEN 12
                       MOVE 31 TO MAX-DAY

                   WHEN 4
                   WHEN 6
                   WHEN 9
                   WHEN 11
                       MOVE 30 TO MAX-DAY

                   WHEN 2
                       PERFORM CHECK-LEAP-YEAR
                       IF LEAP-FLAG = 'Y'
                           MOVE 29 TO MAX-DAY
                       ELSE
                           MOVE 28 TO MAX-DAY
                       END-IF
               END-EVALUATE
           END-IF.

       CHECK-LEAP-YEAR.
           MOVE 'N' TO LEAP-FLAG

           DIVIDE YEAR-VALUE BY 400
               GIVING REM-400
               REMAINDER REM-400

           IF REM-400 = 0
               MOVE 'Y' TO LEAP-FLAG
           ELSE
               DIVIDE YEAR-VALUE BY 100
                   GIVING REM-100
                   REMAINDER REM-100

               IF REM-100 NOT = 0
                   DIVIDE YEAR-VALUE BY 4
                       GIVING REM-4
                       REMAINDER REM-4

                   IF REM-4 = 0
                       MOVE 'Y' TO LEAP-FLAG
                   END-IF
               END-IF
           END-IF.

