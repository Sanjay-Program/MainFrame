IDENTIFICATION DIVISION.
PROGRAM-ID. BRNUPD01.
AUTHOR. SANJAY-CORE-BANKING.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT CUSTKSDS ASSIGN TO "custksds.dat"
        ORGANIZATION IS INDEXED
        ACCESS MODE IS RANDOM
        RECORD KEY IS CUST-ID
        LOCK MODE IS MANUAL
        FILE STATUS IS WS-CUST-FS.

    SELECT BRNMSTR ASSIGN TO "brnmstr.dat"
        ORGANIZATION IS INDEXED
        ACCESS MODE IS RANDOM
        RECORD KEY IS BRN-CODE
        LOCK MODE IS MANUAL
        FILE STATUS IS WS-BRN-FS.

DATA DIVISION.
FILE SECTION.
FD  CUSTKSDS.
01  CUST-RECORD.
    05 CUST-ID                PIC X(10).
    05 CUST-NAME              PIC X(25).
    05 CUST-BALANCE           PIC 9(9)V99.

FD  BRNMSTR.
01  BRN-RECORD.
    05 BRN-CODE               PIC X(6).
    05 BRN-NAME               PIC X(20).
    05 BRN-CASH-TOTAL         PIC 9(11)V99.

WORKING-STORAGE SECTION.
01  WS-FILE-STATUSES.
    05 WS-CUST-FS             PIC X(2).
    05 WS-BRN-FS              PIC X(2).

01  WS-LOCK-CONTROLS.
    05 WS-RETRY-CNT           PIC 9(2) VALUE ZEROS.
    05 WS-MAX-RETRIES         PIC 9(2) VALUE 3.
    05 WS-LOCK-FLAG           PIC X(1) VALUE 'N'.
       88 LOCK-ACQUIRED                VALUE 'Y'.
       88 LOCK-NOT-ACQUIRED            VALUE 'N'.

01  WS-USER-INPUTS.
    05 WS-IN-CUST-ID          PIC X(10).
    05 WS-IN-BRN-CODE         PIC X(6).
    05 WS-IN-DEPOSIT          PIC 9(7)V99.
    05 WS-SIMULATE-DELAY      PIC X(1).
    05 WS-OLD-CUST-BAL        PIC 9(9)V99.
    05 WS-DISP-AMT            PIC ZZZ,ZZZ,ZZ9.99.

PROCEDURE DIVISION.
0000-MAINLINE.
    PERFORM 1000-INIT-SAMPLE-FILES.
    PERFORM 2000-GET-TELLER-INPUT.
    PERFORM 3000-PROCESS-DEPOSIT.

    CLOSE CUSTKSDS BRNMSTR.
    STOP RUN.


1000-INIT-SAMPLE-FILES.
    OPEN I-O CUSTKSDS.
    IF WS-CUST-FS = "35"
        OPEN OUTPUT CUSTKSDS
        MOVE "CUST000001" TO CUST-ID
        MOVE "ARJUN KUMAR" TO CUST-NAME
        MOVE 25000.00 TO CUST-BALANCE
        WRITE CUST-RECORD

        MOVE "CUST000002" TO CUST-ID
        MOVE "PRIYA NAIR" TO CUST-NAME
        MOVE 40000.00 TO CUST-BALANCE
        WRITE CUST-RECORD
        CLOSE CUSTKSDS
        OPEN I-O CUSTKSDS
    END-IF.

    OPEN I-O BRNMSTR.
    IF WS-BRN-FS = "35"
        OPEN OUTPUT BRNMSTR
        MOVE "BRN001" TO BRN-CODE
        MOVE "CHENNAI MAIN BRANCH" TO BRN-NAME
        MOVE 500000.00 TO BRN-CASH-TOTAL
        WRITE BRN-RECORD
        CLOSE BRNMSTR
        OPEN I-O BRNMSTR
    END-IF.


2000-GET-TELLER-INPUT.
    DISPLAY "==================================================".
    DISPLAY "   CORE BANKING TELLER DEPOSIT (CICS SIMULATION)  ".
    DISPLAY "==================================================".
    DISPLAY "Sample IDs -> Customer: CUST000001 or CUST000002".
    DISPLAY "Sample IDs -> Branch  : BRN001".
    DISPLAY "--------------------------------------------------".

    DISPLAY "Enter Customer ID (e.g. CUST000001): " WITH NO ADVANCING.
    ACCEPT WS-IN-CUST-ID.

    DISPLAY "Enter Branch Code (e.g. BRN001)    : " WITH NO ADVANCING.
    ACCEPT WS-IN-BRN-CODE.

    DISPLAY "Enter Deposit Amount (e.g. 5000)   : " WITH NO ADVANCING.
    ACCEPT WS-IN-DEPOSIT.

    DISPLAY "Hold lock for 8 sec to test 2nd terminal? (Y/N): "
        WITH NO ADVANCING.
    ACCEPT WS-SIMULATE-DELAY.


3000-PROCESS-DEPOSIT.
    *> --- STEP 1: LOCK CUSTOMER ACCOUNT FIRST ---
    MOVE WS-IN-CUST-ID TO CUST-ID.
    PERFORM 4000-LOCK-CUSTOMER-WITH-RETRY.

    IF LOCK-NOT-ACQUIRED
        DISPLAY "[SCREEN MSG] Customer Account busy - please retry."
        EXIT PARAGRAPH
    END-IF.

    *> Save old balance in case we need to roll back later!
    MOVE CUST-BALANCE TO WS-OLD-CUST-BAL.
    ADD WS-IN-DEPOSIT TO CUST-BALANCE.
    REWRITE CUST-RECORD.

    *> --- STEP 2: LOCK BRANCH TOTAL SECOND ---
    MOVE WS-IN-BRN-CODE TO BRN-CODE.
    PERFORM 5000-LOCK-BRANCH-WITH-RETRY.

    IF LOCK-NOT-ACQUIRED
        *> CRITICAL: Rollback Customer update & release lock!
        MOVE WS-OLD-CUST-BAL TO CUST-BALANCE
        REWRITE CUST-RECORD
        UNLOCK CUSTKSDS RECORD
        DISPLAY "[ROLLBACK] Customer update undone cleanly."
        DISPLAY "[SCREEN MSG] Branch record in use - please retry."
        EXIT PARAGRAPH
    END-IF.

    *> Optional pause so you can run Terminal B and see contention!
    IF WS-SIMULATE-DELAY = "Y" OR WS-SIMULATE-DELAY = "y"
        DISPLAY "[TERMINAL A] Holding lock on BRN001 for 8s..."
        CALL "C$SLEEP" USING 8
    END-IF.

    ADD WS-IN-DEPOSIT TO BRN-CASH-TOTAL.
    REWRITE BRN-RECORD.

    *> --- STEP 3: COMMIT / RELEASE ALL LOCKS ---
    UNLOCK CUSTKSDS RECORD.
    UNLOCK BRNMSTR RECORD.

    DISPLAY "--------------------------------------------------".
    DISPLAY "[SUCCESS] Deposit Committed!".
    MOVE CUST-BALANCE TO WS-DISP-AMT.
    DISPLAY "Updated Customer (" CUST-ID ") Balance: " WS-DISP-AMT.
    MOVE BRN-CASH-TOTAL TO WS-DISP-AMT.
    DISPLAY "Updated Branch   (" BRN-CODE ") Total  : " WS-DISP-AMT.
    DISPLAY "--------------------------------------------------".


4000-LOCK-CUSTOMER-WITH-RETRY.
    MOVE 0 TO WS-RETRY-CNT.
    SET LOCK-NOT-ACQUIRED TO TRUE.

    PERFORM UNTIL LOCK-ACQUIRED OR WS-RETRY-CNT >= WS-MAX-RETRIES
        READ CUSTKSDS WITH LOCK
        EVALUATE WS-CUST-FS
            WHEN "00"
                SET LOCK-ACQUIRED TO TRUE
            WHEN "51"
                ADD 1 TO WS-RETRY-CNT
                DISPLAY "[WAIT] Customer locked (FS=51). Retry "
                        WS-RETRY-CNT " of " WS-MAX-RETRIES "..."
                CALL "C$SLEEP" USING 1
            WHEN "23"
                DISPLAY "[ERROR] Customer ID not found (FS=23)."
                EXIT PERFORM
            WHEN OTHER
                DISPLAY "[ERROR] Customer file status: " WS-CUST-FS
                EXIT PERFORM
        END-EVALUATE
    END-PERFORM.


5000-LOCK-BRANCH-WITH-RETRY.
    MOVE 0 TO WS-RETRY-CNT.
    SET LOCK-NOT-ACQUIRED TO TRUE.

    PERFORM UNTIL LOCK-ACQUIRED OR WS-RETRY-CNT >= WS-MAX-RETRIES
        READ BRNMSTR WITH LOCK
        EVALUATE WS-BRN-FS
            WHEN "00"
                SET LOCK-ACQUIRED TO TRUE
            WHEN "51"
                ADD 1 TO WS-RETRY-CNT
                DISPLAY "[WAIT] Branch locked (FS=51). Retry "
                        WS-RETRY-CNT " of " WS-MAX-RETRIES "..."
                CALL "C$SLEEP" USING 1
            WHEN "23"
                DISPLAY "[ERROR] Branch Code not found (FS=23)."
                EXIT PERFORM
            WHEN OTHER
                DISPLAY "[ERROR] Branch file status: " WS-BRN-FS
                EXIT PERFORM
        END-EVALUATE
    END-PERFORM.
