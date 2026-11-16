IDENTIFICATION DIVISION.
       PROGRAM-ID. BRNUPD01.
       AUTHOR. CORE-BANKING-TEAM.
      *================================================================*
      * PURPOSE: TELLER CASH DEPOSIT WITH SAFE VSAM LOCKING & RETRY    *
      * FILES:   1. CUSTKSDS (CUSTOMER ACCOUNT - LOCKED 1ST)           *
      *          2. ACCTKSDS (BRANCH TOTALS    - LOCKED 2ND)           *
      *================================================================*

       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
      *--- CICS RESPONSE & RETRY CONTROL VARIABLES --------------------*
       01  WS-RESP                 PIC S9(8) COMP VALUE ZEROS.
       01  WS-RESP2                PIC S9(8) COMP VALUE ZEROS.
       01  WS-RETRY-CNT            PIC 9(2)       VALUE ZEROS.
       01  WS-MAX-RETRIES          PIC 9(2)       VALUE 3.

       01  WS-LOCK-STATUS          PIC X(1)       VALUE 'N'.
           88  LOCK-ACQUIRED                      VALUE 'Y'.
           88  LOCK-NOT-ACQUIRED                  VALUE 'N'.

      *--- FILE RECORD LAYOUTS ----------------------------------------*
       01  WS-CUST-REC.
           05  CUST-ID             PIC X(10).
           05  CUST-NAME           PIC X(30).
           05  CUST-BALANCE        PIC S9(9)V99 COMP-3.
           05  FILLER              PIC X(34).

       01  WS-BRANCH-REC.
           05  BRN-CODE            PIC X(6).
           05  BRN-CASH-TOTAL      PIC S9(11)V99 COMP-3.
           05  BRN-TXN-COUNT       PIC S9(7)     COMP-3.
           05  FILLER              PIC X(64).

      *--- BMS SCREEN MAP COPYBOOK (TELLER SCREEN FIELDS) -------------*
       01  TELLMAPO.
           05  FILLER              PIC X(12).
           05  ACCTI               PIC X(10).
           05  BRNCI               PIC X(6).
           05  AMTI                PIC 9(7)V99.
           05  MSGO                PIC X(60).

       LINKAGE SECTION.
      *--- COMMAREA PASSES DATA BETWEEN TELLER SCREEN INTERACTIONS ----*
       01  DFHCOMMAREA.
           05  CA-TRANS-STATE      PIC X(1).
               88  FIRST-TIME                     VALUE '1'.
               88  PROCESS-INPUT                  VALUE '2'.

       PROCEDURE DIVISION.

      *================================================================*
      * 0000-MAINLINE: ENTRY POINT EVERY TIME TELLER PRESSES ENTER     *
      *================================================================*
       0000-MAINLINE.
      *    SAFETY NET: CATCH UNEXPECTED DEADLOCK ABENDS (AFCZ/AEXY)
           EXEC CICS HANDLE ABEND
                LABEL(9500-DEADLOCK-RECOVERY)
           END-EXEC.

           IF EIBCALEN = 0
               PERFORM 1000-SEND-EMPTY-SCREEN
           ELSE
               PERFORM 2000-PROCESS-TELLER-DEPOSIT
           END-IF.

           EXEC CICS RETURN
                TRANSID('TDEP')
                COMMAREA(DFHCOMMAREA)
                LENGTH(1)
           END-EXEC.

      *================================================================*
      * 1000-SEND-EMPTY-SCREEN: FIRST TIME TELLER OPENS THE SCREEN     *
      *================================================================*
       1000-SEND-EMPTY-SCREEN.
           MOVE LOW-VALUES TO TELLMAPO
           MOVE 'ENTER ACCOUNT, BRANCH, AND DEPOSIT AMOUNT' TO MSGO
           MOVE '2' TO CA-TRANS-STATE

           EXEC CICS SEND
                MAP('TELLMAP')
                MAPSET('TELLSET')
                ERASE
           END-EXEC.

      *================================================================*
      * 2000-PROCESS-TELLER-DEPOSIT: MAIN BUSINESS & LOCKING FLOW      *
      *================================================================*
       2000-PROCESS-TELLER-DEPOSIT.
           EXEC CICS RECEIVE
                MAP('TELLMAP')
                MAPSET('TELLSET')
                INTO(TELLMAPO)
                RESP(WS-RESP)
           END-EXEC.

      *    RULE 1: ALWAYS LOCK CUSTOMER ACCOUNT FIRST (LOCK ORDER #1)
           MOVE ACCTI TO CUST-ID
           PERFORM 3000-LOCK-AND-READ-CUSTOMER

           IF LOCK-NOT-ACQUIRED
               MOVE 'CUSTOMER ACCOUNT BUSY - PLEASE PRESS ENTER TO RETRY'
                 TO MSGO
               PERFORM 8000-REFRESH-SCREEN-ONLY
               GO TO 2000-EXIT
           END-IF.

      *    RULE 2: ALWAYS LOCK BRANCH RECORD SECOND (LOCK ORDER #2)
           MOVE BRNCI TO BRN-CODE
           PERFORM 4000-LOCK-AND-READ-BRANCH

           IF LOCK-NOT-ACQUIRED
      *        IMPORTANT: RELEASE CUSTOMER LOCK SO WE DON'T BLOCK OTHERS!
               EXEC CICS UNLOCK
                    FILE('CUSTKSDS')
                    RESP(WS-RESP)
               END-EXEC
               MOVE 'BRANCH TOTAL IN USE - PLEASE PRESS ENTER TO RETRY'
                 TO MSGO
               PERFORM 8000-REFRESH-SCREEN-ONLY
               GO TO 2000-EXIT
           END-IF.

      *    BOTH RECORDS ARE SAFELY LOCKED -> UPDATE MATH & SAVE TO DISK
           PERFORM 5000-UPDATE-BOTH-RECORDS.

       2000-EXIT.
           EXIT.

      *================================================================*
      * 3000-LOCK-AND-READ-CUSTOMER: BOUNDED RETRY LOOP (ORDER #1)     *
      *================================================================*
       3000-LOCK-AND-READ-CUSTOMER.
           MOVE ZEROS TO WS-RETRY-CNT
           SET LOCK-NOT-ACQUIRED TO TRUE

           PERFORM UNTIL LOCK-ACQUIRED OR WS-RETRY-CNT >= WS-MAX-RETRIES
               EXEC CICS READ
                    FILE('CUSTKSDS')
                    INTO(WS-CUST-REC)
                    RIDFLD(CUST-ID)
                    UPDATE
                    NOSUSPEND
                    RESP(WS-RESP)
                    RESP2(WS-RESP2)
               END-EXEC

               EVALUATE WS-RESP
                   WHEN DFHRESP(NORMAL)
                       SET LOCK-ACQUIRED TO TRUE
                   WHEN DFHRESP(LOCKED)
                       ADD 1 TO WS-RETRY-CNT
                       IF WS-RETRY-CNT < WS-MAX-RETRIES
                           EXEC CICS DELAY FOR SECONDS(1) END-EXEC
                       END-IF
                   WHEN DFHRESP(NOTFND)
                       MOVE 'INVALID ACCOUNT NUMBER' TO MSGO
                       PERFORM 8000-REFRESH-SCREEN-ONLY
                       EXEC CICS RETURN TRANSID('TDEP') END-EXEC
                   WHEN OTHER
                       PERFORM 9900-CRITICAL-ERROR
               END-EVALUATE
           END-PERFORM.

      *================================================================*
      * 4000-LOCK-AND-READ-BRANCH: BOUNDED RETRY LOOP (ORDER #2)       *
      *================================================================*
       4000-LOCK-AND-READ-BRANCH.
           MOVE ZEROS TO WS-RETRY-CNT
           SET LOCK-NOT-ACQUIRED TO TRUE

           PERFORM UNTIL LOCK-ACQUIRED OR WS-RETRY-CNT >= WS-MAX-RETRIES
               EXEC CICS READ
                    FILE('ACCTKSDS')
                    INTO(WS-BRANCH-REC)
                    RIDFLD(BRN-CODE)
                    UPDATE
                    NOSUSPEND
                    RESP(WS-RESP)
                    RESP2(WS-RESP2)
               END-EXEC

               EVALUATE WS-RESP
                   WHEN DFHRESP(NORMAL)
                       SET LOCK-ACQUIRED TO TRUE
                   WHEN DFHRESP(LOCKED)
                       ADD 1 TO WS-RETRY-CNT
                       IF WS-RETRY-CNT < WS-MAX-RETRIES
                           EXEC CICS DELAY FOR SECONDS(1) END-EXEC
                       END-IF
                   WHEN DFHRESP(NOTFND)
                       EXEC CICS UNLOCK FILE('CUSTKSDS') END-EXEC
                       MOVE 'INVALID BRANCH CODE' TO MSGO
                       PERFORM 8000-REFRESH-SCREEN-ONLY
                       EXEC CICS RETURN TRANSID('TDEP') END-EXEC
                   WHEN OTHER
                       PERFORM 9900-CRITICAL-ERROR
               END-EVALUATE
           END-PERFORM.

      *================================================================*
      * 5000-UPDATE-BOTH-RECORDS: WRITE CHANGES & RELEASE ALL LOCKS    *
      *================================================================*
       5000-UPDATE-BOTH-RECORDS.
           ADD AMTI TO CUST-BALANCE
           ADD AMTI TO BRN-CASH-TOTAL
           ADD 1    TO BRN-TXN-COUNT

           EXEC CICS REWRITE
                FILE('CUSTKSDS')
                FROM(WS-CUST-REC)
                RESP(WS-RESP)
           END-EXEC.

           EXEC CICS REWRITE
                FILE('ACCTKSDS')
                FROM(WS-BRANCH-REC)
                RESP(WS-RESP)
           END-EXEC.

      *    COMMIT CHANGES & AUTOMATICALLY RELEASE BOTH VSAM LOCKS
           EXEC CICS SYNCPOINT END-EXEC.

           MOVE 'DEPOSIT SUCCESSFUL - READY FOR NEXT TRANSACTION' TO MSGO
           PERFORM 8000-REFRESH-SCREEN-ONLY.

      *================================================================*
      * 8000-REFRESH-SCREEN-ONLY: SEND MESSAGE WITHOUT WIPING INPUTS   *
      *================================================================*
       8000-REFRESH-SCREEN-ONLY.
           EXEC CICS SEND
                MAP('TELLMAP')
                MAPSET('TELLSET')
                DATAONLY
           END-EXEC.

      *================================================================*
      * 9500-DEADLOCK-RECOVERY: ROLLBACK IF CICS FORCES AN ABEND       *
      *================================================================*
       9500-DEADLOCK-RECOVERY.
           EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           MOVE 'SYSTEM BUSY (LOCK CONFLICT) - PLEASE RETRY' TO MSGO
           PERFORM 8000-REFRESH-SCREEN-ONLY
           EXEC CICS RETURN TRANSID('TDEP') END-EXEC.

       9900-CRITICAL-ERROR.
           EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           MOVE 'TECHNICAL ERROR - CONTACT SUPPORT' TO MSGO
           PERFORM 8000-REFRESH-SCREEN-ONLY
           EXEC CICS RETURN TRANSID('TDEP') END-EXEC.
