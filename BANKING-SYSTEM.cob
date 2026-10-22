
       IDENTIFICATION DIVISION.
         PROGRAM-ID. BANKING-SYSTEM.
         AUTHOR. SANJAY.

       DATA DIVISION.
         WORKING-STORAGE SECTION.

         01 ACCOUNT-NO PIC 9(6) VALUE 100001.
         01 BALANCE    PIC 9(7)V99 VALUE 500000.
         01 AMOUNT     PIC 9(7)V99 VALUE 0.
         01 CHOICE     PIC 9.

       PROCEDURE DIVISION.

           DISPLAY "BANKING SYSTEM STARTED..."
           DISPLAY "ACCOUNT NUMBER: " ACCOUNT-NO
           DISPLAY "INITIAL BALANCE: " BALANCE

           DISPLAY "1. DEPOSIT"
           DISPLAY "2. WITHDRAW"
           DISPLAY "3. CHECK BALANCE"
           DISPLAY "ENTER YOUR CHOICE: "
           ACCEPT CHOICE

           EVALUATE CHOICE

               WHEN 1
                   DISPLAY "ENTER DEPOSIT AMOUNT:"
                   ACCEPT AMOUNT

                   IF AMOUNT > 0
                       COMPUTE BALANCE = BALANCE + AMOUNT
                       DISPLAY "DEPOSIT SUCCESSFUL"
                       DISPLAY "CURRENT BALANCE: " BALANCE
                   ELSE
                       DISPLAY "INVALID AMOUNT"
                   END-IF

               WHEN 2
                   DISPLAY "ENTER WITHDRAW AMOUNT:"
                   ACCEPT AMOUNT

                   IF AMOUNT > 0 AND AMOUNT <= BALANCE
                       COMPUTE BALANCE = BALANCE - AMOUNT
                       DISPLAY "WITHDRAWAL SUCCESSFUL"
                       DISPLAY "CURRENT BALANCE: " BALANCE
                   ELSE
                       DISPLAY "INVALID AMOUNT OR INSUFFICIENT BALANCE"
                   END-IF

               WHEN 3
                   DISPLAY "CURRENT BALANCE: " BALANCE

               WHEN OTHER
                   DISPLAY "INVALID CHOICE"

           END-EVALUATE

           DISPLAY "BANKING SYSTEM COMPLETED."
           STOP RUN.
