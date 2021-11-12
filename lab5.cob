      ******************************************************************
      *Author: David Nguyen
      *Date: November 14, 2021
      *Purpose: lab5
      ******************************************************************
       IDENTIFICATION DIVISION.
           PROGRAM-ID. lab5.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'DA-S-INPUT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRNT-FILE ASSIGN TO 'UR-S-PRNT'.
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD.
       01  INPUT-REC PIC X(80).
       FD  PRNT-FILE
               LABEL RECORDS ARE OMITTED.
       01  PRNT-REC PIC X(110).
       WORKING-STORAGE SECTION.
       01  INPUT-DATA.
           03 I-NAME PIC X(20).
           03 I-MAJOR PIC X(4).
           03 I-YEAR PIC X(4).
           03 I-LOAN.
               05 I-LOANWHOLE PIC 9(5).
               05 I-LOANDECIMAL PIC P9(2).
           03 I-PAID OCCURS 4 TIMES.
               08 I-PAIDWHOLE PIC 9(4).
               08 I-PAIDDECIMAL PIC P9(2).
       01  DATA-FORMATER.
           03 D-LOAN  PIC 9(5)V9(2).
           03 D-PAIDARRAY OCCURS 4 TIMES.
               08 D-PAID PIC 9(4)V9(2).
           03 D-TOTPAID PIC 9(5)V9(2).
           03 D-BALANCE PIC S9(5)V9(2).
       01  PRNT-DATA1.
           03 L-NAME PIC X(20).
           03 L-MAJOR PIC X(10).
           03 L-YEAR PIC X(10).
           03 L-LOAN PIC ZZZZ9.99.
           03 FILLER PIC X(2) VALUES SPACES.
           03 L-PAID1 PIC 9(4).99.
       01  PRNT-DATA2.
           03 FILLER PIC X(50) VALUE SPACES.
           03 L-PAID-OTHER PIC ZZZ9.99.
       01  PRNT-FOOTER1.
           03 FILLER PIC X(36) VALUE SPACES.
           03 FILLER PIC X(11) VALUE 'TOTAL PAID '.
           03 L-TOTPAID PIC $$$,$$9.99.
           03 FILLER PIC X(2) VALUE SPACES.
           03 FILLER PIC X(17) VALUE 'BALANCE '.
           03 L-BALANCE PIC $$$,$$9.99.
       01  PRNT-FOOTER2.
           03 FILLER PIC X(36) VALUE SPACES.
           03 FILLER PIC X(11) VALUE 'TOTAL PAID '.
           03 L-TOTPAID2 PIC $$$,$$9.99.
           03 FILLER PIC X(2) VALUE SPACES.
           03 FILLER PIC X(17) VALUE 'OVERPAID BALANCE '.
           03 L-BALANCE2.
               05 L-BALANCE2NUM PIC $$$,$$9.99.
               05 L-BALANCE2END PIC X(3) VALUE '-**'.
       01  PRNT-HEADING.
           03 FILLER PIC X(20) VALUES 'NAME'.
           03 FILLER PIC X(10) VALUES 'MAJOR'.
           03 FILLER PIC X(10) VALUES 'YEAR'.
           03 FILLER PIC X(10) VALUES 'LOAN'.
           03 FILLER PIC X(10) VALUES 'PAID'.
       01  MISC.
           03 EOF-I PIC 9 VALUE 0.
           03 SUB PIC 99.
           03 PGCOUNT PIC 99 VALUE 1.
       PROCEDURE DIVISION.
       000-MAINLINE.
           OPEN INPUT INPUT-FILE
               OUTPUT PRNT-FILE
           PERFORM 2000-READ-INPUT.
           PERFORM 1400-PRINT-HEAD.
           PERFORM 1500-LOOP
               UNTIL EOF-I = 1;
           CLOSE INPUT-FILE
               PRNT-FILE.
           STOP RUN.
       1400-PRINT-HEAD.
           IF PGCOUNT > 1 THEN
               MOVE SPACES TO PRNT-REC
               WRITE PRNT-REC AFTER ADVANCING 1
               MOVE 1 TO PGCOUNT
           END-IF.
           WRITE PRNT-REC FROM PRNT-HEADING.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC
               AFTER ADVANCING PAGE.
       1500-LOOP.
           PERFORM 1600-PRINT-INFO1.
           PERFORM 2000-READ-INPUT.
       1600-PRINT-INFO1.
           IF PGCOUNT > 8 THEN
              PERFORM 1400-PRINT-HEAD
           END-IF.
           MOVE I-NAME TO L-NAME.
           MOVE I-MAJOR TO L-MAJOR.
           MOVE I-YEAR TO L-YEAR.
           MOVE I-LOAN TO D-LOAN.
           MOVE D-LOAN TO L-LOAN.
           MOVE I-PAID(1) TO D-PAID(1)
           MOVE D-PAID(1) TO L-PAID1.
           WRITE PRNT-REC FROM PRNT-DATA1
               AFTER ADVANCING 1 LINE.
           PERFORM VARYING SUB FROM 2 BY 1
               UNTIL SUB > 4
               MOVE I-PAID(SUB) TO D-PAID(SUB)
               MOVE D-PAID(SUB) TO L-PAID-OTHER
               WRITE PRNT-REC FROM PRNT-DATA2
                   AFTER ADVANCING 1 LINE
           END-PERFORM.
           PERFORM 1700-COMPUTE-MONEY.
           ADD 1 TO PGCOUNT.
       1700-COMPUTE-MONEY.
           COMPUTE D-TOTPAID = D-PAID(1) + D-PAID(2) + 
               D-PAID(3) + D-PAID(4).
           MOVE D-TOTPAID TO L-TOTPAID.
           COMPUTE D-BALANCE = D-LOAN - D-TOTPAID.
           IF D-BALANCE >= 0 THEN
               MOVE D-BALANCE TO L-BALANCE
               WRITE PRNT-REC FROM PRNT-FOOTER1
                   AFTER ADVANCING 1 LINE
               MOVE SPACES TO PRNT-REC
               WRITE PRNT-REC
                   AFTER ADVANCING 1 LINE
           ELSE
               MOVE D-TOTPAID TO L-TOTPAID2
               MOVE D-BALANCE TO L-BALANCE2NUM
               WRITE PRNT-REC FROM PRNT-FOOTER2
                   AFTER ADVANCING 1 LINE
               MOVE SPACES TO PRNT-REC
               WRITE PRNT-REC
                   AFTER ADVANCING 1 LINE
           END-IF.
       2000-READ-INPUT.
           READ INPUT-FILE INTO INPUT-DATA
               AT END MOVE 1 TO EOF-I.
       END PROGRAM lab5.
