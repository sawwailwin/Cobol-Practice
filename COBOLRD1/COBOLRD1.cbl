      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT CALCULATOR ASSIGN TO
           'D:\cOBOLTRAINING\Transaction.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CALCULATOR.
       01 CALCULATOR-FILE.
           05 BRANCH-CODE PIC 9(3).
           05 PRODUCT-CODE PIC 9(2).
           05 QUANTITY PIC 9(2).

       WORKING-STORAGE SECTION.
       77 STRING1 PIC A(12) VALUE "PRODUCT CODE".
       77 STRING2 PIC A(16) VALUE "    BRANCH CODE".
       77 STRING3 PIC A(12) VALUE "   QUANTITY".
       77 STRING4 PIC X(40) VALUE SPACES.

       01 WS-CALCULATOR.
           05 WS-BRANCH-CODE PIC 9(3).
           05 WS-PRODUCT-CODE PIC 9(2).
           05 WS-QUANTITY PIC 9(2).
       01 WS-EOF PIC A(1).

       01 WS-TEMP-ITEM.
           05 WS-TEMP-BCODE PIC 9(3).
           05 WS-TEMP-PCODE PIC 9(2).
           05 WS-TEMP-QTY PIC 9(2).
       01 WS-TEMP PIC 9(3).


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            STRING STRING1,STRING2,STRING3
            DELIMITED BY SIZE INTO STRING4
            END-STRING.
            DISPLAY STRING4.

            OPEN INPUT CALCULATOR.
               PERFORM UNTIL WS-EOF='Y'
                   READ CALCULATOR INTO WS-CALCULATOR
                       AT END MOVE 'Y' TO WS-EOF
                       NOT AT END

                       IF WS-TEMP-BCODE = ZERO AND
                           WS-TEMP-PCODE = ZERO THEN
                           ADD WS-BRANCH-CODE TO WS-TEMP-BCODE
                           ADD WS-PRODUCT-CODE TO WS-TEMP-PCODE
                           ADD WS-QUANTITY TO WS-TEMP-QTY
                       END-IF

                       IF WS-BRANCH-CODE = WS-TEMP-BCODE AND
                           WS-PRODUCT-CODE = WS-TEMP-PCODE THEN
                           ADD WS-QUANTITY TO WS-TEMP
                       ELSE IF WS-BRANCH-CODE = (ZERO OR SPACE) AND
                           WS-PRODUCT-CODE = (ZERO OR SPACE) AND
                           WS-QUANTITY = (ZERO OR SPACE) THEN
                               EXIT PROGRAM
                       ELSE
                           DISPLAY
                           WS-TEMP-BCODE'             '
                           WS-TEMP-PCODE'             '
                           WS-TEMP
                           MOVE WS-CALCULATOR TO WS-TEMP-ITEM
                           MOVE WS-QUANTITY TO WS-TEMP
                       END-IF
                   END-READ
               END-PERFORM.
               IF WS-TEMP-BCODE = (ZERO OR SPACE) AND
                   WS-TEMP-PCODE = (ZERO OR SPACE) AND
                   WS-TEMP = (ZERO OR SPACE) THEN
                       EXIT PROGRAM
               ELSE
                   DISPLAY
                   WS-TEMP-BCODE'             '
                   WS-TEMP-PCODE'             '
                   WS-TEMP
               END-IF

            CLOSE CALCULATOR.
            STOP RUN.
       STOP-PARA.
           STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
