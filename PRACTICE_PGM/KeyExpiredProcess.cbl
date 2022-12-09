      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. KeyExpiredProcess.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KEYFILE ASSIGN TO
           'D:\cOBOLTRAINING\Transaction.txt'
           ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD KEYFILE.
       01 CALCULATOR-FILE.
           05 BRANCH-CODE PIC 9(3).
           05 PRODUCT-CODE PIC 9(2).
           05 QUANTITY PIC 9(2).

       WORKING-STORAGE SECTION.
       77 STRING1 PIC A(8) VALUE "NBIG KEY".
       77 STRING2 PIC A(14) VALUE "    NSMALL KEY".
       77 STRING3 PIC A(11) VALUE "   OBIG KEY".
       77 STRING4 PIC A(13) VALUE "   OSMALL KEY".
       77 STRING5 PIC A(19) VALUE "   TOTAL SAME P KEY".
       77 STRING6 PIC A(19) VALUE "   TOTAL SAME B KEY".
       77 STRING7 PIC X(84) VALUE SPACES.

       01 WS-NEWKEY.
           05 WS-BRANCH-CODE PIC 9(3).
           05 WS-PRODUCT-CODE PIC 9(2).
           05 WS-QUANTITY PIC 9(2).
       01 WS-EOF PIC A(3).

       01 WS-OLDKEY.
           05 WS-TEMP-BCODE PIC 9(3).
           05 WS-TEMP-PCODE PIC 9(2).
           05 WS-TEMP-QTY PIC 9(2).
       01 WS-TEMP PIC 9(3).

       01 WS-BRANCH-TOTAL PIC 9(3).
       01 WS-PRODUCT-TOTAL PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            STRING STRING1,STRING2,STRING3,STRING4,STRING5,STRING6
            DELIMITED BY SIZE INTO STRING7
            END-STRING.
            DISPLAY STRING7.

            PERFORM OPEN-PARA.
            PERFORM READ-PARA.
            PERFORM CLOSE-PARA.
            STOP RUN.

       OPEN-PARA.
            OPEN INPUT KEYFILE.

       READ-PARA.
            READ KEYFILE INTO WS-NEWKEY
               AT END MOVE 'MAX' TO WS-EOF
               NOT AT END
      *             DISPLAY WS-NEWKEY

                   IF WS-BRANCH-CODE = 0 AND WS-PRODUCT-CODE = 0 THEN
                       GO TO CLOSE-PARA
                   ELSE IF WS-TEMP-BCODE = ZEROS AND
                       WS-TEMP-PCODE = ZEROS THEN

                       ADD WS-QUANTITY TO WS-PRODUCT-TOTAL
                       ADD WS-QUANTITY TO WS-BRANCH-TOTAL
                       DISPLAY
                       WS-BRANCH-CODE"         "
                       WS-PRODUCT-CODE"           "
                       WS-TEMP-BCODE"        "
                       WS-TEMP-PCODE"           "
                       WS-PRODUCT-TOTAL"                 "
                       WS-BRANCH-TOTAL
                       MOVE WS-NEWKEY TO WS-OLDKEY

                       PERFORM READ-PARA

                   ELSE IF WS-BRANCH-CODE NOT= WS-TEMP-BCODE AND
                       WS-PRODUCT-CODE = WS-TEMP-PCODE THEN
                       PERFORM BIG-KEY-BROKEN-PARA THRU
                       SMALL-KEY-BROKEN-PARA

                   ELSE IF WS-BRANCH-CODE = WS-TEMP-BCODE AND
                       WS-PRODUCT-CODE NOT= WS-TEMP-PCODE THEN
                       PERFORM SMALL-KEY-BROKEN-PARA

                   ELSE IF WS-BRANCH-CODE NOT= WS-TEMP-BCODE AND
                       WS-PRODUCT-CODE NOT= WS-TEMP-PCODE THEN
                       PERFORM BIG-KEY-BROKEN-PARA THRU
                       SMALL-KEY-BROKEN-PARA
                       PERFORM READ-PARA


                   ELSE
                       ADD WS-QUANTITY TO WS-BRANCH-TOTAL
                       ADD WS-QUANTITY TO WS-PRODUCT-TOTAL

                       DISPLAY
                       WS-BRANCH-CODE"         "
                       WS-PRODUCT-CODE"           "
                       WS-TEMP-BCODE"        "
                       WS-TEMP-PCODE"           "
                       WS-PRODUCT-TOTAL"                 "
                       WS-BRANCH-TOTAL
                       MOVE WS-NEWKEY TO WS-OLDKEY

                       PERFORM READ-PARA

                   END-IF
            END-READ.

       BIG-KEY-BROKEN-PARA.
            MOVE 000 TO WS-BRANCH-TOTAL.

       SMALL-KEY-BROKEN-PARA.
            MOVE 000 TO WS-PRODUCT-TOTAL
            ADD WS-QUANTITY TO WS-BRANCH-TOTAL
            ADD WS-QUANTITY TO WS-PRODUCT-TOTAL

            DISPLAY
            WS-BRANCH-CODE"         "
            WS-PRODUCT-CODE"           "
            WS-TEMP-BCODE"        "
            WS-TEMP-PCODE"           "
            WS-PRODUCT-TOTAL"                 "
            WS-BRANCH-TOTAL
            MOVE WS-NEWKEY TO WS-OLDKEY

            PERFORM READ-PARA
            .

       CLOSE-PARA.
            CLOSE KEYFILE.

       END PROGRAM KeyExpiredProcess.
