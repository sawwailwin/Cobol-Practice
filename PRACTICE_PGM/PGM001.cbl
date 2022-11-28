      ******************************************************************
      * Author: SAW WAI LWIN
      * Date: 28/11/2022
      * Purpose: TRAINING PGM
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARRANGEINASCENDING.

      *THIS PROGRAM ACCEPT DATA FROM USER AND ARRANGE IN ASCENDING

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 ONE-DIME.
         02 A PIC 9(2) VALUE ZERO OCCURS 5 TIMES.
       77 I PIC 9 VALUE ZERO.
       77 J PIC 9 VALUE ZERO.
       77 K PIC 9 VALUE ZERO.
       77 MIN PIC 9(2) VALUE ZERO.
       77 TEMP PIC 9(2) VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM ACCEPT-PARA VARYING I FROM 1 BY 1 UNTIL I>5.
            PERFORM PROCESS-PARA.
            DISPLAY "OUTPUT NUMBER IN ASCENDING ORDER"
            PERFORM DISP-PARA VARYING I FROM 1 BY 1 UNTIL I>5.
            STOP RUN.

       ACCEPT-PARA.
           DISPLAY "ENTER VALUE OF INDEX "I
           ACCEPT A(I).

       PROCESS-PARA.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I>4
           COMPUTE J = I + 1
           PERFORM VARYING K FROM J BY 1 UNTIL K>5
           IF A(K) < A(I) THEN
               MOVE A(I) TO TEMP
               MOVE A(K) TO A(I)
               MOVE TEMP TO A(K)
           END-IF
           END-PERFORM
           END-PERFORM.

       DISP-PARA.
           DISPLAY A(I).

       END PROGRAM ARRANGEINASCENDING.
