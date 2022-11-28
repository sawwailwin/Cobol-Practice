      ******************************************************************
      * Author: SAW WAI LWIN
      * Date: 28/11/2022
      * Purpose: TRAINING PGM
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDINGTWOMATRIX.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 MATRIX-A.
           05 A OCCURS 2 TIMES.
            06 A1 PIC 9(2) VALUE ZERO OCCURS 2 TIMES.

       01 MATRIX-B.
           05 B OCCURS 2 TIMES.
            06 B1 PIC 9(2) VALUE ZERO OCCURS 2 TIMES.

       01 MATRIX-C.
           05 C OCCURS 2 TIMES.
            06 C1 PIC 9(3) OCCURS 2 TIMES.

       01 I PIC 9(1) VALUE ZERO.
       01 J PIC 9(1) VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM READ-MATRIXA.
            PERFORM READ-MATRIXB.
            PERFORM ADD-PARA.
            DISPLAY "TWO MATRIX SUM RESULT..."
            PERFORM DISPLAY-PARA.
            STOP RUN.

       READ-MATRIXA.
            PERFORM VARYING I FROM 1 BY 1 UNTIL I > 2
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 2
                   DISPLAY "READ FOR MATRIX A INDEX " I ","J
                   ACCEPT A1(I,J)
               END-PERFORM
            END-PERFORM.

       READ-MATRIXB.
            PERFORM VARYING I FROM 1 BY 1 UNTIL I > 2
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 2
                   DISPLAY "READ FOR MATRIX B INDEX " I ","J
                   ACCEPT B1(I,J)
               END-PERFORM
            END-PERFORM.

       ADD-PARA.
            PERFORM VARYING I FROM 1 BY 1 UNTIL I > 2
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 2
                   COMPUTE C1(I,J) = A1(I,J) * B1(I,J)
               END-PERFORM
            END-PERFORM.

       DISPLAY-PARA.
            PERFORM VARYING I FROM 1 BY 1 UNTIL I > 2
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 2
                   DISPLAY C1(I,J)
               END-PERFORM
            END-PERFORM.
       END PROGRAM ADDINGTWOMATRIX.
