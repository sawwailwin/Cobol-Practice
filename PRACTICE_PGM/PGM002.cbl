      ******************************************************************
      * Author: SAW WAI LWIN
      * Date: 11/11/2022
      * Purpose: TRAINING PROGRAM
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORTTEST.

      *SORT THE EMPLOYEE-NO IN ASCENDING ORDER WHOSE SALARY UNDER 30000

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT UNSORT-FILE ASSIGN TO
            'D:\cOBOLTRAINING\UNSORTFILE.txt'
            ORGANISATION IS LINE SEQUENTIAL.
            SELECT SORT-FILE ASSIGN TO
            'D:\cOBOLTRAINING\SORTFILE.txt'
            ORGANISATION IS LINE SEQUENTIAL.
            SELECT WORK-FILE ASSIGN TO
            'D:\cOBOLTRAINING\WORKFILE.txt'
            ORGANISATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD UNSORT-FILE.
       01 UNSORT-REC.
           05 UNSORT-EMP-NO PIC 9(3).
           05 UNSORT-EMP-NAME PIC X(10).
           05 UNSORT-EMP-SALARY PIC 9(5).

       FD SORT-FILE.
       01 SORT-REC.
           05 SORT-EMP-NO PIC 9(3).
           05 SORT-EMP-NAME PIC X(10).
           05 SORT-EMP-SALARY PIC 9(5).

       SD WORK-FILE.
       01 WORK-REC.
           05 WORK-EMP-NO PIC 9(3).
           05 WORK-EMP-NAME PIC X(10).
           05 WORK-EMP-SALARY PIC 9(5).

       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            SORT WORK-FILE ON ASCENDING KEY WORK-EMP-NO
            USING UNSORT-FILE
            OUTPUT PROCEDURE IS CHK-SAL
            STOP RUN.

       CHK-SAL SECTION.
       PARA-OPEN.
            OPEN OUTPUT SORT-FILE.
       READ-PARA.
            RETURN WORK-FILE RECORD INTO SORT-REC
            AT END GO TO PARA-CLOSE
            NOT AT END
            IF SORT-EMP-SALARY < 30000
                WRITE SORT-REC
                GO TO READ-PARA
            ELSE
                GO TO READ-PARA.

       PARA-CLOSE.
            CLOSE SORT-FILE.

       END PROGRAM SORTTEST.
