      ******************************************************************
      * Author: SAW WAI LWIN
      * Date: 14/11/2022
      * Purpose: TRAINING PGM
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WRITEINDEXEDFILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT WRITE-FILE ASSIGN TO
            'D:\cOBOLTRAINING\FILETOTEST.txt'
            ORGANISATION IS INDEXED
            ACCESS MODE IS SEQUENTIAL
            RECORD KEY IS WRITE-NO.

       DATA DIVISION.
       FILE SECTION.
       FD WRITE-FILE.
       01 FILE-REC.
           05 WRITE-NO PIC X(5).
           05 WRITE-NAME PIC A(10).
           05 WRITE-SAL PIC X(6).

       WORKING-STORAGE SECTION.
       01 IN-FILE.
           05 IN-NO PIC X(5).
           05 IN-NAME PIC A(10).
           05 IN-SAL PIC X(6).
       77 OPTION PIC A(3) VALUE 'YES'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM OPEN-FILE.
            PERFORM PROCESS-PARA.
            PERFORM CLOSE-FILE.

            STOP RUN.

       OPEN-FILE.
            OPEN OUTPUT WRITE-FILE.

       CLOSE-FILE.
            CLOSE WRITE-FILE.

       PROCESS-PARA.
            PERFORM IN-PARA UNTIL OPTION = 'NO'.
       IN-PARA.
            DISPLAY "ENTER EMP NO".
            ACCEPT IN-NO.
            DISPLAY "ENTER EMP NAME".
            ACCEPT IN-NAME.
            DISPLAY "ENTER EMP SALARY".
            ACCEPT IN-SAL.
            MOVE IN-FILE TO FILE-REC.
            WRITE FILE-REC.
            DISPLAY "DO YOU WANT TO CONTINUE (YES/NO)".
            ACCEPT OPTION.

       END PROGRAM WRITEINDEXEDFILE.
