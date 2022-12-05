      ******************************************************************
      * Author: SAW WAI LWIN
      * Date: 15/11/2022
      * Purpose: TRAINING PGM
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. READRELATIVEFILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT WRITE-FILE ASSIGN TO
            'D:\cOBOLTRAINING\RELATIVEFILE.txt'
            ORGANISATION IS RELATIVE
            ACCESS MODE IS RANDOM
            RELATIVE KEY IS REL-POSITION.

       DATA DIVISION.
       FILE SECTION.
       FD WRITE-FILE.
       01 FILE-REC.
           05 EMP-NO PIC 9(5).
           05 EMP-NAME PIC X(10).
           05 EMP-SAL PIC 9(6).

       WORKING-STORAGE SECTION.
       01 WS-FILE-REC.
           05 WS-EMP-NO PIC 9(5).
           05 WS-EMP-NAME PIC X(10).
           05 WS-EMP-SAL PIC 9(6).
       01 OPTION PIC A(1) VALUE 'Y'.
       01 REL-POSITION PIC 9(1).
       01 I PIC 9(1) VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM OPEN-FILE.
            PERFORM PROCESS-PARA.
            PERFORM CLOSE-FILE.
            STOP RUN.

       OPEN-FILE.
            OPEN OUTPUT WRITE-FILE.

       PROCESS-PARA.
            IF OPTION = 'Y'
                COMPUTE I = I + 1
                DISPLAY "ENTER EMP NO"
                ACCEPT WS-EMP-NO
                DISPLAY "ENTER EMP NAME"
                ACCEPT WS-EMP-NAME
                DISPLAY "ENTER EMP SALARY"
                ACCEPT WS-EMP-SAL
                MOVE WS-FILE-REC TO FILE-REC
                MOVE I TO REL-POSITION
                WRITE FILE-REC
                DISPLAY "DO YOU WANT TO CONTINUE(Y/N)"
                ACCEPT OPTION
                PERFORM PROCESS-PARA
            ELSE
                GO TO CLOSE-FILE
            END-IF.

       CLOSE-FILE.
            CLOSE WRITE-FILE.

       END PROGRAM READRELATIVEFILE.
