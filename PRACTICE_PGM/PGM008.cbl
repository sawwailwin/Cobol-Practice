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
            SELECT TASK-FILE ASSIGN TO
            'D:\cOBOLTRAINING\RELATIVEFILE.txt'
            ORGANISATION IS RELATIVE
            ACCESS MODE IS RANDOM
            RELATIVE KEY IS REL-POSITION.

       DATA DIVISION.
       FILE SECTION.
       FD TASK-FILE.
       01 FILE-REC.
           05 EMP-NO PIC 9(5).
           05 EMP-NAME PIC X(10).
           05 EMP-SAL PIC 9(6).

       WORKING-STORAGE SECTION.
       01 WS-FILE-REC.
           05 WS-EMP-NO PIC 9(5).
           05 WS-EMP-NAME PIC X(10).
           05 WS-EMP-SAL PIC 9(6).

       01 OPTION PIC A(1) VALUE 'N'.
       01 REL-POSITION PIC 9(1).
       01 I PIC 9(1) VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM OPEN-FILE.
            DISPLAY "ENTER THE RELATIVE POSITION"
            ACCEPT I

            PERFORM PROCESS-PARA.
            PERFORM CLOSE-FILE.
            STOP RUN.

       OPEN-FILE.
            OPEN INPUT TASK-FILE.

       PROCESS-PARA.
            MOVE I TO REL-POSITION
            READ TASK-FILE RECORD
               INVALID KEY DISPLAY "RECORD NOT FOUND"
            END-READ
            MOVE FILE-REC TO WS-FILE-REC
            DISPLAY WS-FILE-REC.

       CLOSE-FILE.
            CLOSE TASK-FILE.

       END PROGRAM READRELATIVEFILE.
