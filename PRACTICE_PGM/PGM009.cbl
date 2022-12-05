      ******************************************************************
      * Author: SAW WAI LWIN
      * Date: 15/11/2022
      * Purpose: TRAINING PGM
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPDATERELATIVEFILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT UPDATE-FILE ASSIGN TO
            'D:\cOBOLTRAINING\RELATIVEFILE.txt'
            ORGANISATION IS RELATIVE
            ACCESS MODE IS RANDOM
            RELATIVE KEY IS REL-POSITION.

       DATA DIVISION.
       FILE SECTION.
       FD UPDATE-FILE.
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
            OPEN I-O UPDATE-FILE.

       PROCESS-PARA.
            DISPLAY "ENTER THE RELATIVE POSITION"
            ACCEPT I
            MOVE I TO REL-POSITION
            READ UPDATE-FILE RECORD
               INVALID KEY DISPLAY "RECORD NOT FOUND"
               NOT INVALID KEY
                   DISPLAY "********OLD DATA*********"
                   DISPLAY FILE-REC
                   DISPLAY "*************************"
                   DISPLAY "ENTER EMP NO"
                   ACCEPT WS-EMP-NO
                   DISPLAY "ENTER NEW NAME"
                   ACCEPT WS-EMP-NAME
                   DISPLAY "ENTER NEW SALARY"
                   ACCEPT WS-EMP-SAL
                   MOVE WS-FILE-REC TO FILE-REC
                   REWRITE FILE-REC
                   DISPLAY "UPDATE SUCCESS"
            END-READ
            .
       CLOSE-FILE.
            CLOSE UPDATE-FILE.

       END PROGRAM UPDATERELATIVEFILE.
