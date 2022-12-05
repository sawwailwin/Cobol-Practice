      ******************************************************************
      * Author: SAW WAI LWIN
      * Date: 14/11/2022
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. READINDEXEDFILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT RECORD-FILE ASSIGN TO
            'D:\cOBOLTRAINING\FILETOTEST.txt'
            ORGANISATION IS INDEXED
            ACCESS MODE IS SEQUENTIAL
            RECORD KEY IS WRITE-NO.

       DATA DIVISION.
       FILE SECTION.
       FD RECORD-FILE.
       01 FILE-REC.
           05 WRITE-NO PIC X(5).
           05 WRITE-NAME PIC A(10).
           05 WRITE-SAL PIC X(6).
       WORKING-STORAGE SECTION.
       01 OUT-FILE.
           05 DISP-NO PIC X(5).
           05 DISP-NAME PIC A(10).
           05 DISP-SAL PIC X(6).
       77 EOF PIC A(1).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM OPEN-FILE.
            PERFORM READ-PARA.
            PERFORM CLOSE-FILE.
            STOP RUN.

       OPEN-FILE.
            OPEN INPUT RECORD-FILE.

       CLOSE-FILE.
            CLOSE RECORD-FILE.

       READ-PARA.
            READ RECORD-FILE INTO OUT-FILE
               AT END MOVE 'Y' TO EOF
               NOT AT END
                   PERFORM PROCESS-PARA
            END-READ.

       PROCESS-PARA.
            IF EOF NOT = 'Y'
               DISPLAY OUT-FILE
               PERFORM READ-PARA
            ELSE
               DISPLAY "NO DATA AVAILABLE"
            END-IF.

       END PROGRAM READINDEXEDFILE.
