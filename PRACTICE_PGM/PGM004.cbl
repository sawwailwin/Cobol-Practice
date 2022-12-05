      ******************************************************************
      * Author: SAW WAI LWIN
      * Date: 14/11/2022
      * Purpose: TRAINING PGM
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPDATEINDEXFILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT UPDATE-FILE ASSIGN TO
            'D:\cOBOLTRAINING\FILETOTEST.txt'
            ORGANISATION IS INDEXED
            ACCESS MODE IS SEQUENTIAL
            RECORD KEY IS UPDATE-NO.

       DATA DIVISION.
       FILE SECTION.
       FD UPDATE-FILE.
       01 FILE-REC.
           05 UPDATE-NO PIC X(5).
           05 WRITE-NAME PIC A(10).
           05 WRITE-SAL PIC X(6).

       WORKING-STORAGE SECTION.
       01 UPDATE-DATA.
           05 UPD-NO PIC X(5).
           05 UPD-NAME PIC A(10).
           05 UPD-SAL PIC X(6).

       01 WS-FILE.
           05 WS-NO PIC X(5).
           05 WS-NAME PIC A(10).
           05 WS-SAL PIC X(6).
       01 WS-EOF PIC X(1).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "ENTER THE EMP-NO TO SEARCH"
            ACCEPT UPD-NO
            PERFORM OPEN-FILE.
            PERFORM READ-PARA.
            PERFORM SEARCH-PARA.
            PERFORM CLOSE-FILE.
            STOP RUN.

       OPEN-FILE.
            OPEN I-O UPDATE-FILE.

       READ-PARA.

            READ UPDATE-FILE INTO WS-FILE
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   PERFORM SEARCH-PARA
            END-READ.

       SEARCH-PARA.
            IF UPD-NO = WS-NO
               DISPLAY "******THE OLD DATA*******"
               DISPLAY WS-FILE
               DISPLAY "**********UPDATE FIELD*********"
               DISPLAY "ENTER NEW NAME"
               ACCEPT UPD-NAME
               DISPLAY "ENTER NEW SALARY"
               ACCEPT UPD-SAL
               MOVE UPDATE-DATA TO FILE-REC
               REWRITE FILE-REC
               DISPLAY "UPDATE SUCCESS"
               GO TO CLOSE-FILE
            ELSE IF WS-EOF = 'Y'
                DISPLAY "NO DATA AVAILABLE"
            ELSE
               PERFORM READ-PARA
            END-IF.

       CLOSE-FILE.
            CLOSE UPDATE-FILE.

       END PROGRAM UPDATEINDEXFILE.
