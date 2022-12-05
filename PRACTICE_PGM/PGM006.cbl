      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DELETETESTINDEXFILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT RECORD-FILE ASSIGN TO
            'D:\cOBOLTRAINING\FILETOTEST.txt'
            ORGANISATION IS INDEXED
            ACCESS MODE IS SEQUENTIAL
            RECORD KEY IS EMP-NO.

       DATA DIVISION.
       FILE SECTION.
       FD RECORD-FILE.
       01 FILE-REC.
           05 EMP-NO PIC X(5).
           05 EMP-NAME PIC A(10).
           05 EMP-SAL PIC X(6).

       WORKING-STORAGE SECTION.
       01 WS-FILE.
           05 WS-NO PIC X(5).
           05 WS-NAME PIC A(10).
           05 WS-SAL PIC X(6).
       77 EOF PIC A(1).
       01 OPTION PIC A(1).

       77 IN-EMP-NO PIC X(5) VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "ENTER EMP-NO TO DELETE"
            ACCEPT IN-EMP-NO
            PERFORM OPEN-FILE.
            PERFORM READ-PARA.
            PERFORM PROCESS-PARA.
            PERFORM CLOSE-FILE.
            STOP RUN.
       OPEN-FILE.
            OPEN I-O RECORD-FILE.

       READ-PARA.
            READ RECORD-FILE INTO WS-FILE
               AT END MOVE 'Y' TO EOF
               NOT AT END
                   PERFORM PROCESS-PARA
            END-READ.

       PROCESS-PARA.
            IF IN-EMP-NO = WS-NO
                DISPLAY "*******RECORD DATA*******"
                DISPLAY WS-FILE
                DISPLAY "*************************"
                DISPLAY "ARE U SURE TO DELETE(Y/N)"
                ACCEPT OPTION
                IF OPTION = 'Y'
                   DELETE RECORD-FILE RECORD
                   DISPLAY "SUCCESSFULLY DELETED"
                   GO TO CLOSE-FILE
                ELSE
                    GO TO CLOSE-FILE
                END-IF
            ELSE IF EOF = 'Y'
                DISPLAY "NO DATA AVAILABLE"
            ELSE
                PERFORM READ-PARA
            END-IF.

       CLOSE-FILE.
            CLOSE RECORD-FILE.

       END PROGRAM DELETETESTINDEXFILE.
