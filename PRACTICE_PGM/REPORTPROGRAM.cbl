      ******************************************************************
      * Author: SAW WAI LWIN
      * Date: 11/11/2022
      * Purpose: TRAINING PGM
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEEREPORT.
      *FILE READ, CALCULATE AND WRITE DAILY INCOME OF EMPLOYEE

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT EMPLOYEE-DATA ASSIGN TO
            'D:\cOBOLTRAINING\EMPLOYEEDATA.txt'
            ORGANISATION IS LINE SEQUENTIAL
            ACCESS MODE IS SEQUENTIAL.

            SELECT PAYROLL-DATA ASSIGN TO
            'D:\cOBOLTRAINING\EMPPAYROLL.txt'
            ORGANISATION IS LINE  SEQUENTIAL
            ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-DATA.
       01 EMP-DATA.
           05 NAME PIC X(20).
           05 W-HOUR PIC 9(2).
           05 P-RATE PIC 9(4).

       FD PAYROLL-DATA.
       01 PAY-DATA.
           05 EMP-NAME PIC X(20).
           05 WORK-HOUR PIC 9(2).
           05 PAY-RATE PIC 9(4).
           05 SALARY PIC 9(6).

       WORKING-STORAGE SECTION.
       01 WS-EMP-DATA.
           05 WS-NAME PIC X(20).
           05 WS-W-HOUR PIC 9(2).
           05 WS-P-RATE PIC 9(4).
       01 WS-EOF PIC A(1).

       77 WS-SALARY PIC 9(6).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM OPEN-FILE.
            PERFORM READ-FILE.
            PERFORM CALCULATE-PARA.
            PERFORM CLOSE-FILE.
            STOP RUN.

       OPEN-FILE.
            OPEN INPUT EMPLOYEE-DATA
                   OUTPUT PAYROLL-DATA.

       READ-FILE.
            READ EMPLOYEE-DATA INTO WS-EMP-DATA
               AT END
                   MOVE 'Y' TO WS-EOF
                   GO TO CLOSE-FILE
               NOT AT END
                   DISPLAY "NAME : "WS-NAME
                   DISPLAY "WORKING HOUR : "WS-W-HOUR
                   DISPLAY  "HOURLY RATE : "WS-P-RATE
            END-READ.

       CALCULATE-PARA.
            COMPUTE WS-SALARY = WS-W-HOUR * WS-P-RATE.
            MOVE WS-EMP-DATA TO PAY-DATA
            MOVE WS-SALARY TO SALARY.

            DISPLAY "GOT SALARY : "SALARY
            DISPLAY "**************************************************"
            WRITE PAY-DATA
            END-WRITE.
            PERFORM READ-FILE THRU CALCULATE-PARA UNTIL WS-EOF = 'Y'.

       CLOSE-FILE.
            CLOSE EMPLOYEE-DATA PAYROLL-DATA.

       END PROGRAM EMPLOYEEREPORT.
