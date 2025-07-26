      * EMPLOYEE-RECORD COPYBOOK
      * DEFINES THE STRUCTURE OF EMPLOYEE RECORDS
      * LAST MODIFIED: 2025-01-26
      *
       05  EMP-ID              PIC X(6).
       05  EMP-NAME.
           10  EMP-FIRST-NAME  PIC X(15).
           10  EMP-LAST-NAME   PIC X(20).
       05  EMP-DEPT-CODE       PIC X(4).
       05  EMP-POSITION        PIC X(25).
       05  EMP-HIRE-DATE.
           10  EMP-HIRE-YEAR   PIC 9(4).
           10  EMP-HIRE-MONTH  PIC 9(2).
           10  EMP-HIRE-DAY    PIC 9(2).
       05  EMP-SALARY          PIC 9(7)V99.
       05  EMP-YEARS-SERVICE   PIC 9(2).
       05  EMP-STATUS          PIC X.
           88  EMP-ACTIVE      VALUE 'A'.
           88  EMP-INACTIVE    VALUE 'I'.
           88  EMP-TERMINATED  VALUE 'T'.
       05  FILLER              PIC X(10). 