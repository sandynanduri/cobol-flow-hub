      * EMPLOYEE-CONSTANTS COPYBOOK
      * DEFINES CONSTANTS USED IN EMPLOYEE PROCESSING
      *
       01  CONSTANT-VALUES.
           05  MAX-EMPLOYEES       PIC 9(4) VALUE 9999.
           05  MIN-SALARY          PIC 9(7)V99 VALUE 25000.00.
           05  MAX-SALARY          PIC 9(7)V99 VALUE 150000.00.
           05  STANDARD-TAX-RATE   PIC V999 VALUE .15.
           05  MANAGER-TAX-RATE    PIC V999 VALUE .20.
           
       01  DEPARTMENT-CODES.
           05  DEPT-MGMT           PIC X(4) VALUE 'MGMT'.
           05  DEPT-SALES          PIC X(4) VALUE 'SALE'.
           05  DEPT-IT             PIC X(4) VALUE 'IT  '.
           05  DEPT-HR             PIC X(4) VALUE 'HR  '.
           05  DEPT-FINANCE        PIC X(4) VALUE 'FIN '.
           
       01  ERROR-MESSAGES.
           05  MSG-FILE-ERROR      PIC X(50) VALUE
               'ERROR: Unable to process file operation'.
           05  MSG-INVALID-EMP     PIC X(50) VALUE
               'ERROR: Invalid employee data detected'.
           05  MSG-CALC-ERROR      PIC X(50) VALUE
               'ERROR: Calculation error in payroll processing'. 