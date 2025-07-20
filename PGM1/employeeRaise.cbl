      *==============================================================*
      *  PROGRAM      : EMPLOYEERAISE                                *
      *  AUTOR        : MODIFIED BY CRISTINA ANDREANI                *
      *  DATE         : 15-07-2025                                   *
      *  DESCRIPTION  : UPDATES THE RATE FOR A LIST OF EMPLOYEES     *
      *                 GIVING A FIXED INCREASED                     *
      *==============================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEERAISE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   SELECT EMPLOYEEFILE ASSIGN TO "EMPFILE.DAT"
		ORGANIZATION IS LINE SEQUENTIAL
         FILE STATUS IS WS-FS-EMPLOYEE.  

       SELECT NEWEMPFILE ASSIGN TO "NEWEMPFILE.DAT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-FS-NEW-EMPLOYEE.     
               
       DATA DIVISION.
       FILE SECTION.
	   FD EMPLOYEEFILE.
	   01 EMPDETAILS.
			88 ENDOFFILE VALUE HIGH-VALUES.
            02 EMPDATA              PIC X(38).
            02 EMPINFO REDEFINES EMPDATA.
			   04 EMPLOYEEID  	    PIC 9(07).
			   04 EMPLOYEENAME.
				   05 LASTNAME	    PIC X(10).
				   05 FIRSTNAME     PIC X(10).
			   04 STARTDATE.
				   05 START-YEAR	PIC 9(04).
				   05 START-MONTH	PIC 9(02).
				   05 START-DAY	    PIC 9(02).
			   04 HOURSWORKED       PIC 9(03).
            02 HOURLYRATE           PIC 9(04)V99.    
            02 DEPARTMENT           PIC X(30).   
			02 GENDER               PIC X(01).

       FD NEWEMPFILE.
       01 NEWEMPLOYEE.
			04 NEW-EMP-DATA  	    PIC 9(38).
			04 NEW-HOURLY-RATE      PIC 9(04)V99.
            04 NEW-DEPARTMENT       PIC X(30).   
            04 NEW-GENDER           PIC X.

       WORKING-STORAGE SECTION.
		   
	   01  WS-WORK-AREAS.
		   05 WS-FS-EMPLOYEE        PIC X(02).
           05 WS-FS-NEW-EMPLOYEE    PIC X(02).
           05 WS-EMPLOYEE-COUNT     PIC 9(05).
           05 WS-RAISE              PIC 9V9(02) VALUE 1.03.

       PROCEDURE DIVISION.
       
       0100-READ-EMPLOYEES.
		   OPEN INPUT EMPLOYEEFILE. 
           OPEN OUTPUT NEWEMPFILE.
           IF WS-FS-EMPLOYEE NOT EQUAL ZEROES OR 
              WS-FS-NEW-EMPLOYEE NOT EQUAL ZEROES
               DISPLAY "------------------------"
               DISPLAY "ERROR IN FILE STATUS"
               DISPLAY "FS-EMPLOYEE: " WS-FS-EMPLOYEE
               DISPLAY "FS-NEW-EMPLOYEE: " WS-FS-NEW-EMPLOYEE
               DISPLAY "------------------------"
               PERFORM 9000-END-PROGRAM
           END-IF.

           INITIALIZE WS-EMPLOYEE-COUNT.

		   READ EMPLOYEEFILE
			AT END SET ENDOFFILE TO TRUE 
			END-READ.
		   PERFORM 0200-PROCESS-EMPLOYEES UNTIL ENDOFFILE.
		   PERFORM 9000-END-PROGRAM.
	   0100-END.

	   0200-PROCESS-EMPLOYEES.
              
            COMPUTE NEW-HOURLY-RATE ROUNDED = HOURLYRATE * WS-RAISE
            MOVE EMPINFO           TO NEW-EMP-DATA
            MOVE DEPARTMENT        TO NEW-DEPARTMENT
            MOVE GENDER            TO NEW-GENDER
            ADD 1                  TO WS-EMPLOYEE-COUNT

            WRITE NEWEMPLOYEE AFTER ADVANCING 1 LINE.
            IF WS-FS-NEW-EMPLOYEE NOT EQUAL ZEROES 
               DISPLAY "------------------------"
               DISPLAY "ERROR AT WRITING FILE"
               DISPLAY "FS-NEW-EMPLOYEE: " WS-FS-NEW-EMPLOYEE
               DISPLAY "------------------------"
               PERFORM 9000-END-PROGRAM
           END-IF.

			READ EMPLOYEEFILE 
			  AT END SET ENDOFFILE TO TRUE
			END-READ.
		  
	   0200-END. 
	   
	   
	   9000-END-PROGRAM.	
           CLOSE EMPLOYEEFILE, NEWEMPFILE. 
           IF WS-FS-EMPLOYEE NOT EQUAL ZEROES OR 
              WS-FS-NEW-EMPLOYEE NOT EQUAL ZEROES
           DISPLAY "------------------------"
               DISPLAY "ERROR IN FILE CLOSING"
               DISPLAY "FS-EMPLOYEE: " WS-FS-EMPLOYEE
               DISPLAY "FS-NEW-EMPLOYEE: " WS-FS-NEW-EMPLOYEE
               DISPLAY "------------------------"
           END-IF

           DISPLAY "**********************************"
           DISPLAY "*       END OF PROGRAM           *"
           DISPLAY "*                                *"
           DISPLAY "*  # Employees processed: ", WS-EMPLOYEE-COUNT.
           DISPLAY "**********************************"	
           STOP RUN.
           
          END PROGRAM EMPLOYEERAISE.
