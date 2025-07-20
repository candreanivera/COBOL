       IDENTIFICATION DIVISION.
       PROGRAM-ID. MERGEEMPLOYEES.
      *********************************************************
      * MODIFIED BY: CRISTINA ANDREANI
      * TAKES 2 INPUTS CONTAINING INFORMATION OF EMPLOYEES
      * AND MERGES BOTH FILES INTO 1 OUTPUT USING THE
      * MERGE UTILITY
      *********************************************************

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   SELECT ACME ASSIGN         TO "ACME.DAT"
           FILE STATUS IS ACME-FS
		   ORGANIZATION IS LINE SEQUENTIAL.

	   SELECT FUSESINC ASSIGN     TO "FUSESINC.DAT"
           FILE STATUS IS FUSESINC-FS
	       ORGANIZATION IS LINE SEQUENTIAL.
    
       SELECT SORTEDFILE ASSIGN   TO "SORTED.DAT"
           FILE STATUS IS FS-SORTED
           ORGANIZATION IS LINE SEQUENTIAL.
		
       SELECT WORKFILE ASSIGN     TO "WORK.TMP".

	               
       DATA DIVISION.
       FILE SECTION.
	   FD ACME.
	   01 ACME-REG                PIC X(47).
	
       FD FUSESINC.
	   01 FUSESINC-REG            PIC X(47).
				
       FD SORTEDFILE.
       01 SORTED-REG.         	
           88 END-SORTED    VALUE HIGH-VALUES.
           02 SORTED-SOC-SEC      PIC 9(09).
           02 SORTED-LAST-NAME    PIC X(10).
           02 SORTED-NAME         PIC X(10).
           02 FILLER              PIC X(17).
		   02 SORTED-GENDER       PIC X(01).

       SD WORKFILE.
       01 WORKREC.
          	02 WS-SOCIAL-SEC      PIC 9(09).
			02 WS-LASTNAME        PIC X(10).
			02 WS-NAME            PIC X(10).
			02 FILLER             PIC X(17).		  
            02 WS-GENDER          PIC X(01).	   
           
   
       WORKING-STORAGE SECTION.
       01  WS-WORKING-STORAGE.
           05 FILLER              PIC X(27) VALUE 
		      'WORKING STORAGE STARTS HERE'.   
   
	   01  WS-WORK-AREAS.
	       05 ACME-FS             PIC X(02).
           05 FUSESINC-FS         PIC X(02).
           05 FS-SORTED           PIC X(02).
		 

       PROCEDURE DIVISION.
       0050-START.
           PERFORM 0100-READ-INPUT.
           PERFORM 9000-END-PROGRAM.

       0100-READ-INPUT.

		   OPEN INPUT FUSESINC, ACME.

           IF ACME-FS NOT = '00' OR
              FUSESINC-FS NOT = '00' 
              DISPLAY "****************************"
              DISPLAY "*** ERROR ON FILE OPEN *****"
              DISPLAY "*** FS ACME    : " ACME-FS
              DISPLAY "*** FS FUSESINC: " FUSESINC-FS
              DISPLAY "****************************"
              PERFORM 9000-END-PROGRAM
           END-IF.
		   				
		   MERGE WORKFILE ON ASCENDING KEY 
		                     WS-SOCIAL-SEC
		      USING FUSESINC 
			        ACME
			  GIVING SORTEDFILE.

	   
	   9000-END-PROGRAM.
           CLOSE FUSESINC, ACME.

           STOP RUN.         
           END PROGRAM MERGEEMPLOYEES.
