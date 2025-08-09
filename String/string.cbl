       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRING.
      * Takes the full name of the owner and leaves only 
      * one space between them. At the end, includes and asterisk

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   SELECT IFILE ASSIGN TO "input.dat"
		 ORGANIZATION IS LINE SEQUENTIAL
		 FILE STATUS IS FS-INPUT.
	
       SELECT OFILE ASSIGN TO "output.dat"
         ORGANIZATION IS LINE SEQUENTIAL
		 FILE STATUS IS FS-OUTPUT.   
          
               
       DATA DIVISION.
       FILE SECTION.
		
       FD IFILE.
       01 PETS-RECORD.
           88 EOF VALUES 'HIGH VALUES'.
	       05 PET-SPECIE                 PIC X(10).
		   05 PET-NAME                   PIC X(10).
		   05 PET-AGE                    PIC 9(02).
		   05 PET-OWNER-NAME             PIC X(15).
		   05 PET-OWNER-SURNAME          PIC X(15).
           05 PET-NUMBER                 PIC X(10).
           05 PET-EMAIL                  PIC X(20).
	   
       FD OFILE.
       01 PETS-RECORD-OUT                PIC X(82).
			
       WORKING-STORAGE SECTION.   
	   01  WS-WORKING-STORAGE.
	       05 FILLER                     PIC X(27) VALUE 
		      'WORKING STORAGE STARTS HERE'.
		   05 CLOSE-FILES                PIC X(03).
           05 WS-NAME-AUX                PIC X(30).

		   05 FILE-STATUS.
		      10 FS-INPUT                PIC X(02).
			  10 FS-OUTPUT               PIC X(02).

		   05 PETS-WS.
	         10 WS-PET-SPECIE            PIC X(10).
			 10 WS-PET-NAME              PIC X(10).
			 10 WS-PET-AGE               PIC 9(02).
             10 WS-FULL-NAME             PIC X(30).
		     10 WS-PET-NUMBER            PIC X(10).	
             10 WS-PET-EMAIL             PIC X(20).	   

       PROCEDURE DIVISION.
       
	   0100-BEGIN.
		    
		   OPEN INPUT IFILE.
		   OPEN OUTPUT OFILE.
		   IF FS-INPUT NOT = '00' OR FS-OUTPUT NOT = '00'
		     DISPLAY '*******************************'
			 DISPLAY '******* FILE OPEN ERROR *******'
			 DISPLAY '** FS-INPUT: ' FS-INPUT
			 DISPLAY '** FS-OUTPUT: ' FS-OUTPUT
			 PERFORM 0300-STOP-RUN
		   END-IF.
	
		   READ IFILE
			AT END SET EOF TO TRUE
			END-READ.
		  		   
           PERFORM 0200-PROCESS-RECORDS UNTIL EOF.
		 
		   PERFORM 0300-STOP-RUN.
	   
	   0200-PROCESS-RECORDS.
	        
           MOVE PET-SPECIE            TO WS-PET-SPECIE
           MOVE PET-NAME              TO WS-PET-NAME
           MOVE PET-AGE               TO WS-PET-AGE
           MOVE PET-NUMBER            TO WS-PET-NUMBER
           MOVE PET-EMAIL             to WS-PET-EMAIL
		   
      * Leaves only 1 space between name and surname
           STRING PET-OWNER-NAME DELIMITED BY SPACES
                  ' ' DELIMITED BY SIZE
                  PET-OWNER-SURNAME DELIMITED BY SPACES
           INTO WS-NAME-AUX.

           MOVE WS-NAME-AUX           TO WS-FULL-NAME
           MOVE PETS-WS               TO PETS-RECORD-OUT
           WRITE PETS-RECORD-OUT.

		   READ IFILE 
			 AT END SET EOF           TO TRUE
		   END-READ.
	   
	   0200-END.
	   
	   0300-STOP-RUN.	
		   
           CLOSE IFILE, OFILE.		
		   IF FS-INPUT NOT = '00' OR FS-OUTPUT NOT = '00'
		      DISPLAY '*******************************'
			  DISPLAY '******* FILE CLOSE ERROR *******'
			  DISPLAY '** FS-INPUT: ' FS-INPUT
			  DISPLAY '** FS-OUTPUT: ' FS-OUTPUT
		   END-IF.

           STOP RUN.
           
          END PROGRAM STRING.
