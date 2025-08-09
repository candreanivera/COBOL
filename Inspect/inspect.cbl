       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSPECT.
      * Transforms the pet number: Removes hyphens and replaces
      * for spaces. Also validates the email verifying that only 
      * contains one @

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   SELECT PETS ASSIGN TO "pets.dat"
		 ORGANIZATION IS LINE SEQUENTIAL
		 FILE STATUS IS FS-INPUT.
	
       SELECT PETSOUT ASSIGN TO "petsmodified.dat"
         ORGANIZATION IS LINE SEQUENTIAL
		 FILE STATUS IS FS-OUTPUT.   
          
               
       DATA DIVISION.
       FILE SECTION.
		
       FD PETS.
       01 PETS-RECORD.
           88 EOF VALUES 'HIGH VALUES'.
	       05 PET-SPECIE           PIC X(10).
		   05 PET-NAME             PIC X(10).
		   05 PET-AGE              PIC 9(02).
		   05 PET-OWNER-NAME       PIC X(15).
		   05 PET-OWNER-SURNAME    PIC X(15).
           05 PET-NUMBER           PIC X(10).
           05 PET-EMAIL            PIC X(20).
	   
       FD PETSOUT.
       01 PETS-RECORD-OUT          PIC X(102).
			
       WORKING-STORAGE SECTION.   
	   01  WS-WORKING-STORAGE.
	       05 FILLER                   PIC X(27) VALUE 
		      'WORKING STORAGE STARTS HERE'.
		
           05 STRING-END               PIC 9(04).
		   05 CLOSE-FILES              PIC X(03).
           05 WS-CONT-EMAIL            PIC 9(03).

		   05 FILE-STATUS.
		      10 FS-INPUT              PIC X(02).
			  10 FS-OUTPUT             PIC X(02).

		   05 PETS-WS.
	         10 WS-PET-SPECIE          PIC X(10).
			 10 WS-PET-NAME            PIC X(10).
			 10 WS-PET-AGE             PIC 9(02).
		     10 WS-PET-OWNER-NAME      PIC X(15).
		     10 WS-PET-OWNER-SURNAME   PIC X(15).
		     10 WS-PET-NUMBER          PIC X(10).	
             10 WS-PET-EMAIL           PIC X(20).	   

       PROCEDURE DIVISION.
       
	   0100-BEGIN.
		    
		   OPEN INPUT PETS.
		   OPEN OUTPUT PETSOUT.
		   IF FS-INPUT NOT = '00' OR FS-OUTPUT NOT = '00'
		     DISPLAY '*******************************'
			 DISPLAY '******* FILE OPEN ERROR *******'
			 DISPLAY '** FS-INPUT: ' FS-INPUT
			 DISPLAY '** FS-OUTPUT: ' FS-OUTPUT
			 PERFORM 0300-STOP-RUN
		   END-IF.
	
		   READ PETS
			AT END SET EOF TO TRUE
			END-READ.
		  		   
           PERFORM 0200-PROCESS-RECORDS UNTIL EOF.
		 
		   PERFORM 0300-STOP-RUN.
	   
	   0200-PROCESS-RECORDS.
	       MOVE ZEROES               TO  WS-CONT-EMAIL
*******	Deletes hyphens from the PET-NUMBER field   
           MOVE PETS-RECORD          TO PETS-WS
		   INSPECT WS-PET-NUMBER REPLACING ALL '-' BY ' '.
           
******* Validates that email contains only 1 "@". If not, moves
******* 'Invalid email' to that field           
           INSPECT WS-PET-EMAIL TALLYING WS-CONT-EMAIL FOR ALL '@'
           IF WS-CONT-EMAIL NOT = 1
              MOVE 'INVALID EMAIL'   TO WS-PET-EMAIL
           END-IF

           MOVE PETS-WS     TO PETS-RECORD-OUT
           WRITE PETS-RECORD-OUT.

		   READ PETS 
			 AT END SET EOF TO TRUE
		   END-READ.
	   
	   0200-END.
	   
	   0300-STOP-RUN.	
		   
           CLOSE PETS, PETSOUT.		
		   IF FS-INPUT NOT = '00' OR FS-OUTPUT NOT = '00'
		      DISPLAY '*******************************'
			  DISPLAY '******* FILE CLOSE ERROR *******'
			  DISPLAY '** FS-INPUT: ' FS-INPUT
			  DISPLAY '** FS-OUTPUT: ' FS-OUTPUT
		   END-IF.

           STOP RUN.
           
          END PROGRAM INSPECT.
