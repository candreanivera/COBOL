       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNSTRING.
      * Transforms an input with values separated by commas, into a new
      * structure separated in different values

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   SELECT PETS ASSIGN TO "pets.csv"
		 ORGANIZATION IS LINE SEQUENTIAL
		 FILE STATUS IS FS-INPUT.
	
       SELECT NEWPETS ASSIGN TO "newpets.dat"
         ORGANIZATION IS LINE SEQUENTIAL
		 FILE STATUS IS FS-OUTPUT.   
          
               
       DATA DIVISION.
       FILE SECTION.
	   FD PETS.
	   01 PETSDETAILS.
			88 EOF VALUE HIGH-VALUES.
			02 DETAILS  	           PIC X(102).
		
       FD NEWPETS.
       01 PETS-RECORD.
	       05 NEW-PET-SPECIE           PIC X(10).
		   05 NEW-PET-NAME             PIC X(10).
		   05 NEW-PET-AGE              PIC 9(02).
		   05 NEW-PET-OWNER-NAME       PIC X(15).
		   05 NEW-PET-OWNER-SURNAME    PIC X(15).
		   05 NEW-PET-ADDRESS          PIC X(20).
		   05 NEW-PET-CITY             PIC X(15).
		   05 NEW-PET-COUNTRY          PIC X(15).
	   
			
       WORKING-STORAGE SECTION.   
	   01  WS-WORKING-STORAGE.
	       05 FILLER                   PIC X(27) VALUE 
		      'WORKING STORAGE STARTS HERE'.
		
           05 STRING-END               PIC 9(04).
		   05 CLOSE-FILES              PIC X(03).

		   05 FILE-STATUS.
		      10 FS-INPUT              PIC X(02).
			  10 FS-OUTPUT             PIC X(02).

		   05 PETS-WS.
	         10 WS-PET-SPECIE          PIC X(10).
			 10 WS-PET-NAME            PIC X(10).
			 10 WS-PET-AGE             PIC 9(02).
		     10 WS-PET-OWNER-NAME      PIC X(15).
		     10 WS-PET-OWNER-SURNAME   PIC X(15).
		     10 WS-PET-ADDRESS         PIC X(20).
		     10 WS-PET-CITY            PIC X(15).
		     10 WS-PET-COUNTRY         PIC X(15).		   

       PROCEDURE DIVISION.
       
	   0100-BEGIN.
		    
		   OPEN INPUT PETS.
		   OPEN OUTPUT NEWPETS.
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
	       
*******	Look for the last character different from spaces   
		   PERFORM VARYING STRING-END FROM 100 BY -1
              UNTIL DETAILS(STRING-END:1) NOT = SPACE
			  CONTINUE
		   END-PERFORM
		     
           UNSTRING PETSDETAILS(1:STRING-END) DELIMITED BY ","
             INTO  WS-PET-SPECIE         
                   WS-PET-NAME           
		           WS-PET-AGE           
		           WS-PET-OWNER-NAME     
		           WS-PET-OWNER-SURNAME  
		           WS-PET-ADDRESS        
		           WS-PET-CITY           
		           WS-PET-COUNTRY        
           END-UNSTRING.
		
		   MOVE WS-PET-SPECIE        TO NEW-PET-SPECIE
		   MOVE WS-PET-NAME          TO NEW-PET-NAME
		   MOVE WS-PET-AGE           TO NEW-PET-AGE
		   MOVE WS-PET-OWNER-NAME    TO NEW-PET-OWNER-NAME 
		   MOVE WS-PET-OWNER-SURNAME TO NEW-PET-OWNER-SURNAME
		   MOVE WS-PET-ADDRESS       TO NEW-PET-ADDRESS
		   MOVE WS-PET-CITY          TO NEW-PET-CITY
		   MOVE WS-PET-COUNTRY       TO NEW-PET-COUNTRY
		   WRITE PETS-RECORD.
		   READ PETS 
			 AT END SET EOF TO TRUE
		   END-READ.
	   
	   0200-END.
	   
	   0300-STOP-RUN.	
		   
           CLOSE PETS, NEWPETS.		
		   IF FS-INPUT NOT = '00' OR FS-OUTPUT NOT = '00'
		      DISPLAY '*******************************'
			  DISPLAY '******* FILE CLOSE ERROR *******'
			  DISPLAY '** FS-INPUT: ' FS-INPUT
			  DISPLAY '** FS-OUTPUT: ' FS-OUTPUT
		   END-IF.

           STOP RUN.
           
          END PROGRAM UNSTRING.
