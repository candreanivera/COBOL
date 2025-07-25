       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELATIVERECORDS.
      * CREATE A RELATIVE FILE FROM A SEQUENTIAL FILE
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

	   SELECT INPUTSTUDIOS ASSIGN TO "STUDIOSSEQ.DAT"
	     ORGANIZATION IS LINE SEQUENTIAL.
       
	   SELECT STUDIOSFILE ASSIGN TO "STUDIOSREL.DAT"
		ORGANIZATION IS RELATIVE
		ACCESS MODE IS RANDOM
		RELATIVE KEY IS STUDIOS-KEY
        FILE STATUS IS FILE-CHECK-KEY.

       DATA DIVISION.
       FILE SECTION.
	
       FD INPUTSTUDIOS.
	   01 STUDIOSSEQRECORD.
	      88 ENDOFFILE       VALUE HIGH-VALUES.
		  05 STUDIOSCODESEQ PIC 99.
		  05 STUDIOSNAMESEQ PIC X(20).
		  05 STUDIOSADDRSEQ PIC X(50).

	   FD STUDIOSFILE.
	   01 STUDIOSRECORD.
	      05 STUDIOSCODE    PIC 99.
		  05 STUDIOSNAME    PIC X(20).
		  05 STUDIOSADDRESS PIC X(50).	
		
       WORKING-STORAGE SECTION.
       01  WS-WORKING-STORAGE.
           05 FILLER      PIC X(27) VALUE 
		      'WORKING STORAGE STARTS HERE'.
   
	   01  WS-WORK-AREAS.
	       05  FILE-CHECK-KEY   PIC X(2).
		   05  STUDIOS-KEY     PIC 999.		 

       PROCEDURE DIVISION.
       0100-READ-STUDIOS.

		   OPEN INPUT INPUTSTUDIOS.
		   OPEN OUTPUT STUDIOSFILE.
		   				
           READ INPUTSTUDIOS 
		     AT END SET ENDOFFILE TO TRUE
		   END-READ.
		   PERFORM 0200-PROCESS-FILE UNTIL
		      ENDOFFILE.
		 
		   PERFORM 9000-END-PROGRAM.
		   
	   0100-END.
	   
	   0200-PROCESS-FILE.
           MOVE SPACES TO FILE-CHECK-KEY
		   MOVE STUDIOSSEQRECORD TO 
		      STUDIOSRECORD.
           MOVE STUDIOSCODESEQ TO STUDIOS-KEY
		   WRITE STUDIOSRECORD
           INVALID KEY 
               DISPLAY "INVALID KEY: " FILE-CHECK-KEY
               DISPLAY "Clave duplicada: " STUDIOS-KEY
           END-WRITE.
          
		   READ INPUTSTUDIOS
		      AT END SET ENDOFFILE TO TRUE.
		0200-END.
		   
	   9000-END-PROGRAM.
           CLOSE STUDIOSFILE, INPUTSTUDIOS. 
          
           STOP RUN.
           
          END PROGRAM RELATIVERECORDS.
