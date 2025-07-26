       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREATEINDEXFILE.
      * CREATE AN INDEXED FILE FROM A SEQUENTIAL FILE
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

	   SELECT INPUTFILE ASSIGN TO "BASEBALL2016.DAT"
	     ORGANIZATION IS LINE SEQUENTIAL
         FILE STATUS IS FS-BASE-INPUT.
       
       SELECT BASEBALLINDEXED ASSIGN TO "BASEBALLINDEXED.DAT"
        FILE STATUS IS FS-BASE-IDX
		ORGANIZATION IS INDEXED
		ACCESS MODE IS RANDOM
		RECORD KEY IS BASEBALLID
		ALTERNATE RECORD KEY IS IDX-HOMETEAM
		   WITH DUPLICATES
        ALTERNATE RECORD KEY IS IDX-AWAYTEAM
		   WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.
	   FD INPUTFILE.           
	   01 INPUTFILE-REG.
          88 INPUT-EOF VALUE HIGH-VALUES.  
	      05 BASE-ID               PIC X(36).
		  05 BASE-YEAR             PIC 9(04).
          05 BASE-DATE             PIC X(10).
          05 BASE-TIME             PIC X(13).
          05 BASE-ATTENDANCE       PIC X(05).
          05 BASE-HOMETEAM         PIC X(12).
          05 BASE-AWAYTEAM         PIC X(12).
          05 BASE-MOREINFO         PIC X(35).
	
       FD BASEBALLINDEXED.
	   01 BASE-IDX-REG.
	      05 BASEBALLID            PIC X(36).
		  05 IDX-YEAR              PIC 9(04).
          05 IDX-DATE              PIC X(10).
          05 IDX-TIME              PIC X(13).
          05 IDX-ATTENDANCE        PIC X(05).
          05 IDX-HOMETEAM          PIC X(12).
          05 IDX-AWAYTEAM          PIC X(12).
          05 IDX-MOREINFO          PIC X(35).
	
		
       WORKING-STORAGE SECTION.
       01  WS-WORKING-STORAGE.
           05 FILLER      PIC X(27) VALUE 
		      'WORKING STORAGE STARTS HERE'.
   
	   01  WS-WORK-AREAS.
	       05 FS-BASE-INPUT        PIC X(02).
           05 FS-BASE-IDX          PIC X(02).


       PROCEDURE DIVISION.
           PERFORM 0100-OPEN-FILES.
           PERFORM 0200-PROCESS.
           PERFORM 9000-END-PROGRAM.
       
       
       0100-OPEN-FILES.
		   OPEN INPUT INPUTFILE.
		   OPEN OUTPUT BASEBALLINDEXED.

           IF FS-BASE-INPUT IS NOT = '00' OR
              FS-BASE-IDX   IS NOT = '00'
              DISPLAY '****************************'
              DISPLAY '* FILE OPEN ERROR           '
              DISPLAY '* FS-INPUT FILE: ' FS-BASE-INPUT 
              DISPLAY '* FS-OUTPUT INDEXED FILE: ' FS-BASE-IDX
              DISPLAY '****************************'
           END-IF.

       0200-PROCESS.
		   				
           READ INPUTFILE 
		     AT END SET INPUT-EOF TO TRUE
		   END-READ.
		   PERFORM 0300-WRITE-OUTPUT UNTIL
		                             INPUT-EOF.
		   PERFORM 9000-END-PROGRAM.
	   
	   0300-WRITE-OUTPUT.
	      
           MOVE INPUTFILE-REG        TO BASE-IDX-REG
		   WRITE BASE-IDX-REG
		      INVALID KEY 
               DISPLAY "** ERROR, DUPLICATE KEY **" 
               DISPLAY "* FS-INDEXED FILE: " FS-BASE-IDX
               DISPLAY "* KEY: " BASE-ID
		   END-WRITE.
		   READ INPUTFILE
		      AT END SET INPUT-EOF TO TRUE.
		   
	   9000-END-PROGRAM.
           CLOSE INPUTFILE, BASEBALLINDEXED. 

           IF FS-BASE-INPUT IS NOT = '00' OR
              FS-BASE-IDX   IS NOT = '00'
              DISPLAY '****************************'
              DISPLAY '* FILE CLOSE ERROR           '
              DISPLAY '* FS-INPUT FILE: ' FS-BASE-INPUT 
              DISPLAY '* FS-OUTPUT INDEXED FILE: ' FS-BASE-IDX
              DISPLAY '****************************'
           END-IF.
           STOP RUN.
           
          END PROGRAM CREATEINDEXFILE.
