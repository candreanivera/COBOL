       IDENTIFICATION DIVISION.
       PROGRAM-ID. READINDEXFILE.
      * READS AN INDEXED FILE USING EITHER IDGAME OR HOMETEAM 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   
	   SELECT BASEBALL ASSIGN TO "BASEBALLINDEXED.DAT"
        FILE STATUS IS BASEBALL-FS
		ORGANIZATION IS INDEXED
		ACCESS MODE IS DYNAMIC
		RECORD KEY IS BASEBALLID
		ALTERNATE RECORD KEY IS IDX-HOMETEAM
		   WITH DUPLICATES.
		   
    
       DATA DIVISION.
       FILE SECTION.
	   FD BASEBALL.
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
           05 FILLER               PIC X(27) VALUE 
		      'WORKING STORAGE STARTS HERE'.    
   
	   01  WS-WORK-AREAS.
	       05  BASEBALL-FS         PIC X(2).
		       88 RECORDFOUND      VALUE "00".
			   
		   05  READTYPE            PIC 9.
		       88 ID-KEY           VALUE 1.
			   88 HOMETEAM-KEY     VALUE 2.
               88 ALLMOVIES        VALUE 3.
			   
	       05  PRINTRECORD.
               10 PRINT-ID         PIC X(36).
               10 FILLER           PIC X(02).
               10 PRINT-HOMETEAM   PIC X(12).

       PROCEDURE DIVISION.
       0100-START.

		   OPEN INPUT BASEBALL.
           EVALUATE BASEBALL-FS
            WHEN '00'
               CONTINUE
            WHEN OTHER
               DISPLAY '********************'
               DISPLAY '* OPEN FILE ERROR  *'
               DISPLAY '* FS-INPUT: ' BASEBALL-FS
               DISPLAY '********************'
           END-EVALUATE

		   DISPLAY "TO SELECT RECORD BY GAME ID, ENTER 1". 
		   DISPLAY "TO SELECT RECORD BY HOME TEAM, ENTER 2".
			  
		   ACCEPT READTYPE.
		   
		   IF ID-KEY 
		      DISPLAY "ENTER GAME ID (36 DIGITS): " 
			    WITH NO ADVANCING		    
			  ACCEPT BASEBALLID
			  READ BASEBALL
			    KEY IS BASEBALLID
			    INVALID KEY DISPLAY "ERROR ON KEY: ",
				  BASEBALL-FS
			  END-READ			 
           END-IF	

           IF HOMETEAM-KEY
		      DISPLAY "ENTER HOME TEAM (15 CHARACTERS): " 
			    WITH NO ADVANCING
			  ACCEPT IDX-HOMETEAM
              READ BASEBALL
                KEY IS IDX-HOMETEAM			  
                INVALID KEY DISPLAY "ERROR ON KEY: ",
				  BASEBALL-FS
              END-READ
			END-IF.

			IF RECORDFOUND
			   MOVE BASEBALLID          TO PRINT-ID
			   MOVE IDX-HOMETEAM        TO PRINT-HOMETEAM
			   DISPLAY PRINTRECORD
			END-IF.
			
			
		   PERFORM 9000-END-PROGRAM.
		   
	   0100-END.
	   
	 
	   9000-END-PROGRAM.
           CLOSE BASEBALL.    	   
		   EVALUATE BASEBALL-FS
            WHEN '00'
               CONTINUE
            WHEN OTHER
               DISPLAY '********************'
               DISPLAY '* CLOSE FILE ERROR  *'
               DISPLAY '* FS-INPUT: ' BASEBALL-FS
               DISPLAY '********************'
           END-EVALUATE

           STOP RUN.
           
          END PROGRAM READINDEXFILE.
