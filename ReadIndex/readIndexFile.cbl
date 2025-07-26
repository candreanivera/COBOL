       IDENTIFICATION DIVISION.
       PROGRAM-ID. READINDEXFILE.
      * READS AN INDEXED FILE USING EITHER IDGAME, HOMETEAM OR
      * AWAYTEAM 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   
	    SELECT BASEBALL ASSIGN TO "BASEBALLINDEXED.DAT"
        FILE STATUS IS BASEBALL-FS
		  ORGANIZATION IS INDEXED
		  ACCESS MODE IS DYNAMIC
		  RECORD KEY IS BASEBALLID
		  ALTERNATE RECORD KEY IS IDX-HOMETEAM
		     WITH DUPLICATES
        ALTERNATE RECORD KEY IS IDX-AWAYTEAM
		     WITH DUPLICATES.
		   
       DATA DIVISION.
       FILE SECTION.
	   FD BASEBALL.
	   01 BASE-IDX-REG.
         88 BASEBALL-EOF            VALUE HIGH-VALUES.
	      05 BASEBALLID              PIC X(36).
		   05 IDX-YEAR                PIC 9(04).
         05 IDX-DATE                PIC X(10).
         05 IDX-TIME                PIC X(13).
         05 IDX-ATTENDANCE          PIC X(05).
         05 IDX-HOMETEAM            PIC X(12).
         05 IDX-AWAYTEAM            PIC X(12).
         05 IDX-MOREINFO            PIC X(35).
		
       WORKING-STORAGE SECTION.
       01  WS-WORKING-STORAGE.
           05 FILLER                PIC X(27) VALUE 
		      'WORKING STORAGE STARTS HERE'.    
   
	   01  WS-WORK-AREAS.
	       05  BASEBALL-FS           PIC X(2).
		       88 RECORDFOUND         VALUE "00".
			   
      *lEVEL 88 NOT USABLE ON EVALUATE CLAUSES
		   05  READTYPE               PIC 9.
		       88 ID-KEY              VALUE 1.
			    88 HOMETEAM-KEY        VALUE 2.
             88 ALLRECORDS          VALUE 3.
			   
	       05  PRINTRECORD.
             10 PRINT-ID            PIC X(36).
             10 FILLER              PIC X(02).
             10 PRINT-HOMETEAM      PIC X(12).
             10 FILLER              PIC X(02).
             10 PRINT-AWAYTEAM      PIC X(12).

          05 WORK-VARIABLES.
             10 WS-HOME-TEAM        PIC X(12).
             10 WS-AWAY-TEAM        PIC X(12).
             10 WS-CLOSE-FILE       PIC X(03) VALUE 'YES'.

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
               MOVE 'NO '     TO WS-CLOSE-FILE
               PERFORM 9000-END-PROGRAM
           END-EVALUATE

		   DISPLAY "TO SELECT RECORD BY GAME ID, ENTER 1". 
		   DISPLAY "TO SELECT RECORD BY HOME TEAM, ENTER 2".
         DISPLAY "TO SELECT RECORD BY AWAY TEAM, ENTER 3".
         DISPLAY "TO SELECT ALL THE RECORDS, ENTER 4"
			ACCEPT READTYPE.

		   EVALUATE READTYPE
		   WHEN 1 
		     DISPLAY "ENTER GAME ID (36 DIGITS): " 
      *    allows to recibe the user input on the same line
			  WITH NO ADVANCING		    
			  ACCEPT BASEBALLID
			  READ BASEBALL
      * Reading by record key, no duplicates
			    KEY IS BASEBALLID
			    INVALID KEY DISPLAY "ERROR ON KEY: ",
				  BASEBALL-FS
			  END-READ		
           PERFORM 0200-DISPLAY	 	
         WHEN 2
		      DISPLAY "ENTER HOME TEAM (15 CHARACTERS): " 
      *    allows to recibe the user input on the same line
			    WITH NO ADVANCING
			  ACCEPT IDX-HOMETEAM
              READ BASEBALL
                KEY IS IDX-HOMETEAM			  
                INVALID KEY DISPLAY "ERROR ON KEY: " BASEBALL-FS
              END-READ
              MOVE IDX-HOMETEAM        TO WS-HOME-TEAM
              PERFORM 0200-DISPLAY
              PERFORM 0300-READ-NEXT UNTIL BASEBALL-EOF
         WHEN 3
           DISPLAY "ENTER AWAY TEAM (15 CHARACTERS): " 
      *    allows to recibe the user input on the same line
			    WITH NO ADVANCING
			  ACCEPT IDX-AWAYTEAM
              READ BASEBALL
                KEY IS IDX-AWAYTEAM			  
                INVALID KEY DISPLAY "ERROR ON KEY: " BASEBALL-FS
              END-READ
              MOVE IDX-AWAYTEAM        TO WS-AWAY-TEAM
              PERFORM 0200-DISPLAY
              PERFORM 0300-READ-NEXT UNTIL BASEBALL-EOF
         WHEN 4
              MOVE SPACES              TO IDX-HOMETEAM
              START BASEBALL
                   KEY >= IDX-HOMETEAM	
                   INVALID KEY DISPLAY "ERROR ON KEY: " BASEBALL-FS
              END-START
              PERFORM 0200-DISPLAY
              PERFORM 0300-READ-NEXT UNTIL BASEBALL-EOF
         WHEN OTHER
           DISPLAY "ERROR, OPTION NOT VALID"
         END-EVALUATE.

         PERFORM 9000-END-PROGRAM.

       0200-DISPLAY.

       IF  READTYPE = 1 OR
          (READTYPE = 2 AND IDX-HOMETEAM = WS-HOME-TEAM) OR
          (READTYPE = 3 AND IDX-AWAYTEAM = WS-AWAY-TEAM) OR
           READTYPE = 4
		     MOVE BASEBALLID          TO PRINT-ID
		     MOVE IDX-HOMETEAM        TO PRINT-HOMETEAM
           MOVE IDX-AWAYTEAM        TO PRINT-AWAYTEAM
		     DISPLAY PRINTRECORD
	    END-IF.

	   
       0300-READ-NEXT.

           READ BASEBALL NEXT RECORD
            AT END SET BASEBALL-EOF    TO TRUE
           END-READ.
           PERFORM 0200-DISPLAY.
	 
	    9000-END-PROGRAM.

         IF WS-CLOSE-FILE = 'YES'
           CLOSE BASEBALL  	   
		      EVALUATE BASEBALL-FS
              WHEN '00'
                 CONTINUE
              WHEN OTHER
                 DISPLAY '********************'
                 DISPLAY '* CLOSE FILE ERROR  *'
                 DISPLAY '* FS-INPUT: ' BASEBALL-FS
                 DISPLAY '********************'
             END-EVALUATE
         END-IF
           
           STOP RUN.
           
       END PROGRAM READINDEXFILE.
