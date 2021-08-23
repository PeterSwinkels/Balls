DEFINT A-Z

TYPE BallStr
 DirectionX AS INTEGER
 DirectionY AS INTEGER
 X AS INTEGER
 Y AS INTEGER
END TYPE

TYPE PlateauStr
 DirectionX AS INTEGER
 X AS INTEGER
END TYPE

TYPE PlayerStr
 Level AS INTEGER
 LevelStartScore AS INTEGER
 Lives AS INTEGER
 NameV AS STRING * 15
 Score AS INTEGER
END TYPE

TYPE ScoreStr
 DateV AS STRING * 10
 NameV AS STRING * 15
 Score AS STRING * 12
 TimeV AS STRING * 8
END TYPE

DECLARE FUNCTION Choose$ (X, Y, Prompt$, Choices$)
DECLARE FUNCTION GetInput$ (X, Y, Prompt$, MaximumLength)
DECLARE SUB AddScore ()
DECLARE SUB AskPlayAgain ()
DECLARE SUB BallHitPlateau ()
DECLARE SUB BallMissedPlateau ()
DECLARE SUB Bonus ()
DECLARE SUB Center (Row, Text$)
DECLARE SUB DisplayScores ()
DECLARE SUB DisplayStatusBar ()
DECLARE SUB DisplayTitle ()
DECLARE SUB DisplayYouLost ()
DECLARE SUB DisplayYouWon ()
DECLARE SUB GenerateDelay (MicroSeconds&)
DECLARE SUB GetNameAndLevel ()
DECLARE SUB GoToNextLevel ()
DECLARE SUB Initialize ()
DECLARE SUB LoadScores ()
DECLARE SUB Main ()
DECLARE SUB MoveBall ()
DECLARE SUB MovePlateau ()
DECLARE SUB PlayBounceSound ()
DECLARE SUB Quit ()
DECLARE SUB SaveScores ()
DECLARE SUB SortScores ()
OPTION BASE 0

ON ERROR GOTO ErrorTrap
DIM SHARED Ball AS BallStr
DIM SHARED Delay$
DIM SHARED DelayStart(0 TO 1) AS SINGLE
DIM SHARED Image(0 TO 1300) AS DOUBLE
DIM SHARED Plateau AS PlateauStr
DIM SHARED Player AS PlayerStr
DIM SHARED ScreenModeSet
DIM SHARED Scores(0 TO 10) AS ScoreStr

Initialize
DisplayTitle
DisplayScores
GetNameAndLevel
CALL Main

ErrorTrap:
IF ScreenModeSet THEN GET (120, 160)-(520, 210), Image
ErrorCode = ERR
COLOR 4: LOCATE 11
SELECT CASE ErrorCode
 CASE 7, 14
  Center 0, "Not enough memory."
 CASE 55
  Center 0, "Cannot open score list."
 CASE 61
  Center 0, "Disk is full."
 CASE 62
  Center 0, "Error while reading score list."
 CASE 70
  Center 0, "Disk or score list"
  Center 0, "is write protected."
 CASE 71
  Center 0, "There is no disk in drive."
 CASE 72
  Center 0, "Disk is damaged."
 CASE ELSE
  IF ScreenModeSet THEN
   Center 0, "Unexpected error."
  ELSE
   Center 0, "VGA required."
  END IF
END SELECT
COLOR 12
Center 0, "1 = Try Again  2 = Ignore  3 = Quit"
DO
 Choice$ = INPUT$(1)
LOOP UNTIL INSTR("123", Choice$)
IF ScreenModeSet THEN PUT (120, 160), Image, PSET
SELECT CASE Choice$
 CASE "1": RESUME
 CASE "2": RESUME NEXT
 CASE "3": Quit
END SELECT

SUB AddScore
 LoadScores

 Scores(UBOUND(Scores)).DateV = DATE$
 Scores(UBOUND(Scores)).NameV = Player.NameV
 Scores(UBOUND(Scores)).Score$ = LTRIM$(STR$(Player.Score))
 Scores(UBOUND(Scores)).TimeV = TIME$

 SortScores
 SaveScores
END SUB

SUB AskPlayAgain
 IF Choose$(8, 10, "Play again y/n?", "YN") = "N" THEN Quit
 RUN "Balls"
END SUB

SUB BallHitPlateau
 IF NOT Plateau.DirectionX = 0 THEN Ball.DirectionX = -Plateau.DirectionX

 IF NOT Ball.DirectionX = 0 THEN
  Player.Score = Player.Score + 1
  IF Player.Score = Player.LevelStartScore + (Player.Level * 5) THEN GoToNextLevel
  DisplayStatusBar
 END IF
END SUB

SUB BallMissedPlateau
 SOUND 37, 10
 Player.Lives = Player.Lives - 1
 IF Player.Lives = 0 THEN DisplayYouLost
 DisplayStatusBar
END SUB

SUB Bonus
 COLOR 4
 DelayStart(0) = TIMER
 DO
  LOCATE 3
  PRINT SPC(10); "ÛßßßÜ ÜßßßÜ ÛÜ  Û Û   Û Üßßßß Û"
  PRINT SPC(10); "ÛÜÜÜß Û   Û Û Û Û Û   Û ßÜÜÜ  Û"
  PRINT SPC(10); "Û   Û Û   Û Û ßÜÛ Û   Û     Û ß"
  PRINT SPC(10); "ßßßß   ßßß  ß   ß  ßßß  ßßßß  ß"
  PRINT SPC(10); "Score is: "; Player.Score; ", score becomes: "; Player.Score + (5 * Player.Lives)
  DelayStart(1) = TIMER: DO: LOOP UNTIL TIMER > DelayStart(1) + .2 OR TIMER < 3
  CLS
  DelayStart(1) = TIMER: DO: LOOP UNTIL TIMER > DelayStart(1) + .1 OR TIMER < 3
 LOOP UNTIL TIMER > DelayStart(0) + 2 OR TIMER < 3
 Player.Score = Player.Score + (5 * Player.Lives)
 Player.LevelStartScore = Player.Score
END SUB

SUB Center (Row, Text$)
 IF Row > 0 THEN LOCATE Row
 LOCATE , INT(40 - (LEN(Text$) / 2))
 PRINT Text$
END SUB

FUNCTION Choose$ (X, Y, Prompt$, Choices$)
 LOCATE X, Y: PRINT Prompt$
 DO
  Choice$ = UCASE$(INPUT$(1))
 LOOP UNTIL INSTR(Choices$, Choice$)
 LOCATE X, Y: PRINT SPC(LEN(Prompt$));
 Choose$ = Choice$
END FUNCTION

SUB DisplayScores
 COLOR 9: PRINT "     *** Score List ***"

 FileH = FREEFILE
 OPEN "Scores.lst" FOR BINARY LOCK READ WRITE AS FileH
  IF LOF(FileH) = 0 THEN
   PRINT "There are no scores."
  ELSE
   DO UNTIL LOC(FileH) = LOF(FileH)
    Length = ASC(INPUT$(1, FileH)): ScoreName$ = INPUT$(Length, FileH)
    Length = ASC(INPUT$(1, FileH)): Score$ = INPUT$(Length, FileH)
    Length = ASC(INPUT$(1, FileH)): ScoreTime$ = INPUT$(Length, FileH)
    Length = ASC(INPUT$(1, FileH)): ScoreDate$ = INPUT$(Length, FileH)
    IF INSTR(ScoreName$, CHR$(0)) > 0 THEN ScoreName$ = LEFT$(ScoreName$, INSTR(ScoreName$, CHR$(0)) - 1)
    IF RTRIM$(ScoreName$) = "" THEN EXIT DO
    PRINT USING "\             \  Score: \ \  \        \  \        \"; ScoreName$; Score$; ScoreTime$; ScoreDate$
   LOOP
  END IF
 CLOSE FileH
END SUB

SUB DisplayStatusBar
 COLOR 9
 LOCATE 28, 3
 PRINT USING "Score: ###  Necessary score for next level: ###  "; Player.Score; Player.LevelStartScore + (Player.Level * 5);
 PRINT USING "  Level: ###  Lives: ###"; Player.Level; Player.Lives
END SUB

SUB DisplayTitle
 COLOR 2: LOCATE 2
 PRINT "ÛßßßÜ ÜßßßÜ Û    Û    Üßßß        Balls,       "
 PRINT "ÛßßßÜ ÛßßßÛ Û    Û     ßßÜ  by: Peter Swinkels "
 PRINT "ßßßß  ß   ß ßßßß ßßßß ßßß       ***2021***     "
END SUB

SUB DisplayYouLost
 FOR Frequency = 200 TO 100 STEP -20
  SOUND Frequency, 1
 NEXT Frequency
 COLOR 4

 DelayStart(0) = TIMER
 DO
  CLS
  DelayStart(1) = TIMER: DO: LOOP UNTIL TIMER > DelayStart(1) + .1 OR TIMER < 3
  LOCATE 3
  PRINT SPC(10); "Û   Û ÜßßßÜ Û   Û  Û    ÜßßßÜ Üßßßß ßßÛßß Û"
  PRINT SPC(10); "ßÜÜÜÛ Û   Û Û   Û  Û    Û   Û ßÜÜÜ    Û   Û"
  PRINT SPC(10); "    Û Û   Û Û   Û  Û    Û   Û     Û   Û   ß"
  PRINT SPC(10); "ßßßß   ßßß   ßßß   ßßßß  ßßß  ßßßß    ß   ß"
  DelayStart(1) = TIMER: DO: LOOP UNTIL TIMER > DelayStart(1) + .2 OR TIMER < 3
 LOOP UNTIL TIMER > DelayStart(0) + 2 OR TIMER < 3

 AddScore
 AskPlayAgain
END SUB

SUB DisplayYouWon
 FOR Frequency = 100 TO 200 STEP 20
  SOUND Frequency, 1
 NEXT Frequency
 COLOR 4

 DelayStart(0) = TIMER
 DO
  CLS
  DelayStart(1) = TIMER: DO: LOOP UNTIL TIMER > DelayStart(1) + .1 OR TIMER < 3
  LOCATE 3
  PRINT SPC(10); "Û   Û ÜßßßÜ Û   Û  Û   Ü   Û ÜßßßÜ ÛÜ  Û Û"
  PRINT SPC(10); "ßÜÜÜÛ Û   Û Û   Û  ßÜ ÜßÜ Üß Û   Û Û Û Û Û"
  PRINT SPC(10); "    Û Û   Û Û   Û   Û Û Û Û  Û   Û Û ßÜÛ ß"
  PRINT SPC(10); "ßßßß   ßßß   ßßß     ß   ß    ßßß  ß   ß ß"
  DelayStart(1) = TIMER: DO: LOOP UNTIL TIMER > DelayStart(1) + .2 OR TIMER < 3
 LOOP UNTIL TIMER > DelayStart(0) + 2 OR TIMER < 3

 AddScore
 AskPlayAgain
END SUB

SUB GenerateDelay (MicroSeconds&)
 Delay$ = CHR$(&HB4) + CHR$(&H86)                               'MOV AH, 0x86
 Delay$ = Delay$ + CHR$(&HB9) + MKI$(MicroSeconds& / &H10000)   'MOV CX, HighByte
 Delay$ = Delay$ + CHR$(&HBA) + MKI$(MicroSeconds& AND &HFFFF)  'MOV DX, LowByte
 Delay$ = Delay$ + CHR$(&HCD) + CHR$(&H15)                      'INT 0x15
 Delay$ = Delay$ + CHR$(&HCB)                                   'RETF
END SUB

FUNCTION GetInput$ (X, Y, Prompt$, MaximumLength)
 DO
  LOCATE X, Y: PRINT Prompt$; Text$; "_ "
  DO
   Key$ = INKEY$
  LOOP WHILE Key$ = ""
  Length = LEN(Text$)
  SELECT CASE Key$
   CASE CHR$(8)
    IF Length > 0 THEN Text$ = LEFT$(Text$, Length - 1)
   CASE CHR$(13)
    EXIT DO
   CASE CHR$(27)
    Text$ = ""
    EXIT DO
   CASE IS >= " "
    IF Length < MaximumLength THEN Text$ = Text$ + Key$
  END SELECT
 LOOP

 LOCATE X, Y + (LEN(Prompt$) + Length): PRINT " ";

 GetInput$ = Text$
END FUNCTION

SUB GetNameAndLevel
 Text$ = GetInput$(16, 1, "Name: ", 15)
 IF Text$ = "" THEN Player.NameV = "no name" ELSE Player.NameV = Text$
 Player.Level = VAL(Choose$(17, 1, "Level (1-9):", "123456789"))
 GenerateDelay ((10 - Player.Level) * 300)
END SUB

SUB GoToNextLevel
 FOR Frequency = 100 TO 200 STEP 10
  SOUND Frequency, 1
 NEXT Frequency

 IF Player.Lives > 1 THEN Bonus
 IF Player.Level = 9 THEN DisplayYouWon
 Player.Level = Player.Level + 1
 GenerateDelay ((10 - Player.Level) * 300)

 DisplayStatusBar
END SUB

SUB Initialize
 ScreenModeSet = 0
 CLS
 SCREEN 12: WIDTH 80, 30: COLOR 15: CLS
 ScreenModeSet = -1

 RANDOMIZE TIMER
 Ball.DirectionX = CINT(RND * -2) + 1
 Ball.DirectionY = 1
 Ball.X = INT(RND * 589) + 50
 Player.Lives = 5
 Plateau.X = 270
 Plateau.DirectionX = 0
 Player.Score = 0
 Player.LevelStartScore = 0
END SUB

SUB LoadScores
 ERASE Scores

 FileH = FREEFILE
 OPEN "Scores.lst" FOR BINARY LOCK READ WRITE AS FileH
  FOR Index = LBOUND(Scores) TO UBOUND(Scores) - 1
   IF LOC(FileH) = LOF(FileH) THEN EXIT FOR
   Length = ASC(INPUT$(1, FileH)): Scores(Index).NameV = INPUT$(Length, FileH)
   Length = ASC(INPUT$(1, FileH)): Scores(Index).Score = INPUT$(Length, FileH)
   Length = ASC(INPUT$(1, FileH)): Scores(Index).TimeV = INPUT$(Length, FileH)
   Length = ASC(INPUT$(1, FileH)): Scores(Index).DateV = INPUT$(Length, FileH)
  NEXT Index
 CLOSE FileH
END SUB

SUB Main
 DEF SEG = VARSEG(Delay$)
 CLS
 DisplayStatusBar
 DO
  DO
   MoveBall
   MovePlateau
   CALL Absolute(SADD(Delay$))
   Key$ = INKEY$
  LOOP WHILE Key$ = ""
  SELECT CASE Key$
   CASE CHR$(0) + "K"
    Plateau.DirectionX = -1
   CASE CHR$(0) + "M"
    Plateau.DirectionX = 1
   CASE CHR$(27)
    CLS
    IF Choose$(3, 3, "Quit y/n?", "YN") = "Y" THEN Quit
    DisplayStatusBar
   CASE "P", "p"
    Center 10, "Game paused, press any key to continue."
    DO: LOOP WHILE INKEY$ = ""
    LOCATE 10, 1: PRINT SPACE$(80)
  END SELECT
 LOOP
END SUB

SUB MoveBall
 LINE (Ball.X - 8, Ball.Y - 8)-STEP(16, 16), 0, BF

 IF Ball.DirectionY = -1 THEN
  IF Ball.Y < 11 THEN Ball.DirectionY = 1: PlayBounceSound ELSE Ball.Y = Ball.Y - 1
 ELSEIF Ball.DirectionY = 1 THEN
  IF Ball.Y > 417 THEN
   IF Ball.X > Plateau.X - 11 AND Ball.X < Plateau.X + 111 THEN
    BallHitPlateau
   ELSE
    BallMissedPlateau
   END IF
  
   Ball.DirectionY = -1
   PlayBounceSound
  ELSE
   Ball.Y = Ball.Y + 1
  END IF
 END IF

 IF Ball.DirectionX = -1 THEN
  IF Ball.X < 11 THEN Ball.DirectionX = 1: PlayBounceSound ELSE Ball.X = Ball.X - 1
 ELSEIF Ball.DirectionX = 1 THEN
  IF Ball.X > 628 THEN Ball.DirectionX = -1: PlayBounceSound ELSE Ball.X = Ball.X + 1
 END IF

 CIRCLE (Ball.X, Ball.Y), 7, 2
 PAINT (Ball.X, Ball.Y), 2, 2
END SUB

SUB MovePlateau
 SELECT CASE Plateau.DirectionX
  CASE -1
   IF Plateau.X > 0 THEN
    LINE (Plateau.X, 410)-STEP(100, 17), 0, BF
    Plateau.X = Plateau.X - 1
   END IF
  CASE 1
   IF Plateau.X < 539 THEN
    LINE (Plateau.X, 410)-STEP(100, 17), 0, BF
    Plateau.X = Plateau.X + 1
   END IF
 END SELECT

 LINE (Plateau.X, 410)-STEP(100, 17), 4, BF
END SUB

SUB PlayBounceSound
 SOUND 200, 1
 SOUND 300, 1
END SUB

SUB Quit
 SCREEN 0
 COLOR 7, 0
 CLS
 WIDTH 80, 25
 SYSTEM
END SUB

SUB SaveScores
 FileH = FREEFILE
 OPEN "Scores.lst" FOR OUTPUT LOCK READ WRITE AS FileH
  FOR Index = LBOUND(Scores) TO UBOUND(Scores) - 1
   IF NOT Scores(Index).NameV = "" THEN
    PRINT #FileH, CHR$(LEN(Scores(Index).NameV)); : PRINT #FileH, Scores(Index).NameV;
    PRINT #FileH, CHR$(LEN(Scores(Index).Score)); : PRINT #FileH, Scores(Index).Score;
    PRINT #FileH, CHR$(LEN(Scores(Index).TimeV)); : PRINT #FileH, Scores(Index).TimeV;
    PRINT #FileH, CHR$(LEN(Scores(Index).DateV)); : PRINT #FileH, Scores(Index).DateV;
   END IF
  NEXT Index
 CLOSE FileH
END SUB

SUB SortScores
 FOR Score1 = LBOUND(Scores) TO UBOUND(Scores)
  FOR Score2 = LBOUND(Scores) TO UBOUND(Scores)
   IF VAL(Scores(Score1).Score) > VAL(Scores(Score2).Score) THEN
    SWAP Scores(Score1), Scores(Score2)
   END IF
  NEXT Score2
 NEXT Score1
END SUB

