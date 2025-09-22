SCREEN 7
RANDOMIZE TIMER
PAINT (0, 0), 2
DIM s3&(250)
DIM s4&(250)
OPEN "C:\WINDOWS\Desktop\qb45\kenslugl.c44" FOR INPUT AS #1
FOR j = 0 TO 250
INPUT #1, s3&(j), s4&(j)
NEXT
CLOSE #1
DIM s1&(250)
DIM s2&(250)
OPEN "C:\WINDOWS\Desktop\qb45\kenslugr.c44" FOR INPUT AS #1
FOR j = 0 TO 250
INPUT #1, s1&(j), s2&(j)
NEXT
CLOSE #1
lauf:
y = CINT(RND * 150)
FOR j = 0 TO 270 STEP 3
IF INKEY$ <> "" THEN SYSTEM
IF j MOD 2 = 0 THEN PUT (j, y + 1), s1&, PSET
IF j MOD 2 = 1 THEN PUT (j, y), s2&, PSET
FOR k = 0 TO 2000: NEXT
NEXT
LINE (j, y)-(j + 40, y + 41), 2, BF

lauf2:
y = CINT(RND * 150)
FOR j = 270 TO 0 STEP -3
IF INKEY$ <> "" THEN END
IF j MOD 2 = 0 THEN PUT (j, y + 1), s3&, PSET
IF j MOD 2 = 1 THEN PUT (j, y), s4&, PSET
FOR k = 0 TO 2000: NEXT
NEXT
LINE (j, y)-(j + 40, y + 41), 2, BF
GOTO lauf


