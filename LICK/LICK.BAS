SCREEN 7
RANDOMIZE TIMER
PAINT (0, 0), 2
DIM l1&(250)
DIM l2&(250)
DIM l3&(250)
DIM l4&(250)
OPEN "c:\WINDOWS\Desktop\qb45\keenlick.c4-" FOR INPUT AS #1
FOR j = 0 TO 250
INPUT #1, l1&(j), l2&(j), l3&(j), l4&(j)
NEXT
los:
sy = CINT(RND * 100 + 20)
FOR j = 0 TO 240 STEP 30
y = sy
yr = 5
x = j
DO
IF INKEY$ <> "" THEN SYSTEM
PUT (x, y), l2&, PSET
LINE (x - 3, y)-(x - 1, y + 45), 2, BF
y = y - yr
yr = yr - 1
x = x + 3
FOR i = 0 TO 900: NEXT
PAINT (x + 2, y + 2), 2
LOOP UNTIL y > sy - 1
PUT (j + 30, y + 3), l3&, PSET
FOR i = 0 TO 900: NEXT
IF x > 110 AND x < 130 THEN GOSUB fire
NEXT
LINE (x - 5, sy + 3)-(x + 40, sy + 43), 2, BF
GOTO los:

fire:
FOR c = 0 TO 250
IF c MOD 2 = 0 THEN PUT (x, y), l1&, PSET
IF c MOD 2 = 1 THEN PUT (x, y), l4&, PSET
NEXT
RETURN

