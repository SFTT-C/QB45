'READ FIRST!!
'Under the Declares is Important changes to be made to accomodate your system.

DECLARE SUB GorauFill (Points() AS ANY)
DECLARE SUB Delay (Seconds!)
DECLARE SUB ChangeColors ()
DECLARE FUNCTION ValidPolPoints! ()
DECLARE SUB help ()
DECLARE SUB quit ()
DECLARE SUB ocenter (text$, y%, Fore%, back%)
DECLARE SUB menu (sx!(), sy!(), cl!(), outcome)
DECLARE SUB start (sx!(), sy!(), cl!(), outcome)
DECLARE SUB explode (green%, Blue%, red%, boomX!, boomY!, boomSize!, comarcs!, son!, ax, ay, noarc)
DECLARE SUB ofont (text$, X%, y%, Fore%, back%)
DECLARE SUB sbplay (ply$, length!)
DECLARE FUNCTION DetectCard% ()
DECLARE SUB SB ()
DECLARE SUB SBINIT ()
DECLARE SUB sbplay1 (Freq%, Wave%, Feedback%, Modl%, Clen%)
DECLARE SUB WriteReg (Reg%, Value%)


'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''''''' LOWER the 15 to go faster or add more to go slower  '''''''''''''''''''''
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
spd = 15 / 100
'*Warning* you may have to change the time - spd ratio. search for 53.333
' Ajust that number to reach a comforatble time -spd ratio
'It works fine on my PENTIUM 200 at 53.33333334

''''''''''''''''Change Below to where the files are!''''''''''''''''''''
CHDIR "c:\WINDOWS\Desktop\qb45"
''''''''''''''''''''''''''''''''''''''''''''''



TYPE arctype
  radius AS INTEGER
  colr AS INTEGER
  p1 AS SINGLE
  p2 AS SINGLE
END TYPE

TYPE GorType
 Y1 AS INTEGER
 Y2 AS INTEGER
 C1 AS INTEGER
 C2 AS INTEGER
END TYPE

TYPE RoutineType
 X AS INTEGER
 y AS INTEGER
 C AS INTEGER
END TYPE

DIM SHARED PolPoints(0 TO 3) AS RoutineType

CONST pi = 3.141592653589#

DIM SHARED son
DIM ybk(8000)
 RANDOMIZE 1
IF death = 0 THEN IF DetectCard% = false THEN son = 0:  ELSE son = 1
IF son = 1 THEN SB
IF son = 1 THEN SBINIT
SCREEN 13
B = 0
FOR a = 1 TO 100
B = B + .5
PALETTE a, 65536 * INT(B) + 2 + 256 * (INT(B) + 1) + INT(B)
NEXT
PALETTE 254, 65536 * 0 + 256 * 63 + 63

B = 0
FOR a = 101 TO 201
B = B + .5
PALETTE a, 65536 * 0 + 256 * (INT(B) + 1) + 0
NEXT
FOR a = 100 TO 1 STEP -1
CIRCLE (150 - B, 150 - B), a * 1.4, a
PAINT (150 - B, 150 - B), a

NEXT
CIRCLE (100 - B, 100 - B), 51, 0
PAINT (200, 100), 0
B = -.1
FOR a = 10 TO 1 STEP -1
B = B + .1
CIRCLE (72 - B, 72 - B), a, 20 + a
PAINT (72 - B, 72 - B), 20 + a
NEXT
DIM deathstar(9000) AS INTEGER
GET (1, 1)-(101, 101), deathstar
CLS
FOR a = 80 TO 1 STEP -1
CIRCLE (140 - B, 140 - B), a * 1.4, (a * 1.25) + 101
PAINT (140 - B, 140 - B), (a * 1.25) + 101

NEXT
CIRCLE (100 - B, 100 - B), 41, 0
PAINT (200, 100), 0
DIM planet(9000) AS INTEGER
GET (50, 55)-(151, 140), planet
PUT (200, 50), planet
CLS
DIM sx(150)
DIM msx(150)
DIM sy(150)
DIM msy(150)
DIM cl(150)
FOR a = 1 TO 150
sx(a) = INT(RND * 419) + 1
sy(a) = INT(RND * 199) + 1
cl(a) = INT(RND * 5) + 1
msx(a) = sx(a): msy(a) = sy(a)
PSET (sx(a), sy(a)), cl(a) * 10
NEXT
'''''''
'Start'
'''''''
CALL start(sx(), sy(), cl(), outcome)
outcome1:
IF outcome = 1 THEN GOTO game
IF outcome = 3 THEN CALL help: GOTO menu
IF outcome = 4 THEN CALL quit
CLS
FOR a = 1 TO 150
PSET (sx(a), sy(a)), cl(a) * 10
NEXT

COLOR 100
LOCATE 20, 1: PRINT "Sir, We are nearing the"
LOCATE 21, 1: PRINT "Rebal planet."
FOR a = 1 TO 100
IF INKEY$ <> "" THEN GOTO menu
FOR aa = 1 TO 150
IF POINT(sx(aa), sy(aa)) = 0 THEN
  PSET (sx(aa), sy(aa)), cl(aa) * 10
END IF
NEXT
LINE (a - 1, 0)-(a - 1, 101), 0
PUT (a, 0), deathstar, PSET
IF son = 1 THEN sbplay1 1, 1, 3, 0, 0

tim = TIMER + .05
WHILE TIMER < tim: WEND
IF son = 1 THEN sbplay1 2, 1, 3, 0, 0
NEXT
CLS
FOR a = 100 TO 0 STEP -1
FOR aa = 1 TO 150
sx(aa) = sx(aa) - 1
IF POINT(sx(aa), sy(aa)) = 0 THEN
  PSET (sx(aa) + 1, sy(aa)), 0
   PSET (sx(aa), sy(aa)), cl(aa) * 10
END IF
NEXT
LINE (0, 0)-(0, 199), 0
LINE (a + 102, 0)-(a + 102, 101), 0, BF
PUT (a, 0), deathstar, PSET
LINE (a + 110 + 102, 90)-(a + 110 + 102, 90 + 85), 0, BF
PUT (a + 110, 90), planet, PSET
IF son = 1 THEN sbplay1 1, 1, 3, 0, 0
tim = TIMER + .04
WHILE TIMER < tim: WEND
IF son = 1 THEN sbplay1 2, 1, 3, 0, 0
NEXT
COLOR 100
  LOCATE 4, 20: PRINT "Rebal X-wings"
  LOCATE 5, 20: PRINT "attacking "
  tim = TIMER + 1
  WHILE TIMER < tim: WEND
IF son = 1 THEN CALL WriteReg(&HB0, &H0)
  tim = TIMER + 1
  WHILE TIMER < tim: WEND
  GOTO menu

112
CLS
FOR a = 1 TO 100
B = B + .5
PALETTE a, 65536 * INT(B) + 2 + 256 * (INT(B) + 1) + INT(B)
NEXT
FOR a = 1 TO 150:
PSET (sx(a), sy(a)), cl(a) * 10
NEXT
a = 0
PUT (a, 0), deathstar, PSET
LINE (a + 110 + 102, 90)-(a + 110 + 102, 90 + 85), 0, BF
PUT (a + 110, 90), planet, PSET

COLOR 100
LOCATE 5, 20: PRINT "Starting"
LOCATE 6, 20: PRINT "Primary Ingition"
1 'primary ingition
IF son = 1 THEN CALL WriteReg(&HB0, &H0)
FOR a = 50 TO 25 STEP -1
IF son = 1 THEN sbplay1 a + .001, 1, 3, 0, 0
tim = TIMER + .03
WHILE TIMER < tim
WEND
NEXT
tim = TIMER + 2
WHILE TIMER < tim: WEND
FOR a = 24 TO 0 STEP -1
IF son = 1 THEN sbplay1 a + .001, 1, 3, 0, 0
tim = TIMER + .03
WHILE TIMER < tim: WEND
NEXT
IF son = 1 THEN CALL WriteReg(&HB0, &H0)
2
COLOR 100
LOCATE 7, 20: PRINT "FIRING"
LINE (a + 72, 0 + 72)-(a + 150, 150), 220
LINE (a + 72, 0 + 72)-(a + 170, 140), 220
LINE (a + 150, 150)-(a + 170, 140), 220
PAINT (a + 163, 140), 220
tim = TIMER + 1
WHILE TIMER < tim
PALETTE 220, 65536 * 0 + 256 * (58 + INT(RND * 10) - 5) + (58 + INT(RND * 10) - 5)
WEND
explode 10, 10, 10, 163, 130, 40, 63, son, 40, 40, 0
CLS
FOR aa = 1 TO 1050
d = INT(RND * 360)
X = COS(d)
y = SIN(d)
e = INT(RND * 50) + 1
PSET (163 + (X * e), 130 + (y * e)), (INT(RND * 50) + 10) * 1.25 + 101
NEXT
FOR aa = 1 TO 150
PSET (sx(aa), sy(aa)), cl(aa) * 10
NEXT

PUT (0, 0), deathstar, PSET
LOCATE 5, 20
COLOR 100
LOCATE 7, 20: PRINT "Rebel planet"
LOCATE 8, 20: PRINT "destroyed"
WHILE INKEY$ <> " ": WEND
GOTO menu

113
CLS
FOR a = 1 TO 100
B = B + .5
PALETTE a, 65536 * INT(B) + 2 + 256 * (INT(B) + 1) + INT(B)
NEXT
FOR aa = 1 TO 150
PSET (sx(aa), sy(aa)), cl(aa) * 10
NEXT
a = 0
PUT (a, 0), deathstar, PSET
LINE (a + 110 + 102, 90)-(a + 110 + 102, 90 + 85), 0, BF
PUT (a + 110, 90), planet, PSET
FOR a = 1 TO 15
CALL explode(10, 10, 10, 50, 50, 3, 33, son, 80, 80, 0)
PUT (0, 0), deathstar, PSET
NEXT
a = 0
CALL explode(10, 10, 10, 50, 50, 40, 63, son, 80, 80, 0)
LINE (0, 0)-(100, 100), 0, BF
FOR aa = 1 TO 1050
d = INT(RND * 360)
X = COS(d)
y = SIN(d)
e = INT(RND * 50) + 1
PSET (50 + (X * e), 50 + (y * e)), INT(RND * 100)
NEXT
tim13 = TIMER + 1
WHILE TIMER < tim13: WEND
B = 50
FOR a = 100 TO 320
B = B - 1 + INT(RND * 3)
IF B <= 1 THEN B = B + 1
IF B >= 100 THEN B = B - 1
LINE (a - 2, B - 2)-(a + 2, B + 2), 0, BF
FOR aa = 1 TO 150
IF sy(aa) < 90 THEN PSET (sx(aa), sy(aa)), cl(aa) * 10
NEXT
PUT (110, 90), planet, PSET

PSET (a, B), 100
tim14 = TIMER + .1
WHILE TIMER < tim14: WEND
NEXT
tim13 = TIMER + 1
WHILE TIMER < tim13: WEND
CLS
COLOR 25
FOR aa = 1 TO 150
PSET (sx(aa), sy(aa)), cl(aa) * 10
NEXT
PRINT "The Deathstar is destroyed and the"
PRINT "Empire has fallen. The Rebel Alliance"
PRINT "cheers and awards are given to you "
PRINT "the saviour of the Alliance."
PRINT
PRINT "But, the lone Tie bomber that had"
PRINT "escaped the destuction held the"
PRINT "leader of the Empire, Darth Vader."
B = 150
FOR a = 0 TO 320
B = B - 1 + INT(RND * 3)
IF B <= 100 THEN B = B + 1
IF B >= 199 THEN B = B - 1
LINE (a - 2, B - 2)-(a + 2, B + 2), 0, BF
FOR aa = 1 TO 150
PSET (sx(aa), sy(aa)), cl(aa) * 10
NEXT

PSET (a, B), 100
tim14 = TIMER + .1
WHILE TIMER < tim14: WEND
NEXT
LOCATE 23, 14: COLOR 60: PRINT "TO BE CONTINUED"
WHILE INKEY$ <> " ": WEND
CLS
PRINT "EPISODE 2"
PRINT "  Empire's Wrath"
PRINT
PRINT "You are part of a group of Red X-Wing"
PRINT "fighters and are on a routine patrol"
PRINT "of Deep Space when you and the fighters"
PRINT "find five Star Destroyers heading toward"
PRINT "a recently liberated system."
PRINT ""
PRINT "All of your team except you are"
PRINT "destoryed as they fire most of their"
PRINT "cannons at you."
PRINT
PRINT "At main operations on the planet Tua,"
PRINT "You are quickly informed that the main"
PRINT "Alliance Force is 25 light years away"
PRINT "and there is only you to fight them!"
WHILE INKEY$ <> " ": WEND
CLS
PRINT "All is not lost however, some newly"
PRINT "smuggled Blue prints have been"
PRINT "studied and scientists have found"
PRINT "four key areas on the Star Destroyers."
PRINT
PRINT "Your mission is to immediatly go to"
PRINT "intercept the Star Destroyers and "
PRINT "destroy them. The ships have ten "
PRINT "Tie fighters each and have 5 cannons"
PRINT
PRINT "Your X-wing is fitted with a new type"
PRINT "of torpedo which will be used to "
PRINT "Penetrate their shields and blow the"
PRINT "Key areas."
PRINT
PRINT "GOOD LUCK"
WHILE INKEY$ <> " ": WEND
CLS
PRINT "Episode Two is in the making and should"
PRINT "be finished by June 97. Look out for "
PRINT "More SWITCHBLADE PROGRAMS ON THE NET!"
PRINT
PRINT "Nicholas Klaebe"

WHILE INKEY$ <> " ": WEND
GOTO menu


game:
B = 15
FOR a = 1 TO 51
B = B + .7
PALETTE a, 65536 * INT(B) + 2 + 256 * (INT(B)) + INT(B)
NEXT
CLS
FOR a = 101 TO 200 STEP 5
LINE (0, a)-(320, a + 10), a - 100, BF
NEXT
cl = 0
FOR a = 0 TO 1000
tim = TIMER + .3
PSET (160 - (a / 1.5), 100 + a / 5), 0
PSET (160 + (a / 1.5), 100 + a / 5), 0
NEXT
PAINT (160, 198), 0
FOR a = 0 TO 1000
PSET (160 - (a / 1.5), 100 + a / 5), 100
PSET (160 + (a / 1.5), 100 + a / 5), 100
PSET (160 - (a / 5), 100 + a / 5), 100
PSET (160 + (a / 5), 100 + a / 5), 100
B = B + 1
IF B < 20 THEN
  LINE (160 - (a / 5), 100 + a / 5)-(160 + (a / 5), 100 + a / 5), 30 + cl
  X = 160 - (a / 5)
  y = 100 + a / 5 - 1
  IF y > 200 THEN y = 199
  WHILE POINT(X, y) = 0
    PSET (X, y), 30 + cl
    y = y - 1
  WEND
  X = 160 + (a / 5)
  y = 100 + a / 5 - 1
IF y > 200 THEN y = 199
  WHILE POINT(X, y) = 0
    PSET (X, y), 30 + cl
    y = y - 1
  WEND
ELSE
  X = 160 - (a / 5)
  y = 100 + a / 5 - 1
IF y > 200 THEN y = 199
WHILE POINT(X, y) = 0
  PSET (X, y), 30 + cl
  y = y - 1
WEND
X = 160 + (a / 5)
y = 100 + a / 5 - 1
IF y > 200 THEN y = 199
WHILE POINT(X, y) = 0
  PSET (X, y), 30 + cl
  y = y - 1
WEND
cl = cl + 5
IF cl = 20 THEN cl = 5
B = 0
END IF
NEXT
LINE (160, 99)-(160, 0), 0
x2 = 319
x1 = 0
Y1 = 100
Y2 = 199
REDIM bk(4 + INT(((x2 - x1 + 1) * (4 * ((Y2 - Y1) + 1) + 7) / 8))) AS INTEGER
GET (0, 100)-(319, 199), bk
RANDOMIZE TIMER
y = 140: X = 110
gunwait = 1
PUT (210, 0), planet, PSET
LOCATE 2, 1: PRINT "TIME LEFT"
LOCATE 3, 1: PRINT "DISTANCE"
shield = 100
minteg = 0
fff = 1
integ = 100
saysth = 0
wzoom = 0
ytorpzoom = 0
dis = 1
timleft = 0
hittrench = 0
mshield = 0
zoomfac = 0
shieldfac = 0
confac = 0
gunwait = 0
4
ex = INT(RND * 160 * 4 - 1) - 320
ey = INT(RND * 50) + 145
ex1 = 160 + ex * (.3)
ey1 = 100 + ey * (.3)
IF tonview = 0 THEN
  tno = tno + 1:
  tonview = 1
  IF tno = 1 THEN
    tno = 0
      IF INT(RND * 2) + 1 = 1 THEN
        tx = -320
        ty = 150
      ELSE
        tx = 320
        ty = 150
      END IF
    tzoom = .01
  END IF
END IF

zoom = .01
x1 = 160 + ex * (zoom + .01) - 10
x2 = 160 + (ex + 30) * (zoom + .01) + 10
Y1 = 100 + ey * (zoom + .01) - 10
Y2 = 100 + (ey + 30) * (zoom + .01) + 10
REDIM ebk(4 + INT(((x2 - x1 + 1) * (4 * ((Y2 - Y1) + 1) + 7) / 8))) AS INTEGER
GET (x1, Y1)-(x2, Y2), ebk

3
tim = TIMER + spd
tim1 = TIMER + .0000001
WHILE TIMER < tim:
  tim1 = TIMER + spd / 2
    WHILE TIMER < tim1
    SELECT CASE INKEY$
      CASE "/": spd = 15 / 100
      CASE "*": dis = 12900
      CASE "+": gun = gun + 1: IF gun > gunlim THEN gun = 0
      CASE "-": gun = gun - 1: IF gun < 0 THEN gun = gunlim
      CASE "5": fire2 = guntype(gun) + 1
      CASE "8", CHR$(0) + "H": ystep = ystep - 2 - confac
      CASE "2", CHR$(0) + "P": ystep = ystep + 2 - confac
      CASE "4", CHR$(0) + "K": xstep = xstep - 2 - confac
      CASE "6", CHR$(0) + "M": xstep = xstep + 2 - confac
      CASE "7": xstep = xstep - 2: ystep = ystep - 2 - confac
      CASE "9": xstep = xstep + 2: ystep = ystep - 2 - confac
      CASE "1": xstep = xstep - 2: ystep = ystep + 2 - confac
      CASE "3": xstep = xstep + 2: ystep = ystep + 2 - confac
      CASE " ": fire = 1
    END SELECT
    WEND
''''''''''''''
'Main Console'
''''''''''''''
  IF timon1 = 0 THEN tim11 = TIMER + spd * (53.333333334# - (spd * 133.3)): timon1 = 1
  IF TIMER < tim11 THEN
  ELSE
    timleft = timleft + 1
    timon1 = 0
    LINE (70 + timleft, 8)-(70 + timleft, 16), 210
  END IF
  IF timleft = 130 THEN death = 1: GOTO 112
  dis = dis + 5 - (zoomfac * 500)
  LINE (70 + INT(dis / 100), 17)-(70 + INT(dis / 100), 17 + 8), 254
  IF shield <> mshield THEN
    COLOR 20
    LOCATE 5, 1: PRINT "SHIELD:"; shield; "% "
    IF shield = 0 THEN tim10 = tim10 + 4
    mshield = shield
  END IF
  IF integ <> minteg THEN
    COLOR 20
    LOCATE 6, 1: PRINT "STRUCTUAL INTEGRITY:"; integ: LOCATE 6, 21 + LEN(integ): PRINT "% "
    IF minteg <> 0 AND fff = 0 THEN saysth = 1:
    fff = 0
    minteg = integ
  END IF
  IF integ <= 0 THEN
    explode 10, 10, 10, X + 25, y + 10, 20, 63, son, 30, 10, 0
    GOTO 112
  END IF
  IF saysth = 1 THEN
    LOCATE 7, 1: PRINT "                           "
    w = INT(RND * 4) + 1
    IF w = 1 THEN LOCATE 7, 1: PRINT "Engines hit!": zoomfac = zoomfac + .002
    IF w = 2 THEN LOCATE 7, 1: PRINT "Shield Generator HIT": shieldfac = shieldfac + .25
    IF w = 3 THEN LOCATE 7, 1: PRINT "WEAPON DAMAGED": gunwait = gunwait + 1
    IF w = 4 THEN LOCATE 7, 1: PRINT "Steering control hit": confac = confac + .5
    IF confac < 0 THEN confac = 0
    saysth = 0
  END IF
  IF gun <> mgun THEN
   mgun = gun
   
   LOCATE 8, 1: PRINT "                           "
   LOCATE 8, 1: PRINT "Weapon 2:";
  SELECT CASE guntype(gun)
    CASE 1: PRINT "Homing Missiles"
    CASE 2: PRINT "Shield Restore"
    CASE 3: PRINT "Anti-Torpedo Blast"
    CASE 0: PRINT "Torpedos"
  END SELECT
  END IF
  IF INT(dis / 100) > 130 THEN GOTO endtrench
  
''''''''''''''''''
'Shield Generator'
''''''''''''''''''
  IF timon = 0 THEN timon = 1: tim10 = TIMER + 1 + (shieldfac * 2)
  IF TIMER > tim10 THEN shield = shield + 1: timon = 0
  IF shield > 100 THEN shield = 100

''''''''''''''
'Surface fire'
''''''''''''''
  IF y < 140 THEN
    sfx1 = INT(RND * 319) + 1
    sfy1 = INT(RND * 60) + 100
    sfx2 = INT(RND * 319) + 1
    sfy2 = INT(RND * 60) + 100
    LINE (0, 100)-(sfx1, sfy1), 254
    LINE (319, 100)-(sfx2, sfy2), 254
  
    C = 0
    tim7 = TIMER + .1
    WHILE TIMER < tim7
      C = C + 1
      FOR a = 180 TO 360 STEP 10
        bb = a * (pi / 180)
      IF son = 1 THEN sbplay1 50 + C + COS(bb) * 50, 5, 6, 5, 0
      IF son = 1 THEN sbplay1 50 + C + SIN(bb) * 50, 5, 6, 5, 0
      NEXT
      FOR a = 0 TO 180 STEP 10
        bb = a * (pi / 180)
      IF son = 1 THEN sbplay1 0 + C + COS(bb) * 100, 5, 6, 5, 0
      IF son = 1 THEN sbplay1 0 + C + SIN(bb) * 100, 5, 6, 5, 0
      NEXT
      WEND
    IF son = 1 THEN sbplay1 200 + es1, 5, 0, 0, 0
    IF son = 1 THEN CALL WriteReg(&HB0, &H0)
  
    IF sfx1 > X AND sfx1 < X + 50 AND sfy1 > y AND sfy1 < y + 20 THEN
      IF shield = 0 THEN
        integ = integ - 25
      ELSE
        shield = shield - 25
        IF shield < 0 THEN shield = 0
      END IF
    END IF
    IF sfx2 > X AND sfx2 < X + 50 AND sfy2 > y AND sfy2 < y + 20 THEN
      IF shield = 0 THEN
        integ = integ - 25
      ELSE
        shield = shield - 25
        IF shield < 0 THEN shield = 0
      END IF
    END IF
  END IF

''''''''''''''
'TIE FIGHTERS'
''''''''''''''
  IF intel = 0 THEN
    IF INT(RND * 10) + 1 = 1 THEN
      o = INT(RND * 319) + 1
      p = INT(RND * 50) + 150
      LINE (160 + (ex + 50) * (zoom - B), 100 + (ey + 50) * (zoom - B))-(o, p), 210
      tim4 = TIMER + .1
      FOR a = 1 TO 1000
        es = es + 50
        es1 = es1 - 50
        IF es = 1000 THEN es = -1000
        IF es1 = -1000 THEN es1 = 1000
        IF son = 1 THEN sbplay1 50 + es - 25 + INT(RND * 50), 5, 6, 5, 0
        IF son = 1 THEN sbplay1 50 + es1 - 25 + INT(RND * 50), 5, 6, 5, 0
      NEXT
      IF son = 1 THEN sbplay1 200 + es1, 5, 0, 0, 0
      IF son = 1 THEN CALL WriteReg(&HB0, &H0)
      IF o > X AND o < X + 50 AND p > y AND p < y + 20 THEN
        IF shield = 0 THEN
          integ = integ - 15
        ELSE
          shield = shield - 15
          IF shield < 0 THEN shield = 0
        END IF
      END IF
    END IF
  END IF
''''''''''''''''
'PUT BACKGROUND'
''''''''''''''''
endtrench:
  PUT (0, 100), bk, PSET

'''''''''''''''
'End of trench'
'''''''''''''''
  IF INT(dis / 100) > 130 THEN
    IF wzoom >= .3 AND y > 130 THEN integ = 0
    IF wzoom >= .3 AND hittrench = 1 AND y < 130 THEN GOTO 113
    IF wzoom >= .3 AND y < 130 THEN GOTO 112
    LINE (160 + (-534 * wzoom), 100 + (150 * wzoom))-(160 + (534 * wzoom), 100 + (667 * wzoom)), 50, BF
    LINE (160 + (-534 * wzoom), 100 + (150 * wzoom))-(160, 100), 50
    LINE (160 + (534 * wzoom), 100 + (150 * wzoom))-(160, 100), 50
    PAINT (160 + (0 * wzoom), 100 + (130 * wzoom)), 50
    LINE (160 + (-534 * wzoom), 100 + (150 * wzoom))-(160 + (534 * wzoom), 100 + (667 * wzoom)), 30, B
    LINE (160 + (-534 * wzoom), 100 + (150 * wzoom))-(160, 100), 30
    LINE (160 + (534 * wzoom), 100 + (150 * wzoom))-(160, 100), 30
    LINE (160 + (-50 * wzoom), 100 + (310 * wzoom))-(160 + (50 * wzoom), 100 + (370 * wzoom)), 0, BF
    IF hittrench = 1 THEN LINE (160 + (-50 * wzoom), 100 + (310 * wzoom))-(160 + (50 * wzoom), 100 + (370 * wzoom)), 254, BF
  END IF
  IF ytorp = 1 THEN
    IF ytorpzoom <= wzoom THEN
      tr = INT(RND * (10 - wzoom * 10))
      IF tr < 3 THEN hittrench = 1: fire2 = 0: firsttim1 = 0: ytorp = 0 ELSE ytorpy = 300:
      IF ytorpzoom <= wzoom AND hittrench = 0 THEN CALL explode(10, 10, 10, 160 + ytorpx * ytorpzoom, 100 + ytorpy * ytorpzoom, 10, 10, son, 1, 1, 1): ytorp = 0: firsttim1 = 0: fire2 = 0
    END IF
  END IF
  IF INT(dis / 100) > 130 THEN GOTO zooms

''''''''''''''
'TARGET CHEAT'
''''''''''''''

'LINE (ex1, ey1)-(ex1 + 30, ey1 + 30), 240, B



  LINE (160 + ex * (zoom - B), 100 + ey * (zoom - B))-(160 + ex * (zoom - B), 100 + (ey + 100) * (zoom - B)), 215
  LINE (160 + (ex + 100) * (zoom - B), 100 + ey * (zoom - B))-(160 + (ex + 100) * (zoom - B), 100 + (ey + 100) * (zoom - B)), 215
  LINE (160 + ex * (zoom - B), 100 + (ey + 50) * (zoom - B))-(160 + (ex + 100) * (zoom - B), 100 + (ey + 50) * (zoom - B)), 215
  FOR C = 1 TO 20
  CIRCLE (160 + (ex + 50) * (zoom - B), 100 + (ey + 50) * (zoom - B)), C * zoom, 215
  NEXT

''''''''
'TOWERS'
''''''''
  IF tonview = 1 THEN
    I = 0
    FOR a = tzoom - tzoom / 7.5 TO tzoom STEP .004
      LINE (160 + tx * a, 100 + ty * a)-(160 + (tx + 100) * a, 100 + (ty + 200) * a), 20, BF
    NEXT
    IF tx = 320 THEN
      LINE (160 + tx * tzoom, 100 + ty * tzoom)-(160 + tx * (tzoom - tzoom / 7.5), 100 + ty * (tzoom - tzoom / 7.5)), 24
      LINE (160 + (tx + 100) * tzoom, 100 + ty * tzoom)-(160 + (tx + 100) * (tzoom - tzoom / 7.5), 100 + ty * (tzoom - tzoom / 7.5)), 24
      LINE (160 + tx * (tzoom - tzoom / 7.5), 100 + ty * (tzoom - tzoom / 7.5))-(160 + (tx + 100) * (tzoom - tzoom / 7.5), 100 + ty * (tzoom - tzoom / 7.5)), 24
      LINE (160 + tx * (tzoom - tzoom / 7.5), 100 + ty * (tzoom - tzoom / 7.5))-(160 + tx * (tzoom - tzoom / 7.5), 100 + (ty + 200) * (tzoom - tzoom / 7.5)), 24
      LINE (160 + tx * tzoom, 100 + ty * tzoom)-(160 + (tx + 100) * tzoom, 100 + (ty + 200) * tzoom), 24, B
    ELSE
      LINE (160 + tx * tzoom, 100 + ty * tzoom)-(160 + tx * (tzoom - tzoom / 7.5), 100 + ty * (tzoom - tzoom / 7.5)), 24
      LINE (160 + (tx + 100) * tzoom, 100 + ty * tzoom)-(160 + (tx + 100) * (tzoom - tzoom / 7.5), 100 + ty * (tzoom - tzoom / 7.5)), 24
      LINE (160 + tx * (tzoom - tzoom / 7.5), 100 + ty * (tzoom - tzoom / 7.5))-(160 + (tx + 100) * (tzoom - tzoom / 7.5), 100 + ty * (tzoom - tzoom / 7.5)), 24
      LINE (160 + (tx + 100) * (tzoom - tzoom / 7.5), 100 + ty * (tzoom - tzoom / 7.5))-(160 + (tx + 100) * (tzoom - tzoom / 7.5), 100 + (ty + 200) * (tzoom - tzoom / 7.5)), 24
      LINE (160 + tx * tzoom, 100 + ty * tzoom)-(160 + (tx + 100) * tzoom, 100 + (ty + 200) * tzoom), 24, B
    END IF
    IF tzoom > .106 THEN
      CIRCLE (160 + (tx + 25) * a, 100 + (ty + 25) * a), 10 * a, 0
      PAINT (160 + (tx + 25) * a, 100 + (ty + 25) * a), 0
      CIRCLE (160 + (tx + 75) * a, 100 + (ty + 25) * a), 10 * a, 0
      PAINT (160 + (tx + 75) * a, 100 + (ty + 25) * a), 0
    ELSE
      PSET (160 + (tx + 25) * a, 100 + (ty + 25) * a), 0
      PSET (160 + (tx + 75) * a, 100 + (ty + 25) * a), 0
    END IF
    IF intel = 0 THEN
    IF INT(RND * 5) + 1 = 1 THEN
      tfx = INT(RND * 300) + 10
      tfy = INT(RND * 50) + 150
      ta1 = INT(RND * 360) + 1
      ta2 = INT(RND * 360) + 1
      ta1 = ta1 * (pi / 180)
      ta2 = ta2 * (pi / 180)
      tb1 = INT(RND * 20) + 1
      tb2 = INT(RND * 20) + 1
      LINE (160 + (tx + 25) * a, 100 + (ty + 25) * a)-(tfx + COS(ta1) * tb1, tfy + SIN(ta1) * tb1), 254
      LINE (160 + (tx + 75) * a, 100 + (ty + 25) * a)-(tfx + COS(ta2) * tb2, tfy + SIN(ta2) * tb2), 254
      GET (X, y)-(X + 50, y + 20), ybk
      LINE (X, y)-(X + 50, y + 20), 60
      LINE (X, y + 20)-(X + 50, y), 60
      FOR aaa = 1 TO 5
        CIRCLE (X + 15, y + 10), aaa, 150
        CIRCLE (X + 35, y + 10), aaa, 150
      NEXT
      FOR a = 1 TO 5
        IF torpzoom(a) > .015 THEN
          IF tonscreen(a) = 1 THEN
            C = 15
            CIRCLE (160 + (torpx(a)) * (torpzoom(a) - B), 100 + (torpy(a)) * (torpzoom(a))), C * torpzoom(a), 150
            PAINT (160 + (torpx(a)) * (torpzoom(a) - B), 100 + (torpy(a)) * (torpzoom(a))), 150
          END IF
        END IF
      NEXT
      C = 0
      tim7 = TIMER + .1
      WHILE TIMER < tim7
      C = C + 1
      FOR a = 180 TO 360 STEP 2
        bb = a * (pi / 180)
        IF son = 1 THEN sbplay1 50 + C + COS(bb) * 50, 5, 6, 5, 0
        IF son = 1 THEN sbplay1 50 + C + SIN(bb) * 50, 5, 6, 5, 0
      NEXT
      FOR a = 0 TO 180 STEP 2
        bb = a * (pi / 180)
        IF son = 1 THEN sbplay1 51 + C + COS(bb) * 50, 5, 6, 5, 0
        IF son = 1 THEN sbplay1 51 + C + SIN(bb) * 50, 5, 6, 5, 0
      NEXT
    WEND
    IF son = 1 THEN sbplay1 200 + es1, 5, 0, 0, 0
    IF son = 1 THEN CALL WriteReg(&HB0, &H0)
    o = tfx + COS(ta1) * tb1
    p = tfy + Sib(ta1) * tb1
    o1 = tfx + COS(ta2) * tb2
    p1 = tfy + Sib(ta2) * tb2
    IF o > X + 3 AND o < X + 45 AND p > y AND p < y + 20 THEN
      IF shield = 0 THEN
        integ = integ - 20
      ELSE
        shield = shield - 20
        IF shield < 0 THEN shield = 0
      END IF
    END IF
GOTO 8
    IF o1 > X + 3 AND o1 < X + 45 AND p1 > y AND p1 < y + 20 THEN
      IF shield = 0 THEN
        integ = integ - 20
      ELSE
        shield = shield - 20
        IF shield < 0 THEN shield = 0
      END IF
    END IF
8
    PUT (X, y), ybk, PSET
    END IF
  END IF
    IF tzoom > .32 THEN
      IF tx = -320 THEN
        GET (X, y)-(X + 50, y + 20), ybk
        LINE (X, y)-(X + 50, y + 20), 60
        LINE (X, y + 20)-(X + 50, y), 60
        FOR aaa = 1 TO 5
          CIRCLE (X + 15, y + 10), aaa, 150
          CIRCLE (X + 35, y + 10), aaa, 150
        NEXT
        IF X < 100 AND y + 20 > 150 THEN integ = 0
        PUT (X, y), ybk, PSET
      ELSE
        GET (X, y)-(X + 50, y + 20), ybk
        LINE (X, y)-(X + 50, y + 20), 60
        LINE (X, y + 20)-(X + 50, y), 60
        FOR aaa = 1 TO 5
          CIRCLE (X + 15, y + 10), aaa, 150
          CIRCLE (X + 35, y + 10), aaa, 150
        NEXT
        IF X + 50 > 280 AND y + 20 > 150 THEN integ = 0
        PUT (X, y), ybk, PSET
      END IF
      tzoom = .6: tonview = 0
    END IF
  END IF
''''''''''
'Torpedos'
''''''''''
  FOR a = 1 TO 6
    IF tonscreen(a) = 0 THEN
      IF INT(RND * 20) + 1 = 1 THEN
        torpx(a) = INT(RND * 160 * 4 - 1) - 320
        torpy(a) = INT(RND * 100) + 150
        tonscreen(a) = 1
        torpzoom(a) = 0
      END IF
    END IF
    IF torpzoom(a) > .015 THEN
      IF tonscreen(a) = 1 THEN
      C = 15
      CIRCLE (160 + (torpx(a)) * (torpzoom(a) - B), 100 + (torpy(a)) * (torpzoom(a))), C * torpzoom(a), 150
      PAINT (160 + (torpx(a)) * (torpzoom(a) - B), 100 + (torpy(a)) * (torpzoom(a))), 150
      IF torpzoom(a) > .36 THEN
        IF 160 + (torpx(a)) * (torpzoom(a) - B) > X AND 160 + (torpx(a)) * (torpzoom(a) - B) < X + 50 AND 100 + (torpy(a)) * (torpzoom(a)) > y AND 100 + (torpy(a)) * (torpzoom(a)) < y + 20 THEN
          shield = shield - 10
          tonscreen(a) = 0
          LINE (X, y)-(X + 50, y + 20), 60
          LINE (X, y + 20)-(X + 50, y), 60
          FOR aaa = 1 TO 5
            CIRCLE (X + 15, y + 10), aaa, 150
            CIRCLE (X + 35, y + 10), aaa, 150
          NEXT
          CALL explode(10, 10, 10, 160 + (torpx(a)) * (torpzoom(a) - B), 100 + (torpy(a)) * (torpzoom(a)), 10, 10, son, 1, 1, 1)
          IF shield <= 0 THEN
            integ = integ - 10
          END IF
          IF shield < 0 THEN shield = 0
        END IF
      END IF
    END IF
  END IF
  NEXT
'''''''
'Bonus'
'''''''
IF bon = 1 THEN
  IF bonfirst = 0 THEN
  bzoom = 0:
  bx = INT(RND * 160 * 4 - 1) - 320
  by = INT(RND * 100) + 150
  bonfirst = 1
  END IF
  FOR a = 1 TO 15
  CIRCLE (160 + bx * bzoom, 100 + by * bzoom), a * bzoom, INT(RND * 250) + 1
  NEXT
  IF bzoom > .36 THEN
  bonfirst = 0: bon = 0
  IF 160 + bx * bzoom > X AND 160 + bx * bzoom < X + 50 AND 100 + by * bzoom > y AND 100 + by * bzoom < y + 20 THEN
      tim7 = TIMER + .1
      WHILE TIMER < tim7
      C = C + 1
      FOR a = 180 TO 360 STEP 20
        bb = a * (pi / 180)
        IF son = 1 THEN sbplay1 150 + C + COS(bb) * 50, 5, 6, 5, 0
        IF son = 1 THEN sbplay1 150 + C + SIN(bb) * 50, 5, 6, 5, 0
      NEXT
      FOR a = 0 TO 180 STEP 2
        bb = a * (pi / 180)
        IF son = 1 THEN sbplay1 151 + C + COS(bb) * 50, 5, 6, 5, 0
        IF son = 1 THEN sbplay1 151 + C + SIN(bb) * 50, 5, 6, 5, 0
      NEXT
    WEND
    IF son = 1 THEN sbplay1 151 + C + COS(bb) * 50, 1, 1, 1, 0
    IF son = 1 THEN CALL WriteReg(&HB0, &H0)
  gunlim = 1
  guntype(1) = INT(RND * 3) + 1
  guninhand = 1
  guntim = TIMER + 10
  END IF
  END IF
ELSE
  IF INT(RND * 50) = 1 THEN bon = 1
END IF

 IF TIMER > guntim THEN gunlim = 0: guninhand = 0: gun = 0

'''''
'YOU'
'''''
zooms:

  IF xstep > 5 THEN xstep = 6
  IF ystep > 5 THEN ystep = 6
  IF ystep < -5 THEN ystep = -6
  IF xstep < -5 THEN xstep = -6
  X = X + xstep
  y = y + ystep
  IF X < 0 THEN X = 0: xstep = 0
  IF X > 269 THEN X = 269: xstep = 0
  IF y < 100 THEN y = 100: ystep = 0
  IF y > 179 THEN y = 179: ystep = 0
  LINE (X, y)-(X + 50, y + 20), 60
  LINE (X, y + 20)-(X + 50, y), 60
  FOR a = 1 TO 5
    CIRCLE (X + 15, y + 10), a, 150
    CIRCLE (X + 35, y + 10), a, 150
  NEXT
  IF gunwait = 1 THEN 12
  IF fi = 1 THEN IF TIMER < tim12 THEN 11
12
  fi = 0
  IF fire = 1 THEN
    fi = 1
    tim12 = TIMER + gunwait
    fx(1) = X
    fy(1) = y
    fx(2) = X + 50
    fy(2) = y
    fx(3) = X
    fy(3) = y + 20
    fx(4) = X + 50
    fy(4) = y + 20
    FOR C = 1 TO 4
      mfx(C) = fx(C)
      mfy(C) = fy(C)
    NEXT
    FOR C = 1 TO 4
      LINE (fx(C), fy(C))-(160, 100), 190
    NEXT
    tim2 = TIMER + .05
    WHILE TIMER < tim2: WEND
    fx = (X + 25) - 160
    fy = (y + 10) - 100
    s = -10
    s1 = 10
    FOR a = 1 TO 1000
      s = s + 1
      s1 = s1 - 1
      IF s = 100 THEN s = -100
      IF s1 = -100 THEN s1 = 100
      IF son = 1 THEN sbplay1 100 + s, 5, 6, 5, 0
      IF son = 1 THEN sbplay1 100 + s1, 5, 6, 5, 0
    NEXT
    IF son = 1 THEN sbplay1 100 + s1, 5, 0, 0, 0
    IF son = 1 THEN CALL WriteReg(&HB0, &H0)
    IF X + 25 > ex1 AND X + 25 < ex1 + 30 AND y + 10 > ey1 AND y + 10 < ey1 + 30 THEN fire = 0: explode 10, 10, 10, 160 + (ex + 50) * (zoom - B), 100 + (ey + 50) * (zoom - B), 60 * zoom, 33, son, 70 * zoom, 70 * zoom, 0: GOTO 4
    fire = 0
  END IF
11
  
  IF fire2 = 1 THEN
    IF firsttim1 = 0 THEN
      firsttim1 = 1
      ytorp = 1
      
      ytorpy = y + 20
      ytorpx = -380 + (X + 25) * 2.2
      ytorpzoom = .3
    END IF
    IF ytorpx < 0 THEN ytorpx = ytorpx + 10
    IF ytorpx > 0 THEN ytorpx = ytorpx - 10
    IF ytorpy < 320 THEN ytorpy = ytorpy + 10
    IF ytorpy > 320 THEN ytorpy = ytorpy - 10
    CIRCLE (160 + ytorpx * ytorpzoom, 100 + ytorpy * ytorpzoom), 15 * ytorpzoom, 254
    PAINT (160 + ytorpx * ytorpzoom, 100 + ytorpy * ytorpzoom), 254
  END IF

  IF fire2 = 2 THEN
    IF firsttim2 = 0 THEN
      firsttim2 = 1
      mis = 1
      misy = y + 20
      misx = -380 + (X + 25) * 2.2
      mzoom = .4
    END IF
  END IF
  IF mis = 1 THEN
   IF misy < ey + 15 THEN misy = misy + 10
   IF misy > ey + 15 THEN misy = misy - 10
   IF misx < ex + 15 THEN misx = misx + 10
   IF misx > ex + 15 THEN misx = misx - 10
   FOR a = 1 TO 6
   CIRCLE (160 + misx * mzoom, 100 + misy * mzoom), a * mzoom, 150
   NEXT
   IF mzoom > zoom - .02 AND mzoom < zoom + .02 AND misx > ex AND misy > ey AND misx < ex + 100 AND misy < ey + 50 THEN explode 10, 10, 10, 160 + (ex + 50) * (zoom - B), 100 + (ey + 50) * (zoom - B), 60 * zoom, 33, son, 70 * zoom, 70 * zoom, 0: mis  _
= 0: fire2 = 0: firsttim2 = 0: GOTO 4
     
  END IF
  IF fire2 = 4 THEN
    FOR a = 1 TO 10
      tonscreen(a) = 0
    NEXT
  END IF
  IF fire2 = 3 THEN
   shield = shield + 25
   IF shield > 100 THEN shield = 100
   guntim = TIMER
  END IF

WEND

'''''''''''''''''
'Palette control'
'''''''''''''''''
r = r + 1
IF r >= 4 + (zoomfac * 500) THEN r = 1
IF r = 1 THEN
  PALETTE 45, 65536 * INT(35) + 2 + 256 * (INT(35)) + INT(35)
  PALETTE 40, 65536 * INT(41) + 2 + 256 * (INT(41)) + INT(41)
  PALETTE 35, 65536 * INT(41) + 2 + 256 * (INT(41)) + INT(41)
END IF
IF r = 2 THEN
  PALETTE 35, 65536 * INT(35) + 2 + 256 * (INT(35)) + INT(35)
  PALETTE 45, 65536 * INT(41) + 2 + 256 * (INT(41)) + INT(41)
  PALETTE 40, 65536 * INT(41) + 2 + 256 * (INT(41)) + INT(41)
END IF
IF r = 3 THEN
  PALETTE 40, 65536 * INT(35) + 2 + 256 * (INT(35)) + INT(35)
  PALETTE 35, 65536 * INT(41) + 2 + 256 * (INT(41)) + INT(41)
  PALETTE 45, 65536 * INT(41) + 2 + 256 * (INT(41)) + INT(41)
  END IF
''''''
'ZOOM'
''''''
IF zoomfac >= .01 THEN GOTO 3
zoom = zoom + (.01 - zoomfac)
IF zoom >= .3 THEN GOTO 4
tzoom = tzoom + (.011 - zoomfac)
IF tzoom >= .4 THEN tonview = 0
FOR a = 1 TO 6
torpzoom(a) = torpzoom(a) + (.015 - zoomfac)
IF torpzoom(a) >= .4 THEN tonscreen(a) = 0
NEXT
IF INT(dis / 100) > 130 THEN
  wzoom = wzoom + .004
END IF
IF ytorp = 1 THEN ytorpzoom = ytorpzoom - .01
IF ytorpzoom <= .01 THEN ytorp = 0: firsttim1 = 0: tfire = 0: fire2 = 0
IF bon = 1 THEN bzoom = bzoom + .015
IF mis = 1 THEN mzoom = mzoom - .015: IF mzoom < .015 THEN fire2 = 0: fire2 = 0: firsttim2 = 0: mis = 0
GOTO 3

menu:
CLS
CALL menu(sx(), sy(), cl(), outcome)
GOTO outcome1

SUB ChangeColors

' Changes the palette to one of 6 possible color variations

DifColors% = INT(RND * 6) + 1

FOR X% = 1 TO 230

 OUT &H3C8, X%

  SELECT CASE DifColors%
   CASE 1: OUT &H3C9, EasyVal!: OUT &H3C9, 0: OUT &H3C9, 0
   CASE 2: OUT &H3C9, 0: OUT &H3C9, EasyVal!: OUT &H3C9, 0
   CASE 3: OUT &H3C9, 0: OUT &H3C9, 0: OUT &H3C9, EasyVal!
   CASE 4: OUT &H3C9, EasyVal!: OUT &H3C9, EasyVal!: OUT &H3C9, EasyVal!
   CASE 5: OUT &H3C9, EasyVal!: OUT &H3C9, EasyVal!: OUT &H3C9, 0
   CASE 6: OUT &H3C9, EasyVal!: OUT &H3C9, 0: OUT &H3C9, EasyVal!
  END SELECT

 EasyVal! = EasyVal! + .273913

NEXT

END SUB

SUB Delay (Seconds!)
Future! = TIMER + Seconds!
DO
LOOP UNTIL TIMER >= Future! OR TIMER - (TIMER - Seconds!) < 0
END SUB

DEFINT A-Z
FUNCTION DetectCard%

'Returns -1 (true) if detected and 0 (false) if not.
                                                         
CALL WriteReg(&H4, &H60)
CALL WriteReg(&H4, &H80)
B = INP(&H388)
CALL WriteReg(&H2, &HFF)
CALL WriteReg(&H4, &H21)
FOR X = 0 TO 130
a = INP(&H388)
NEXT X
C = INP(&H388)
CALL WriteReg(&H4, &H60)
CALL WriteReg(&H4, &H80)
SUCCESS = 0
IF (B AND &HE0) = &H0 THEN
IF (C AND &HE0) = &HC0 THEN
SUCCESS = -1
END IF
END IF
DetectCard% = SUCCESS
END FUNCTION

DEFSNG A-Z
SUB explode (green%, Blue%, red%, boomX, boomY, boomSize, comarcs, son, ax, ay, noarc)
    
'radius = 3.146
'This code started as EXPLODE.BAS by Brent P. Newhall.  I've only made a
'few minor changes.  The comments are all his.

''I was thinking about that "I need fire and an exploding graphic" post
''and came up with this simple explosion program.  Any suggestions,
''comments, etc. are welcome with open arms.

''NOTES: There are two lines that do not wrap properly; you'll be able to
''find them easily.  Also, NumArcs is set to 100, which does an
''acceptable explosion on my 486DX2/50 with 8 megs of RAM.  You Pentium
''people should be able to get a much nicer explosion if you set NumArcs
''equal to 1000 or so.

''The program sets colors 100 to 131 to varying shades of red, orange,
''and yellow for the explosive effect, with the nice side-effect that
'you can theoretically do several explosions at once.

NumArcs = comarcs
DIM arc(1 TO NumArcs) AS arctype
RANDOMIZE TIMER
FOR cnt% = 1 TO NumArcs STEP 1' Initialise
  arc(cnt%).radius = 0
  arc(cnt%).p1 = RND * 5.8
  arc(cnt%).p2 = arc(cnt%).p1 + (RND / 2)
  arc(cnt%).colr = INT(RND * 5 + 221)
NEXT cnt%
rx = -(ax / 2) + INT(RND * ax) + 1
ry = -(ay / 2) + INT(RND * ay) + 1
rx1 = -(ax / 2) + INT(RND * ax) + 1
ry1 = -(ay / 2) + INT(RND * ay) + 1
rx2 = -(ax / 2) + INT(RND * ax) + 1
ry2 = -(ay / 2) + INT(RND * ay) + 1
rx3 = -(ax / 2) + INT(RND * ax) + 1
ry3 = -(ay / 2) + INT(RND * ay) + 1
rx4 = -(ax / 2) + INT(RND * ax) + 1
ry4 = -(ay / 2) + INT(RND * ay) + 1
rx5 = -(ax / 2) + INT(RND * ax) + 1
ry5 = -(ay / 2) + INT(RND * ay) + 1
rx6 = -(ax / 2) + INT(RND * ax) + 1
ry6 = -(ay / 2) + INT(RND * ay) + 1
rx7 = -(ax / 2) + INT(RND * ax) + 1
ry7 = -(ay / 2) + INT(RND * ay) + 1
rx8 = -(ax / 2) + INT(RND * ax) + 1
ry8 = -(ay / 2) + INT(RND * ay) + 1
rx9 = -(ax / 2) + INT(RND * ax) + 1
ry9 = -(ay / 2) + INT(RND * ay) + 1
red% = 63 ' Start it deep red
FOR cnt% = 221 TO 251 ' Change colors
  PALETTE cnt%, red% + 256 * green% + 65536 * Blue%

  IF cnt% <= 234 THEN ' Lower red, increase yellow
    red% = red% - 2
    green% = green% + 2
  ELSE ' Lower red and yellow
    red% = red% - 2
    green% = green% - 2
  END IF
NEXT cnt%
PALETTE 252, 0
timecnt% = 0
x1 = boomX - (boomSize + 30)
IF x1 < 0 THEN x1 = 0
Y1 = boomY - (boomSize + 30)
IF Y1 < 0 THEN Y1 = 0
x2 = boomX + (boomSize + 30)
IF x2 > 319 THEN x2 = 319
Y2 = boomY + (boomSize + 30)
IF Y2 > 199 THEN Y2 = 199
 DIM ExplodeHold(50 + (4 + INT(((x2 - x1 + 1) * (4 * ((Y2 - Y1) + 1) + 7) / 8)))) AS INTEGER
 GET (x1, Y1)-(x2, Y2), ExplodeHold(1)
DO
  timecnt% = timecnt% + 1
  FOR currarc% = 1 TO NumArcs
  
    IF arc(currarc%).radius > 4 THEN ' Erase previous arc
IF noarc = 0 THEN CIRCLE (boomX, boomY), arc(currarc%).radius - 5, 0, arc(currarc%).p1, arc(currarc%).p2
IF noarc = 1 OR noarc = 0 THEN CIRCLE (boomX + rx, boomY + ry), arc(currarc%).radius - 5, 0, arc(currarc%).p1, arc(currarc%).p2
IF noarc = 0 THEN CIRCLE (boomX + rx1, boomY + ry1), arc(currarc%).radius - 5, 0, arc(currarc%).p1, arc(currarc%).p2
IF noarc = 0 THEN CIRCLE (boomX + rx2, boomY + ry2), arc(currarc%).radius - 5, 0, arc(currarc%).p1, arc(currarc%).p2
IF noarc = 0 THEN CIRCLE (boomX + rx3, boomY + ry3), arc(currarc%).radius - 5, 0, arc(currarc%).p1, arc(currarc%).p2
IF noarc = 0 THEN CIRCLE (boomX + rx4, boomY + ry4), arc(currarc%).radius - 5, 0, arc(currarc%).p1, arc(currarc%).p2
IF noarc = 0 THEN CIRCLE (boomX + rx5, boomY + ry5), arc(currarc%).radius - 5, 0, arc(currarc%).p1, arc(currarc%).p2
IF noarc = 0 THEN CIRCLE (boomX + rx6, boomY + ry6), arc(currarc%).radius - 5, 0, arc(currarc%).p1, arc(currarc%).p2
IF noarc = 0 THEN CIRCLE (boomX + rx7, boomY + ry7), arc(currarc%).radius - 5, 0, arc(currarc%).p1, arc(currarc%).p2
IF noarc = 0 THEN CIRCLE (boomX + rx8, boomY + ry8), arc(currarc%).radius - 5, 0, arc(currarc%).p1, arc(currarc%).p2
IF noarc = 0 THEN CIRCLE (boomX + rx9, boomY + ry9), arc(currarc%).radius - 5, 0, arc(currarc%).p1, arc(currarc%).p2
IF son = 0 THEN SOUND -100 + 300 + timecnt% * 5 + (RND * 200), .025

FOR dee = 1 TO 5
IF son = 1 THEN sbplay1 0 + timecnt% + (RND * 200), 1, 2, 0, 0
NEXT
IF INT(RND * 60) + 1 = 60 THEN
  FOR de = 600 + INT(RND * 50) TO 100 + INT(RND * 50) STEP -60 + INT(RND * 40)
IF son = 0 THEN SOUND -100 + 300 + de * 5 + (RND * 200), .025
FOR dee = 1 TO 5
IF son = 1 THEN sbplay1 0 + de + (RND * 200), 1, 2, 0, 0
NEXT
  NEXT
  END IF
    
      END IF
    IF arc(currarc%).radius = 0 THEN ' Not yet alive
      ' About 30% of the time, create a new arc
      IF RND > .7 THEN arc(currarc%).radius = 1
    ELSEIF arc(currarc%).colr = 131 THEN ' Dead
      REM Do Nothing
    ELSE
      arc(currarc%).radius = arc(currarc%).radius + 1 ' Increase radius
      arc(currarc%).colr = arc(currarc%).colr + 1 ' Increase color
      ' Draw arc
     
IF noarc = 0 THEN CIRCLE (boomX, boomY), arc(currarc%).radius, arc(currarc%).colr, arc(currarc%).p1, arc(currarc%).p2
IF noarc = 1 OR noarc = 0 THEN CIRCLE (boomX + rx, boomY + ry), arc(currarc%).radius, arc(currarc%).colr, arc(currarc%).p1, arc(currarc%).p2
     
IF noarc = 0 THEN CIRCLE (boomX + rx1, boomY + ry1), arc(currarc%).radius, arc(currarc%).colr, arc(currarc%).p1, arc(currarc%).p2
IF noarc = 0 THEN CIRCLE (boomX + rx2, boomY + ry2), arc(currarc%).radius, arc(currarc%).colr, arc(currarc%).p1, arc(currarc%).p2
IF noarc = 0 THEN CIRCLE (boomX + rx3, boomY + ry3), arc(currarc%).radius, arc(currarc%).colr, arc(currarc%).p1, arc(currarc%).p2
IF noarc = 0 THEN CIRCLE (boomX + rx4, boomY + ry4), arc(currarc%).radius, arc(currarc%).colr, arc(currarc%).p1, arc(currarc%).p2
IF noarc = 0 THEN CIRCLE (boomX + rx5, boomY + ry5), arc(currarc%).radius, arc(currarc%).colr, arc(currarc%).p1, arc(currarc%).p2
IF noarc = 0 THEN CIRCLE (boomX + rx6, boomY + ry6), arc(currarc%).radius, arc(currarc%).colr, arc(currarc%).p1, arc(currarc%).p2
IF noarc = 0 THEN CIRCLE (boomX + rx7, boomY + ry7), arc(currarc%).radius, arc(currarc%).colr, arc(currarc%).p1, arc(currarc%).p2
    
IF noarc = 0 THEN CIRCLE (boomX + rx8, boomY + ry8), arc(currarc%).radius, arc(currarc%).colr, arc(currarc%).p1, arc(currarc%).p2
IF noarc = 0 THEN CIRCLE (boomX + rx9, boomY + ry9), arc(currarc%).radius, arc(currarc%).colr, arc(currarc%).p1, arc(currarc%).p2
    END IF
IF son = 0 THEN SOUND -100 + 200 + timecnt% * 3 + (RND * 200), .025
FOR dee = 1 TO 5
IF son = 1 THEN sbplay1 0 + timecnt% * 3 + (RND * 200), 1, 2, 0, 0

NEXT
IF INT(RND * 60) + 1 = 60 THEN
  FOR de = 600 + INT(RND * 50) TO 100 + INT(RND * 50) STEP -60 + INT(RND * 40)
IF son = 0 THEN SOUND -100 + 200 + de * 3 + (RND * 200), .025
FOR dee = 1 TO 5
IF son = 1 THEN sbplay1 0 + de * 3 + (RND * 200), 1, 2, 0, 0
NEXT
NEXT
  END IF
  NEXT currarc%
  't! = TIMER: WHILE t! = TIMER: WEND ' Pause
  IF timecnt% >= boomSize THEN quitExplode = 1
LOOP UNTIL quitExplode > 0
 PUT (x1, Y1), ExplodeHold(1), PSET

'PALETTE 101, 65536 * 59 + 256 * 59 + 59
IF son = 1 THEN CALL WriteReg(&HB0, &H0)

END SUB

SUB GorauFill (Points() AS RoutineType)

DIM Scan(0 TO 320) AS GorType

XMax% = -32767
Xmin% = 32767

FOR X% = 0 TO 3
  IF XMax% < Points(X%).X THEN XMax% = Points(X%).X
  IF Xmin% > Points(X%).X THEN Xmin% = Points(X%).X
  PSET (Points(X%).X, Points(X%).y), 230
NEXT


 IF Xmin% < 0 THEN Xmin% = 0
 IF XMax% > 320 THEN XMax% = 320
 IF Xmin% > 320 THEN EXIT SUB
 IF XMax% < 0 THEN EXIT SUB

FOR X% = Xmin% TO XMax%
  Scan(X%).Y1 = -32767
NEXT



FOR X% = 0 TO 3

  Val1% = X%
  Val2% = (X% + 1) MOD 4

    IF Points(Val1%).X > Points(Val2%).X THEN
      SWAP Val1%, Val2%
    END IF
 
  Y1% = Points(Val1%).y
  x1% = Points(Val1%).X
  Col1% = Points(Val1%).C

  Y2% = Points(Val2%).y
  x2% = Points(Val2%).X
  Col2% = Points(Val2%).C

  YDelta% = Y2% - Y1%
 
  XDelta% = x2% - x1%
  CDelta% = Col2% - Col1%
 
  IF XDelta% <> 0 THEN
   YSlope! = YDelta% / XDelta%
   CSlope! = CDelta% / XDelta%
  ELSE
   YSlope% = 0
   CSlope% = 0
  END IF

  YVal! = Y1%
  CVal! = Col1%
 
      FOR I% = x1% TO x2%
        IF Scan(I%).Y1 = -32767 THEN
          Scan(I%).Y1 = YVal!
          Scan(I%).C1 = CVal!
        ELSE
          Scan(I%).Y2 = YVal!
          Scan(I%).C2 = CVal!
        END IF

        YVal! = YVal! + YSlope!
        CVal! = CVal! + CSlope!
     
      NEXT

NEXT



FOR X% = Xmin% TO XMax%

  IF Scan(X%).Y1 > Scan(X%).Y2 THEN
    Y2% = Scan(X%).Y1
    Y1% = Scan(X%).Y2
    Col2% = Scan(X%).C1
    Col1% = Scan(X%).C2
  ELSE
    Y2% = Scan(X%).Y2
    Y1% = Scan(X%).Y1
    Col2% = Scan(X%).C2
    Col1% = Scan(X%).C1
  END IF

  YDelta% = Y2% - Y1%
 
  IF YDelta% = 0 THEN YDelta% = 1

  CDelta% = Col2% - Col1%
  CSlope! = CDelta% / YDelta%
  CVal! = Col1%

    FOR I% = Scan(X%).Y1 TO Scan(X%).Y2
      PSET (X%, I%), CVal!
      CVal! = CVal! + CSlope!
    NEXT

NEXT



END SUB

SUB help
a = 1
COLOR 60
CLS
OPEN "sw1help.txt" FOR INPUT AS #1
33
INPUT #1, a$, lin
a = a + lin
PRINT a$
IF a$ = "*" THEN PRINT "END OF TEXT": GOTO 34
IF a < 21 THEN 33
WHILE INKEY$ = "": WEND
a = 1
CLS
GOTO 33
34
CLOSE #1
WHILE INKEY$ = "": WEND

END SUB

SUB junk
'PUT (160 + ex * zoom, 100 + ey * zoom), ebk, PSET
'PUT (0, 100), bk, PSET
'FOR b = 0 TO .005 STEP .001
'LINE (160 + ex * (zoom - b), 100 + ey * (zoom - b))-(160 + ex * (zoom - b), 100 + (ey + 100) * (zoom - b)), 220
'LINE (160 + (ex + 100) * (zoom - b), 100 + ey * (zoom - b))-(160 + (ex + 100) * (zoom - b), 100 + (ey + 100) * (zoom - b)), 220
'LINE (160 + ex * (zoom - b), 100 + (ey + 50) * (zoom - b))-(160 + (ex + 100) * (zoom - b), 100 + (ey + 50) * (zoom - b)), 220
'FOR c = 1 TO 20
'CIRCLE (160 + (ex + 50) * (zoom - b), 100 + (ey + 50) * (zoom - b)), c * zoom, 220
'NEXT
''IF zoom < .3 THEN
''PAINT (160 + (ex + 50) * (zoom - b), 100 + (ey + 47) * (zoom - b)), 220
''PAINT (160 + (ex + 50) * (zoom - b), 100 + (ey + 53) * (zoom - b)), 220
''END IF
''NEXT
''you
'LINE (x, y)-(x + 50, y + 20), 60
'LINE (x, y + 20)-(x + 50, y), 60
'LINE (x + 15, y + 5.999999)-(x + 35, y + 15.999999#), 60, BF

END SUB

SUB menu (sx(), sy(), cl(), outcome)
xx = -350
yy = 220
'zoom = 1

FOR zoom1 = 1 TO 0 STEP -.01
  LINE (300 + (xx * zoom1), yy * zoom1)-(300 + (xx + 50) * zoom1, (yy + 20) * zoom1), 60
  LINE (300 + xx * zoom1, (yy + 20) * zoom1)-(300 + (xx + 50) * zoom1, yy * zoom1), 60
  FOR a = 1 TO 5
    CIRCLE (300 + (xx + 15) * zoom1, (yy + 10) * zoom1), a * zoom1, 150
    CIRCLE (300 + (xx + 35) * zoom1, (yy + 10) * zoom1), a * zoom1, 150
  NEXT
  IF INT(RND * 50) + 1 = 1 THEN
    f = 1
    LINE (300 + (xx * zoom1), yy * zoom1)-(300, 0), 190
    LINE (300 + ((xx + 50) * zoom1), yy * zoom1)-(300, 0), 190
    LINE (300 + (xx * zoom1), (yy + 20) * zoom1)-(300, 0), 190
    LINE (300 + ((xx + 50) * zoom1), (yy + 20) * zoom1)-(300, 0), 190
    s = -10
    s1 = 10
    FOR a = 1 TO 1000
      s = s + 1
      s1 = s1 - 1
      IF s = 100 THEN s = -100
      IF s1 = -100 THEN s1 = 100
    IF son = 1 THEN sbplay1 100 + s, 5, 6, 5, 0
    IF son = 1 THEN sbplay1 100 + s1, 5, 6, 5, 0
    NEXT
    IF son = 1 THEN sbplay1 100 + s1, 5, 0, 0, 0
    IF son = 1 THEN CALL WriteReg(&HB0, &H0)
  END IF
IF f = 0 THEN
  tim = TIMER + .05
  WHILE TIMER < tim: WEND
END IF
 zoom = 1: sy = 0
FOR a = 1 TO 150
PSET (sx(a), sy(a)), cl(a) * 10
NEXT
X = sx - 100
y = sy
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + (y * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X) * zoom), 50 + ((y) * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 254
LINE (160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 20) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 254

X = sx - 78
LINE (160 + ((X) * zoom), 50 + (sy * zoom))-(160 + ((X + 20) * zoom), 50 + (sy * zoom)), 254
LINE (160 + ((X + 10) * zoom), 50 + (sy * zoom))-(160 + ((X + 10) * zoom), 50 + ((sy + 20) * zoom)), 254

X = sx - 56
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + (y * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 254
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X) * zoom), 50 + ((y + 20) * zoom)), 254
LINE (160 + ((X + 20) * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 254

X = sx - 34
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + (y * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 254
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X) * zoom), 50 + ((y + 20) * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 254
LINE (160 + ((X + 20) * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 254

X = sx + 14

LINE (160 + ((X) * zoom), 50 + (sy * zoom))-(160 + ((X + 5) * zoom), 50 + ((sy + 20) * zoom)), 254
LINE (160 + ((X + 5) * zoom), 50 + ((sy + 20) * zoom))-(160 + ((X + 10) * zoom), 50 + ((sy + 10) * zoom)), 254
LINE (160 + ((X + 10) * zoom), 50 + ((sy + 10) * zoom))-(160 + ((X + 15) * zoom), 50 + ((sy + 20) * zoom)), 254
LINE (160 + ((X + 15) * zoom), 50 + ((sy + 20) * zoom))-(160 + ((X + 20) * zoom), 50 + (sy * zoom)), 254

X = sx + 36
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + (y * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 254
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X) * zoom), 50 + ((y + 20) * zoom)), 254
LINE (160 + ((X + 20) * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 254

X = sx + 58
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + (y * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 254
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X) * zoom), 50 + ((y + 20) * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 254
LINE (160 + ((X + 20) * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 254

X = sx + 80
y = sy
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + (y * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X) * zoom), 50 + ((y) * zoom)), 254

LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 254
LINE (160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 20) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 254

  LINE (300 + (xx * zoom1), yy * zoom1)-(300 + (xx + 50) * zoom1, (yy + 20) * zoom1), 0
  LINE (300 + xx * zoom1, (yy + 20) * zoom1)-(300 + (xx + 50) * zoom1, yy * zoom1), 0
  FOR a = 1 TO 5
    CIRCLE (300 + (xx + 15) * zoom1, (yy + 10) * zoom1), a * zoom1, 0
    CIRCLE (300 + (xx + 35) * zoom1, (yy + 10) * zoom1), a * zoom1, 0
  NEXT
  IF f = 1 THEN
  f = 0
    LINE (300 + (xx * zoom1), yy * zoom1)-(300, 0), 0
    LINE (300 + ((xx + 50) * zoom1), yy * zoom1)-(300, 0), 0
    LINE (300 + (xx * zoom1), (yy + 20) * zoom1)-(300, 0), 0
    LINE (300 + ((xx + 50) * zoom1), (yy + 20) * zoom1)-(300, 0), 0
  END IF

NEXT
ocenter "EPISODE ONE: SUICIDE MISSON", 75, 150, 125
ocenter "PLAY GAME", 100, 40, 30
ocenter "INTRODUCTION", 120, 40, 30
ocenter "HELP", 140, 40, 30
ocenter "EXIT", 160, 40, 30

y = 100
WHILE hit = 0
SELECT CASE INKEY$
  CASE "8", CHR$(0) + "H": y = y - 20
  CASE "2", CHR$(0) + "P": y = y + 20
  CASE " ": hit = 1
END SELECT
IF y < 100 THEN y = 160
IF y > 160 THEN y = 100
IF y = 100 THEN
  ocenter "PLAY GAME", 100, 60, 50
  ocenter "INTRODUCTION", 120, 40, 30
  ocenter "HELP", 140, 40, 30
  ocenter "EXIT", 160, 40, 30
END IF
IF y = 120 THEN
  ocenter "PLAY GAME", 100, 40, 30
  ocenter "INTRODUCTION", 120, 60, 50
  ocenter "HELP", 140, 40, 30
  ocenter "EXIT", 160, 40, 30
END IF
IF y = 140 THEN
  ocenter "PLAY GAME", 100, 40, 30
  ocenter "INTRODUCTION", 120, 40, 30
  ocenter "HELP", 140, 60, 50
  ocenter "EXIT", 160, 40, 30
END IF
IF y = 160 THEN
  ocenter "PLAY GAME", 100, 40, 30
  ocenter "INTRODUCTION", 120, 40, 30
  ocenter "HELP", 140, 40, 30
  ocenter "EXIT", 160, 60, 50
END IF
WEND
IF y = 100 THEN outcome = 1
IF y = 120 THEN outcome = 2
IF y = 140 THEN outcome = 3
IF y = 160 THEN outcome = 4

END SUB

DEFINT A-Z
SUB ocenter (text$, y, Fore, back)
ofont text$, 160 - INT(LEN(text$) * 4), y, Fore, back 'Call with appropriate
END SUB                                              'Starting X coordinate

SUB ofont (text$, X, y, Fore, back)
DEF SEG = &HFFA6                              'Stores masks for letters
FOR Letter = 1 TO LEN(text$)                  'Does each letter
Address = (8 * ASC(MID$(text$, Letter))) + 14 'Address for start of letter
FOR Height = 0 TO 7                       'Each letter is an 8x8 pixel matrix
Mask = PEEK(Address + Height) * 128   'Address for mask of each line of letter
LINE (X + Curntx + 1, y + Height + 1)-(X + 9 + Curntx, y + Height + 1), Fore, , Mask
NEXT
Curntx = Curntx + 8                   'Advances X axis by 8 for next letter
NEXT                                  'Continue to next letter
DEF SEG = &HA000                      'Change to video memory
IF back > 0 THEN                      'Background color can't be color 0
FOR V = y TO y + 7                    'Again, they're 8x8 pixels
FOR H = X TO (LEN(text$) * 8) - 1 + X 'Calculates length of text in pixels
PK0& = PEEK(H + V * 320&)             'Is point at H,V = to foreground color?
PK1& = PEEK(H + 1 + (V + 1) * 320&)   'Is point at H+1, V+1 = to foreground?
PK2& = PEEK(H + 1 + V * 320&)        'Is point at H+1, V = to foreground?
PT& = H + V * 320&                    'Video memory pointer
IF PK0& <> Fore THEN                  'If this is foreground, don't overlap it
IF PK1& = Fore OR PK2& = Fore THEN POKE PT&, back    'Put pixel into memory
END IF
NEXT H     'Next horizontal
NEXT V     'Next vertical
END IF
DEF SEG    'Put us back where
END SUB    'We started

DEFSNG A-Z
SUB quit
CLS
' Gorau Shaded Polygons.  Written by Luke Molnar.  A Molnar \ Kucalaba
' Productions program.  Tons of concept and execution help provided by
' David Eichorn.  If you are one of those people that need really obvious
' things pointed out, this is REALLY SLOW!  I don't think many people in
' their right mind would try to write a Gorau routine in QBasic and demand
' that it be fast enough for animation, but this is pretty nice looking
' if I do say so myself. :-)


' Another excellent QBasic graphics "demo" that appeared first at :

' WWW : http://members.aol.com/mkwebsite/index.html
' FTP : ftp://users.aol.com/blood225/

'$DYNAMIC

CLS
SCREEN 13

COLOR 230

DO
timmm = timmm + 1
     ThisShouldBeASub% = ValidPolPoints ' Generate decent polygon points

     ChangeColors                       ' Randomly assign a color scheme

     GorauFill PolPoints()              ' Fill the 4 point polygon

     'Delay 1                            ' Give user time to admire the beauty                      

  User$ = INKEY$                        ' Store any keypresses

  'CLS                                   ' Clear the screen

LOOP UNTIL timmm = 5                ' Keep going until they press a key
ocenter "This Game Brought to you", 40, 180, 220
ocenter "By", 50, 180, 220
ocenter "SWITCHBLADE PROGRAMMING CORP", 80, 180, 220
ocenter "Nicholas Klaebe              Tim Stoakes", 150, 180, 220
ofont "EMAIL rakoth@ezinet.com.au", 0, 160, 180, 220
WHILE INKEY$ = "": WEND
CLS
SCREEN 0
WIDTH 80
SYSTEM

END
END SUB

REM $STATIC
DEFINT A-Z
SUB SB
SCREEN 9

COLOR 4
CONST false = 0, true = NOT false
IF DetectCard = true THEN
LOCATE 1, 1: PRINT "Sound Blaster (Compatible) Card Detected"
FOR I = 1 TO 30000
NEXT I
SBINIT
END IF
DEF SEG = 0
SCH% = 4: 'Number of Sound Channels
LOCATE 2, 1: PRINT "Sound Card Initialized"
END SUB

SUB SBINIT
FOR Q = 1 TO &HF5
CALL WriteReg(Q, 0)
NEXT Q
END SUB

DEFSNG A-Z
SUB sbplay (ply$, length)
C = 262
C# = 278
d = 294
d# = 311
e = 300: 'e = 329
e# = 338
f = 349
f# = 370
g = 392
g# = 416
a = 440
a# = 467
B = 494
B# = 508
ply$ = UCASE$(ply$)
SELECT CASE ply$
CASE "A": note = a' + 150
CASE "B": note = B' + 150
CASE "C": note = C' + 150
CASE "D": note = e' + 150
CASE "E": note = d' + 150
CASE "F": note = f' + 150
CASE "G": note = g' + 150
CASE "P":  note = 0: IF son = 1 THEN sbplay1 note + 1, 1, 2, 2, 1: CALL WriteReg(&HB0, &H0):
END SELECT
IF note > 0 THEN
IF son = 1 THEN sbplay1 note + 1, 1, 3, 4, 1 ELSE SOUND note, 1 / length
END IF

FOR C = 1 TO ((mspd * 5) / length)
NEXT
' sbplay1 100, 1, 2, 2, 1:
' CALL WriteReg(&HB0, &H0):

IF son = 1 THEN CALL WriteReg(&HB0, &H0):
END SUB

DEFINT A-Z
SUB sbplay1 (Freq%, Wave%, Feedback%, Modl%, Clen%)
                                'Channel 1
                                'Operator 1
CALL WriteReg(&H20, Modl%)      '&H51
CALL WriteReg(&H40, 10)         '49
CALL WriteReg(&H60, &H40)       '&HF0
CALL WriteReg(&H80, &H240)      '&H77
CALL WriteReg(&HA0, Freq%)      'Freq%
                                'Operator 2
CALL WriteReg(&H23, 1)          '
CALL WriteReg(&H43, 0)          '49
CALL WriteReg(&H63, &HF0)       '
CALL WriteReg(&H83, Clen%)      'CLEN%
CALL WriteReg(&HB0, &H31)       '&H31
                                                    
CALL WriteReg(&HE0, Wave%)      '0 or 1
CALL WriteReg(&HC0, Feedback%)  '

END SUB

DEFSNG A-Z
SUB start (sx(), sy(), cl(), outcome)
CLS
sy = 10
zoom = 4
WHILE zoom > .05
IF dir = 0 THEN sy = sy - (1 - ((4 - zoom) / 1)): IF sy = -.5 THEN dir = 1
IF dir = 1 THEN sy = sy + .5: IF sy = 10.5 THEN dir = 1
FOR a = 1 TO 150
PSET (sx(a), sy(a)), cl(a) * 10
NEXT
X = sx - 100
y = sy
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + (y * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X) * zoom), 50 + ((y) * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 254
LINE (160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 20) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 254

X = sx - 78
LINE (160 + ((X) * zoom), 50 + (sy * zoom))-(160 + ((X + 20) * zoom), 50 + (sy * zoom)), 254
LINE (160 + ((X + 10) * zoom), 50 + (sy * zoom))-(160 + ((X + 10) * zoom), 50 + ((sy + 20) * zoom)), 254

X = sx - 56
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + (y * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 254
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X) * zoom), 50 + ((y + 20) * zoom)), 254
LINE (160 + ((X + 20) * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 254

X = sx - 34
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + (y * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 254
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X) * zoom), 50 + ((y + 20) * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 254
LINE (160 + ((X + 20) * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 254

X = sx + 14

LINE (160 + ((X) * zoom), 50 + (sy * zoom))-(160 + ((X + 5) * zoom), 50 + ((sy + 20) * zoom)), 254
LINE (160 + ((X + 5) * zoom), 50 + ((sy + 20) * zoom))-(160 + ((X + 10) * zoom), 50 + ((sy + 10) * zoom)), 254
LINE (160 + ((X + 10) * zoom), 50 + ((sy + 10) * zoom))-(160 + ((X + 15) * zoom), 50 + ((sy + 20) * zoom)), 254
LINE (160 + ((X + 15) * zoom), 50 + ((sy + 20) * zoom))-(160 + ((X + 20) * zoom), 50 + (sy * zoom)), 254

X = sx + 36
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + (y * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 254
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X) * zoom), 50 + ((y + 20) * zoom)), 254
LINE (160 + ((X + 20) * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 254

X = sx + 58
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + (y * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 254
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X) * zoom), 50 + ((y + 20) * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 254
LINE (160 + ((X + 20) * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 254

X = sx + 80
y = sy
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + (y * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X) * zoom), 50 + ((y) * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 254
LINE (160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 254
LINE (160 + (X * zoom), 50 + ((y + 20) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 254

IF INKEY$ = "q" THEN GOTO 23
IF INKEY$ = "" THEN
  tim = TIMER + .05
  WHILE TIMER < tim: WEND
END IF
X = sx - 100
y = sy
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + (y * zoom)), 0
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X) * zoom), 50 + ((y) * zoom)), 0
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 0
LINE (160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 0
LINE (160 + (X * zoom), 50 + ((y + 20) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 0

X = sx - 78
LINE (160 + ((X) * zoom), 50 + (sy * zoom))-(160 + ((X + 20) * zoom), 50 + (sy * zoom)), 0
LINE (160 + ((X + 10) * zoom), 50 + (sy * zoom))-(160 + ((X + 10) * zoom), 50 + ((sy + 20) * zoom)), 0

X = sx - 56
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + (y * zoom)), 0
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 0
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X) * zoom), 50 + ((y + 20) * zoom)), 0
LINE (160 + ((X + 20) * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 0

X = sx - 34
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + (y * zoom)), 0
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 0
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X) * zoom), 50 + ((y + 20) * zoom)), 0
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 0
LINE (160 + ((X + 20) * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 0
                                                                                                  

X = sx + 14

LINE (160 + ((X) * zoom), 50 + (sy * zoom))-(160 + ((X + 5) * zoom), 50 + ((sy + 20) * zoom)), 0
LINE (160 + ((X + 5) * zoom), 50 + ((sy + 20) * zoom))-(160 + ((X + 10) * zoom), 50 + ((sy + 10) * zoom)), 0
LINE (160 + ((X + 10) * zoom), 50 + ((sy + 10) * zoom))-(160 + ((X + 15) * zoom), 50 + ((sy + 20) * zoom)), 0
LINE (160 + ((X + 15) * zoom), 50 + ((sy + 20) * zoom))-(160 + ((X + 20) * zoom), 50 + (sy * zoom)), 0

X = sx + 36
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + (y * zoom)), 0
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 0
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X) * zoom), 50 + ((y + 20) * zoom)), 0
LINE (160 + ((X + 20) * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 0

X = sx + 58
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + (y * zoom)), 0
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 0
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X) * zoom), 50 + ((y + 20) * zoom)), 0
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 0
LINE (160 + ((X + 20) * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 0

X = sx + 80
y = sy
LINE (160 + (X * zoom), 50 + (y * zoom))-(160 + ((X + 20) * zoom), 50 + (y * zoom)), 0
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X) * zoom), 50 + ((y) * zoom)), 0
LINE (160 + (X * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom)), 0
LINE (160 + ((X + 20) * zoom), 50 + ((y + 10) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 0
LINE (160 + (X * zoom), 50 + ((y + 20) * zoom))-(160 + ((X + 20) * zoom), 50 + ((y + 20) * zoom)), 0



zoom = zoom - .02
WEND
23
CALL menu(sx(), sy(), cl(), outcome)
END SUB

FUNCTION ValidPolPoints

' This polygons must be convex in order for this to work properly, so this
' generates one point within each quarter of the screen, which might give
' one the incorrect impression that this routine is somewhat limited.

FOR I% = 0 TO 3
 
  SELECT CASE I%
    CASE 0:
           PolPoints(0).X = INT(RND * 60) + 1
           PolPoints(0).y = INT(RND * 60) + 1
    CASE 1:
           PolPoints(1).X = INT(RND * 160) + 160
           PolPoints(1).y = INT(RND * 100) + 2
'           IF PolPoints(1).Y < PolPoints(0).Y THEN PolPoints(1).Y = PolPoints(0).Y + INT(RND * (100 - PolPoints(0).Y))
    CASE 2:
           PolPoints(2).X = INT(RND * 160) + 160
'           IF PolPoints(2).X < PolPoints(1).X THEN PolPoints(2).X = PolPoints(1).X + 1
           PolPoints(2).y = INT(RND * 50) + 150
    CASE 3:
           PolPoints(3).X = INT(RND * 160) + 1
           IF PolPoints(3).X < PolPoints(0).X THEN PolPoints(3).X = PolPoints(0).X + INT(RND * (160 - PolPoints(0).X))
           PolPoints(3).y = INT(RND * 100) + 100
'           IF PolPoints(3).Y < PolPoints(2).Y THEN PolPoints(3).Y = PolPoints(2).Y + INT(RND * (100 - PolPoints(2).Y))
  END SELECT

 PolPoints(I%).C = -1
'           PolPoints(0).X = 0
'           PolPoints(0).y = 0
'           PolPoints(1).X = 0
'           PolPoints(1).y = 199
'           PolPoints(2).X = 319
'           PolPoints(2).y = 0
'           PolPoints(3).X = 319
'           PolPoints(3).y = 199
          
           NEXT

 
' Make one corner black and one corner the brightest color; the other 2 random
  
   DO UNTIL BlackOne% <> BrightOne%
     BlackOne% = INT(RND * 3)
     BrightOne% = INT(RND * 3)
   LOOP

  PolPoints(BlackOne%).C = 0
  PolPoints(BrightOne%).C = 230

  FOR I% = 0 TO 3
   IF PolPoints(I%).C = -1 THEN PolPoints(I%).C = INT(RND * 230) + 1
  NEXT

END FUNCTION

DEFINT A-Z
SUB WriteReg (Reg%, Value%)
OUT &H220, Reg%
FOR X = 0 TO writereg.delay1
   a = INP(&H220)

NEXT X
OUT &H223, Value%

FOR X = 0 TO writereg.delay2
   a = INP(&H220)

NEXT X

END SUB

