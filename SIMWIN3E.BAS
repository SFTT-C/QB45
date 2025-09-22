DECLARE SUB wind (x1%, y1%, x2%, y2%, title$, menu$, style%, textonoff%, mainwind%)
DECLARE SUB infologo (x%, y%)
DECLARE SUB vect (text$, size!, x%, y%, col%)
DECLARE SUB button (x1%, y1%, x2%, y2%, text$, inout%)
DECLARE SUB pulldownmenu (x%, y%, row%, col%)
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SIMWIN Version D3.0 (C) 1996 Thomas Hirsch                              บ
'บ This is a program imitating windows 3.11 in following points.           บ
'บ * Looks like W3.11                                                      บ
'บ * Windows sizeable and maximizeable                                     บ
'บ * Entire Menu available. Quit and Info executable                       บ
'บ And it imitates Win95 in the following point.                           บ
'บ * Is as slow that it needs a pentium for real performance.              บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
DEFINT A-Z
COMMON row%, col%, rows%, cols%, outv
DEF SEG = 0
M.seg& = 256 * PEEK(207) + PEEK(206)
M.info% = 256 * PEEK(205) + PEEK(204) + 2
DEF SEG = M.seg&
IF (M.seg& OR (M.info% - 2)) AND PEEK(M.info% - 2) = 207 THEN
COLOR 4: PRINT "No mouse found. You must reinstall Windows ;)"
END IF

SCREEN 12
CALL absolute(0, 0, 0, 0, M.info%)
DIM SHARED font(127, 53, 3)
OPEN "d:\009th\vectfont.dat" FOR INPUT AS #1: CLOSE
OPEN "vectfont.dat" FOR BINARY AS #1
FOR ltr = 0 TO 127
ln = ASC(INPUT$(1, #1))
font(ltr, 0, 0) = ln
FOR a = 1 TO ln
start = ASC(INPUT$(1, #1))
ziel = ASC(INPUT$(1, #1))
stx = start \ 16: zx = ziel \ 16
sty = start - stx * 16: zy = ziel - zx * 16
font(ltr, a, 0) = stx
font(ltr, a, 1) = sty
font(ltr, a, 2) = zx
font(ltr, a, 3) = zy
NEXT
NEXT
CLOSE #1

DIM inact&(105)
OPEN "winactiv.iko" FOR BINARY AS #1
FOR a = 0 TO 105
GET #1, a * 4 + 1, inact&(a)
NEXT
CLOSE #1

pmx1 = 0: pmy1 = 0: pmx2 = 639: pmy2 = 479: pmtit$ = "Program Manager": pmmenu$ = "File   Options   Window   Help"
DIM x1(3), y1(3), x2(3), y2(3), tit(3) AS STRING, act(3)
x1(3) = 50: y1(3) = 50: x2(3) = 500: y2(3) = 300: tit(3) = "Main": act(3) = 1
x1(1) = 80: y1(1) = 320: x2(1) = 200: y2(1) = 450: tit(1) = "Applications": act(1) = 0
x1(2) = 300: y1(2) = 320: x2(2) = 500: y2(2) = 450: tit(2) = "Accessories": act(2) = 0
x1(0) = 10: y1(0) = 40: x2(0) = 620: y2(0) = 80: tit(0) = "Games": act(0) = 0
anzwin = 4

too = 1: GOSUB refresh
CALL absolute(4, 0, 320, 240, M.info%)
CALL absolute(1, 0, 0, 0, M.info%)

DO
CALL absolute(3, mbut%, xpos%, ypos%, M.info%)
IF mbut% = 1 THEN GOSUB whatsup
LOOP
SYSTEM

whatsup:
IF xpos% < pmx1 OR xpos% > pmx2 OR ypos% < pmy1 OR ypos% > pmy2 THEN RETURN
'resize tags ;)
IF xpos% <= pmx1 + 4 THEN
CALL absolute(2, 0, 0, 0, M.info%)
DO
CALL absolute(3, mbut%, xpos%, ypos%, M.info%)
IF xpos% < pmx2 - 140 THEN pmx1 = xpos%: too = 0: GOSUB refresh
LOOP WHILE mbut% = 1
too = 1: GOSUB refresh
CALL absolute(1, 0, 0, 0, M.info%)
RETURN
END IF
IF xpos% >= pmx2 - 4 THEN
CALL absolute(2, 0, 0, 0, M.info%)
DO
CALL absolute(3, mbut%, xpos%, ypos%, M.info%)
IF xpos > pmx1 + 140 THEN pmx2 = xpos%: too = 0: GOSUB refresh
LOOP WHILE mbut% = 1
too = 1: GOSUB refresh
CALL absolute(1, 0, 0, 0, M.info%)
RETURN
END IF
IF ypos% <= pmy1 + 4 THEN
CALL absolute(2, 0, 0, 0, M.info%)
DO
CALL absolute(3, mbut%, xpos%, ypos%, M.info%)
IF ypos% < pmy2 - 40 THEN pmy1 = ypos%: too = 0: GOSUB refresh
LOOP WHILE mbut% = 1
too = 1: GOSUB refresh
CALL absolute(1, 0, 0, 0, M.info%)
RETURN
END IF
IF ypos% >= pmy2 - 4 THEN
CALL absolute(2, 0, 0, 0, M.info%)
DO
CALL absolute(3, mbut%, xpos%, ypos%, M.info%)
IF ypos% > pmy1 + 40 THEN pmy2 = ypos%: too = 0: GOSUB refresh
LOOP WHILE mbut% = 1
too = 1: GOSUB refresh
CALL absolute(1, 0, 0, 0, M.info%)
RETURN
END IF
'move wins
IF xpos% > pmx1 + 19 AND xpos% < pmx2 - 35 AND ypos% > pmy1 + 4 AND ypos% <= pmy1 + 19 THEN
xposa = xpos: yposa = ypos: pmx1a = pmx1: pmx2a = pmx2: pmy1a = pmy1: pmy2a = pmy2
CALL absolute(2, 0, 0, 0, M.info%)
DO
CALL absolute(3, mbut%, xpos%, ypos%, M.info%)
IF pmx1a - xposa + xpos% >= 0 AND pmx2a - xposa + xpos% < 640 AND pmy1a - yposa + ypos% >= 0 AND pmy2a - yposa + ypos% < 480 THEN
pmx1 = pmx1a - xposa + xpos%
pmx2 = pmx2a - xposa + xpos%
pmy1 = pmy1a - yposa + ypos%
pmy2 = pmy2a - yposa + ypos%
too = 0: GOSUB refresh
END IF
LOOP WHILE mbut% = 1
too = 1: GOSUB refresh
CALL absolute(1, 0, 0, 0, M.info%)
RETURN
END IF
'quit
IF xpos% > pmx1 + 4 AND ypos% > pmy1 + 4 AND xpos% < pmx1 + 20 AND ypos% < pmy1 + 20 THEN
endwin:
CALL absolute(2, 0, 0, 0, M.info%)
wind 100, 150, 540, 250, "Exit Windows", "", 1, 1, 1
infologo 135, 195
vect "Do you want to exit Windows ?", 1, 170, 190, 0
button 200, 210, 280, 230, "OK", 0
button 310, 210, 390, 230, "Cancel", 0
CALL absolute(1, 0, 0, 0, M.info%)
DO
CALL absolute(3, mbut%, xpos%, ypos%, M.info%)
IF mbut% = 1 THEN
IF xpos% > 200 AND xpos% < 280 AND ypos% > 210 AND ypos% < 230 THEN
CALL absolute(2, 0, 0, 0, M.info%)
button 200, 210, 280, 230, "OK", 1
CALL absolute(1, 0, 0, 0, M.info%)
SYSTEM
END IF
IF (xpos% > 310 AND xpos% < 390 AND ypos% > 210 AND ypos% < 230) OR (xpos% > 100 AND xpos% < 119 AND ypos% > 150 AND ypos% < 169) THEN
CALL absolute(2, 0, 0, 0, M.info%)
button 310, 210, 390, 230, "Cancel", 1
too = 1: GOSUB refresh
CALL absolute(1, 0, 0, 0, M.info%)
EXIT DO
END IF
END IF
LOOP
RETURN
END IF
'maximize (?)
IF xpos% < pmx2 - 4 AND xpos% > pmx2 - 19 AND ypos% < pmy1 + 19 AND ypos% > pmy1 + 4 THEN
CALL absolute(2, 0, 0, 0, M.info%)
pmx1 = 0: pmy1 = 0: pmx2 = 639: pmy2 = 479
too = 1: GOSUB refresh
CALL absolute(1, 0, 0, 0, M.info%)
RETURN
END IF

'menus
IF xpos% > pmx1 + 4 AND ypos% > pmy1 + 19 AND xpos% < pmx1 + 44 AND ypos% < pmy1 + 37 THEN
CALL absolute(2, 0, 0, 0, M.info%)
IF menu > 1 THEN too = 1: GOSUB refresh
LINE (pmx1 + 4, pmy1 + 20)-(pmx1 + 10 + 6 * 8, pmy1 + 36), 1, BF
vect "File", 1, pmx1 + 12, pmy1 + 21, 15
pulldownmenu pmx1 + 5, pmy1 + 38, 6, 18
pulldownmenu pmx1 + 5, pmy1 + 38 + 6 * 16, 1, 18
pulldownmenu pmx1 + 5, pmy1 + 38 + 7 * 16, 1, 18
vect "New...", 1, pmx1 + 7, pmy1 + 40, 0
vect "Open", 1, pmx1 + 7, pmy1 + 40 + 16, 0
vect "Move...", 1, pmx1 + 7, pmy1 + 40 + 2 * 16, 0
vect "Copy", 1, pmx1 + 7, pmy1 + 40 + 3 * 16, 0
vect "Delete", 1, pmx1 + 7, pmy1 + 40 + 4 * 16, 0
vect "Properties...", 1, pmx1 + 7, pmy1 + 40 + 5 * 16, 0
vect "Run...", 1, pmx1 + 7, pmy1 + 40 + 6 * 16, 0
vect "Exit Windows...", 1, pmx1 + 7, pmy1 + 40 + 7 * 16, 0
menu = 1
CALL absolute(1, 0, 0, 0, M.info%)
RETURN
END IF
IF xpos% > pmx1 + 4 + 7 * 8 AND ypos% > pmy1 + 19 AND xpos% < pmx1 + 4 + 15 * 8 AND ypos% < pmy1 + 37 THEN
CALL absolute(2, 0, 0, 0, M.info%)
IF menu <> 0 AND menu <> 2 THEN too = 1: GOSUB refresh
LINE (pmx1 + 4 + 7 * 8, pmy1 + 20)-(pmx1 + 10 + 16 * 8, pmy1 + 36), 1, BF
vect "Options", 1, pmx1 + 12 + 7 * 8, pmy1 + 21, 15
pulldownmenu pmx1 + 5 + 7 * 8, pmy1 + 38, 3, 36
vect "Auto arrange", 1, pmx1 + 7 + 7 * 8, pmy1 + 40, 0
vect "Minimize on use", 1, pmx1 + 7 + 7 * 8, pmy1 + 40 + 16, 0
vect "Save changes on exit", 1, pmx1 + 7 + 7 * 8, pmy1 + 40 + 2 * 16, 0
menu = 2
CALL absolute(1, 0, 0, 0, M.info%)
RETURN
END IF
IF xpos% > pmx1 + 4 + 17 * 8 AND ypos% > pmy1 + 19 AND xpos% < pmx1 + 4 + 23 * 8 AND ypos% < pmy1 + 37 THEN
CALL absolute(2, 0, 0, 0, M.info%)
IF menu <> 0 AND menu <> 3 THEN too = 1: GOSUB refresh
LINE (pmx1 + 4 + 17 * 8, pmy1 + 20)-(pmx1 + 10 + 24 * 8, pmy1 + 36), 1, BF
vect "Window", 1, pmx1 + 12 + 17 * 8, pmy1 + 21, 15
pulldownmenu pmx1 + 5 + 17 * 8, pmy1 + 38, 3, 16
vect "Cascade", 1, pmx1 + 7 + 17 * 8, pmy1 + 40, 0
vect "Tile", 1, pmx1 + 7 + 17 * 8, pmy1 + 40 + 16, 0
vect "Arrange Icons", 1, pmx1 + 7 + 17 * 8, pmy1 + 40 + 2 * 16, 0
menu = 3
CALL absolute(1, 0, 0, 0, M.info%)
RETURN
END IF
IF xpos% > pmx1 + 4 + 26 * 8 AND ypos% > pmy1 + 19 AND xpos% < pmx1 + 4 + 31 * 8 AND ypos% < pmy1 + 37 THEN
CALL absolute(2, 0, 0, 0, M.info%)
IF menu <> 0 AND menu <> 4 THEN too = 1: GOSUB refresh
LINE (pmx1 + 4 + 26 * 8, pmy1 + 20)-(pmx1 + 10 + 32 * 8, pmy1 + 36), 1, BF
vect "Help", 1, pmx1 + 12 + 26 * 8, pmy1 + 21, 15
pulldownmenu pmx1 + 5 + 26 * 8, pmy1 + 38, 2, 20
pulldownmenu pmx1 + 5 + 26 * 8, pmy1 + 38 + 2 * 16, 2, 20
pulldownmenu pmx1 + 5 + 26 * 8, pmy1 + 38 + 4 * 16, 1, 20
vect "Index", 1, pmx1 + 7 + 26 * 8, pmy1 + 40, 0
vect "Glossary...", 1, pmx1 + 7 + 26 * 8, pmy1 + 40 + 16, 0
vect "Using Help", 1, pmx1 + 7 + 26 * 8, pmy1 + 40 + 2 * 16, 0
vect "Basic Skills", 1, pmx1 + 7 + 26 * 8, pmy1 + 40 + 3 * 16, 0
vect "About Program Manager", 1, pmx1 + 7 + 26 * 8, pmy1 + 40 + 4 * 16, 0
menu = 4
CALL absolute(1, 0, 0, 0, M.info%)
RETURN
END IF

'inside menus
IF xpos% > pmx1 + 7 AND ypos% > pmy1 + 37 AND xpos% < pmx1 + 7 + 18 * 7 AND ypos% < pmy1 + 37 + 8 * 16 AND menu = 1 THEN
IF ypos% > pmy1 + 37 + 7 * 16 THEN GOTO endwin
RETURN
END IF
IF xpos% > pmx1 + 7 + 7 * 8 AND ypos% > pmy1 + 37 AND xpos% < pmx1 + 7 + 43 * 8 AND ypos% < pmy1 + 37 + 8 * 16 AND menu = 2 THEN
RETURN
END IF
IF xpos% > pmx1 + 7 + 17 * 8 AND ypos% > pmy1 + 37 AND xpos% < pmx1 + 7 + 33 * 8 AND ypos% < pmy1 + 37 + 8 * 16 AND menu = 3 THEN
RETURN
END IF
IF xpos% > pmx1 + 7 + 25 * 8 AND ypos% > pmy1 + 37 AND xpos% < pmx1 + 7 + 45 * 8 AND ypos% < pmy1 + 37 + 8 * 16 AND menu = 4 THEN
IF ypos% > pmy1 + 37 + 4 * 16 THEN GOTO info
RETURN
END IF

'subwindows
FOR a = 0 TO anzwin - 1
 IF act(a) = 1 THEN
  IF xpos% > x1(a) AND ypos% > y1(a) AND xpos% < x2(a) AND ypos% < y2(a) THEN
  GOSUB topwindow
  a = anzwin - 1
   'resize tags ;)
   IF xpos% <= x1(a) + 4 THEN
    CALL absolute(2, 0, 0, 0, M.info%)
    DO
     CALL absolute(3, mbut%, xpos%, ypos%, M.info%)
     IF xpos% < x2(a) - 140 AND xpos% > pmx1 + 3 THEN x1(a) = xpos%: lr = 0: GOSUB refresh2
    LOOP WHILE mbut% = 1
    lr = 1: GOSUB refresh2
    CALL absolute(1, 0, 0, 0, M.info%)
    RETURN
   END IF
   IF xpos% >= x2(a) - 4 THEN
    CALL absolute(2, 0, 0, 0, M.info%)
    DO
     CALL absolute(3, mbut%, xpos%, ypos%, M.info%)
     IF xpos% > x1(a) + 140 AND xpos% < pmx2 - 3 THEN x2(a) = xpos%: lr = 0: GOSUB refresh2
    LOOP WHILE mbut% = 1
    lr = 1: GOSUB refresh2
    CALL absolute(1, 0, 0, 0, M.info%)
    RETURN
   END IF
   IF ypos% <= y1(a) + 4 THEN
    CALL absolute(2, 0, 0, 0, M.info%)
    DO
     CALL absolute(3, mbut%, xpos%, ypos%, M.info%)
     IF ypos% < y2(a) - 40 AND ypos% > pmy1 + 37 THEN y1(a) = ypos%: lr = 0: GOSUB refresh2
    LOOP WHILE mbut% = 1
    lr = 1: GOSUB refresh2
    CALL absolute(1, 0, 0, 0, M.info%)
    RETURN
   END IF
   IF ypos% >= y2(a) - 4 THEN
    CALL absolute(2, 0, 0, 0, M.info%)
    DO
     CALL absolute(3, mbut%, xpos%, ypos%, M.info%)
     IF ypos% > y1(a) + 40 AND ypos% < pmy2 - 3 THEN y2(a) = ypos%: lr = 0: GOSUB refresh2
    LOOP WHILE mbut% = 1
    lr = 1: GOSUB refresh2
    CALL absolute(1, 0, 0, 0, M.info%)
    RETURN
   END IF
   IF xpos% < x2(a) - 3 AND ypos% < y1(a) + 19 AND xpos% > x2(a) - 19 AND ypos% > y1(a) + 3 THEN
    CALL absolute(2, 0, 0, 0, M.info%)
    x1(a) = pmx1 + 4
    y1(a) = pmy1 + 38
    x2(a) = pmx2 - 4
    y2(a) = pmy2 - 4
    lr = 1
    GOSUB refresh2
    CALL absolute(1, 0, 0, 0, M.info%)
    RETURN
   END IF
   IF xpos% > x1(a) + 19 AND ypos% > y1(a) + 3 AND ypos% < y1(a) + 19 AND xpos% < x2(a) - 35 THEN
    xposa = xpos: yposa = ypos: x1a = x1(a): x2a = x2(a): y1a = y1(a): y2a = y2(a)
    CALL absolute(2, 0, 0, 0, M.info%)
    DO
     CALL absolute(3, mbut%, xpos%, ypos%, M.info%)
     IF x1a - xposa + xpos% >= 0 AND x2a - xposa + xpos% < 640 AND y1a - yposa + ypos% >= 37 AND y2a - yposa + ypos% < 480 THEN
      x1(a) = x1a - xposa + xpos%
      x2(a) = x2a - xposa + xpos%
      y1(a) = y1a - yposa + ypos%
      y2(a) = y2a - yposa + ypos%
      lr = 0
      GOSUB refresh2
     END IF
    LOOP WHILE mbut% = 1
    lr = 1: GOSUB refresh2
    CALL absolute(1, 0, 0, 0, M.info%)
    RETURN
   END IF
   IF xpos% > x1(a) + 4 AND ypos% > y1(a) + 4 AND xpos% < x1(a) + 20 AND ypos% < y1(a) + 20 THEN
    act(a) = 0
    GOSUB topwindow
    CALL absolute(2, 0, 0, 0, M.info%)
    lr = 1: GOSUB refresh2
    CALL absolute(1, 0, 0, 0, M.info%)
    RETURN
   END IF

  RETURN
  END IF
 ELSE
  IF xpos% > pmx1 + 50 + a * 70 AND xpos% < pmx1 + 80 + a * 70 AND ypos% > pmy2 - 70 AND ypos% < pmy2 - 45 THEN
   act(a) = 1
   GOSUB topwindow
   CALL absolute(2, 0, 0, 0, M.info%)
   lr = 1: GOSUB refresh2
   CALL absolute(1, 0, 0, 0, M.info%)
   RETURN
  END IF
 END IF
NEXT

RETURN

'CALL absolute(1, 0, 0, 0, M.info%)  'Turn on Pointer
'CALL absolute(2, 0, 0, 0, M.info%)  'Turn off Pointer

info:
CALL absolute(2, 0, 0, 0, M.info%)
wind 50, 50, 500, 350, "About Program Manager", "", 1, 1, 1
vect "SIMWIN Version 3.0 E", 1, 120, 120, 0
vect "(C) 1996 Thomas Hirsch. All rights reserved", 1, 120, 136, 0
button 380, 100, 460, 120, "OK", 0

CALL absolute(1, 0, 0, 0, M.info%)
DO
CALL absolute(3, mbut%, xpos%, ypos%, M.info%)
IF mbut% = 1 AND xpos% > 380 AND ypos% > 100 AND xpos% < 460 AND ypos% < 120 THEN
CALL absolute(2, 0, 0, 0, M.info%)
button 380, 100, 460, 120, "OK", 1
GOSUB refresh
CALL absolute(1, 0, 0, 0, M.info%)
RETURN
END IF
LOOP

refresh:
CLS
wind pmx1, pmy1, pmx2, pmy2, pmtit$, pmmenu$, 0, too, 1
menu = 0
IF too = 1 THEN
VIEW SCREEN (pmx1 + 4, pmy1 + 38)-(pmx2 - 4, pmy2 - 4)
FOR a = 0 TO anzwin - 1
IF act(a) = 1 THEN
wind x1(a), y1(a), x2(a), y2(a), tit$(a), "", 2, too, -(a = anzwin - 1)
ELSE
IF pmy2 - pmy1 > 120 AND pmx1 + 50 + a * 70 < pmx2 - 31 THEN PUT (pmx1 + 50 + a * 70, pmy2 - 70), inact&, PSET
vect tit(a), .8, pmx1 + 65 + a * 70 - LEN(tit(a)) * 8 * .8 / 2, pmy2 - 40, 0
END IF
NEXT
VIEW
END IF
RETURN
refresh2:
LINE (pmx1 + 4, pmy1 + 38)-(pmx2 - 4, pmy2 - 4), 15, BF
VIEW SCREEN (pmx1 + 4, pmy1 + 38)-(pmx2 - 4, pmy2 - 4)
IF lr = 1 THEN
FOR i = 0 TO anzwin - 1
IF act(i) = 1 THEN
wind x1(i), y1(i), x2(i), y2(i), tit(i), "", 2, 1, -(i = anzwin - 1)
ELSE
IF pmy2 - pmy1 > 120 AND pmx1 + 50 + a * 70 < pmx2 - 31 THEN PUT (pmx1 + 50 + i * 70, pmy2 - 70), inact&, PSET
vect tit(i), .8, pmx1 + 65 + i * 70 - LEN(tit(i)) * 8 * .8 / 2, pmy2 - 40, 0
END IF
NEXT
ELSE
wind x1(a), y1(a), x2(a), y2(a), tit(a), "", 2, 1, 1
END IF
VIEW
RETURN

topwindow:
x1a = x1(a): y1a = y1(a): x2a = x2(a): y2a = y2(a): tita$ = tit(a): acta = act(a)
FOR i = a TO anzwin - 2
x1(i) = x1(i + 1)
x2(i) = x2(i + 1)
y1(i) = y1(i + 1)
y2(i) = y2(i + 1)
tit(i) = tit(i + 1)
act(i) = act(i + 1)
NEXT
x1(anzwin - 1) = x1a: y1(anzwin - 1) = y1a: x2(anzwin - 1) = x2a: y2(anzwin - 1) = y2a: tit(anzwin - 1) = tita$: act(anzwin - 1) = acta
RETURN

'

SUB button (x1, y1, x2, y2, text$, inout)
IF inout = 0 THEN c1 = 15: c2 = 8 ELSE c1 = 8: c2 = 15
LINE (x1 + 2, y1 + 2)-(x2 - 2, y2 - 2), 7, BF
LINE (x1 + 1, y1 + 1)-(x2 - 1, y2 - 1), 0, B
LINE (x1 + 1, y1)-(x2 - 1, y1), 0
LINE (x1 + 1, y2)-(x2 - 1, y2), 0
LINE (x1, y1 + 1)-(x1, y2 - 1), 0
LINE (x2, y1 + 1)-(x2, y2 - 1), 0
LINE (x2 - 2, y1 + 3)-(x1 + 2, y1 + 2), c1, BF
LINE -(x1 + 3, y2 - 2), c1, BF
LINE (x1 + 2, y2 - 2)-(x2 - 2, y2 - 2), c2
LINE -(x2 - 2, y1 + 2), c2
LINE (x2 - 3, y2 - 3)-(x2 - 3, y2 - 3), c2
LINE -(x2 - 3, y1 + 3), c2
vect text$, 1, x1 + (x2 - x1 - LEN(text$) * 8) / 2, y1 + 3, 0
END SUB

SUB infologo (x, y)
CIRCLE (x, y), 18, 0
PAINT (x, y), 1, 0
LINE (x - 3, y - 5)-(x + 3, y + 14), 15, BF
LINE (x - 7, y - 5)-(x - 3, y - 1), 15, BF
LINE (x - 7, y + 10)-(x + 7, y + 14), 15, BF
CIRCLE (x, y - 13), 3, 15
PAINT (x, y - 13), 15
END SUB

SUB pulldownmenu (x, y, row, col)
LINE (x, y)-(x + col * 8 + 4, y + row * 16 + 4), 15, BF
LINE (x, y)-(x + col * 8 + 4, y + row * 16 + 4), 0, B
LINE (x + 1, y + row * 16 + 5)-(x + col * 8 + 5, y + row * 16 + 5), 7
LINE -(x + col * 8 + 5, y + 1), 7
END SUB

SUB vect (text$, size!, x, y, col)
IF size! = 0 THEN size! = 1

FOR a = 1 TO LEN(text$)
ltr = ASC(MID$(text$, a, 1))
FOR B = 1 TO font(ltr, 0, 0)
LINE (font(ltr, B, 0) * size! + (a - 1) * size! * 8 + x, font(ltr, B, 1) * size! + y)-(font(ltr, B, 2) * size! + (a - 1) * size! * 8 + x, font(ltr, B, 3) * size! + y), col
NEXT
NEXT
END SUB

SUB wind (x1, y1, x2, y2, title$, menu$, style, textonoff, mainwind)
'window
LINE (x1, y1)-(x2, y2), 15, BF
'border
IF style = 0 OR style = 2 THEN bcol = 7 ELSE bcol = 1
LINE (x1, y1)-(x2, y2), 0, B
LINE (x1 + 1, y1 + 1)-(x2 - 1, y2 - 1), bcol, B
LINE (x1 + 2, y1 + 2)-(x2 - 2, y2 - 2), bcol, B
LINE (x1 + 3, y1 + 3)-(x2 - 3, y2 - 3), 0, B
'titlebar
IF mainwind = 0 THEN tc = 15: txc = 0 ELSE tc = 1: txc = 15
LINE (x1 + 4, y1 + 4)-(x2 - 4, y1 + 19), tc, BF
LINE (x1, y1 + 19)-(x2, y1 + 19), 0
sttit = x1 + (x2 - x1) / 2 - LEN(title$) * 4
IF textonoff = 1 THEN vect title$, 1, sttit, y1 + 4, txc
'button_close
IF style = 0 THEN ibs = 2 ELSE ibs = 4
LINE (x1 + 4, y1 + 4)-(x1 + 19, y1 + 19), 7, BF
LINE (x1 + 3, y1 + 3)-(x1 + 19, y1 + 19), 0, B
LINE (x1 + 3 + ibs, y1 + 10)-(x1 + 19 - ibs - 1, y1 + 12), 0, B
LINE (x1 + 4 + ibs, y1 + 13)-(x1 + 19 - ibs, y1 + 13), 8
LINE -(x1 + 19 - ibs, y1 + 11), 8
'button maximize
IF style = 0 OR style = 2 THEN
LINE (x2 - 19, y1 + 4)-(x2 - 4, y1 + 19), 7, BF
LINE (x2 - 19, y1 + 3)-(x2 - 3, y1 + 19), 0, B
LINE (x2 - 5, y1 + 4)-(x2 - 4, y1 + 18), 8, B
LINE -(x2 - 18, y1 + 17), 8, BF
LINE (x2 - 5, y1 + 4)-(x2 - 18, y1 + 4), 15
LINE -(x2 - 18, y1 + 18), 15
''arrow up
LINE (x2 - 15, y1 + 12)-(x2 - 8, y1 + 12), 0
LINE -(x2 - 12, y1 + 8), 0
LINE -(x2 - 15, y1 + 12), 0
PAINT (x2 - 12, y1 + 10), 0
'button minimize
LINE (x2 - 19 - 16, y1 + 4)-(x2 - 4 - 16, y1 + 19), 7, BF
LINE (x2 - 19 - 16, y1 + 3)-(x2 - 3 - 16, y1 + 19), 0, B
LINE (x2 - 5 - 16, y1 + 4)-(x2 - 4 - 16, y1 + 18), 8, B
LINE -(x2 - 18 - 16, y1 + 17), 8, BF
LINE (x2 - 5 - 16, y1 + 4)-(x2 - 18 - 16, y1 + 4), 15
LINE -(x2 - 18 - 16, y1 + 18), 15
''arrow down
LINE (x2 - 15 - 16, y1 + 9)-(x2 - 8 - 16, y1 + 9), 0
LINE -(x2 - 11 - 16, y1 + 13), 0
LINE -(x2 - 15 - 16, y1 + 9), 0
PAINT (x2 - 11 - 16, y1 + 10), 0
'enlarge
LINE (x1 + 19, y1)-(x1 + 19, y1 + 19), 0
LINE (x1 + 19, y2)-(x1 + 19, y2 - 3), 0
LINE (x1, y2 - 19)-(x1 + 3, y2 - 19), 0
LINE (x2 - 19, y1)-(x2 - 19, y1 + 3), 0
LINE (x2 - 3, y2 - 19)-(x2, y2 - 19), 0
LINE (x2 - 19, y2 - 3)-(x2 - 19, y2), 0
'menubar
IF style = 0 THEN
LINE (x1 + 4, y1 + 37)-(x2 - 4, y1 + 37), 0
IF textonoff = 1 THEN
vect menu$, 1, x1 + 12, y1 + 21, 0
LINE (x1 + 12, y1 + 34)-(x1 + 10 + 8, y1 + 34), 0
FOR a = 1 TO LEN(menu$) - 1
IF MID$(menu$, a, 1) = " " AND MID$(menu$, a + 1, 1) <> " " THEN LINE (x1 + 12 + a * 8, y1 + 34)-(x1 + 10 + (a + 1) * 8, y1 + 34), 0
NEXT
END IF
END IF
END IF
END SUB

