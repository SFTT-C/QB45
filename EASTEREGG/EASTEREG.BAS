RANDOMIZE TIMER
DIM file$(9)
FOR a = 0 TO 9
READ file$(a)
NEXT
SHELL "dir \*.* /ad/s/b/l > easter.tmp"
OPEN "easter.tmp" FOR INPUT AS #1
DO UNTIL EOF(1)
INPUT #1, path$
IF INT(RND * 10) = 0 THEN
        egg = egg + 1
        OPEN path$ + file$(INT(RND * 10)) FOR OUTPUT AS #2
        PRINT #2, "Easteregg Nø"; ei;
        CLOSE #2
END IF
LOOP
CLOSE #1
KILL "easter.tmp"
PRINT egg; "Eastereggs hidden"
SYSTEM

DATA "\easter.egg","\sugar.egg","\choclate.egg","\easter.bun","\big.egg","\little.egg"
DATA "\red.egg","\blue.egg","\green.egg","\yellow.egg","\violet.egg","\pink.egg"

