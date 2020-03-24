(defpackage :smelt-ores
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :smelt-ores)

#|
Fills Smelter(s) with Ore, then fels and lights Smelter(s). Upon completion, drops all slag, stockpiles all the metal and
repeats until all ore is gone (or until gty of ore is <= 1 smelter load)

gfx/terobjs/smelter
sdt: 70 -> lit
sdt: 64 -> not lit

States:
 1) Fill
 2) Light
 3) Wait
 4) Drop junk
 5) stockpile
|#
