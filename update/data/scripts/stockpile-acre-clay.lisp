(load "data/scripts/lib/_common_stockpile.lisp")
(defpackage :stockpile-acre-clay
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config
        :stockpile-common))
(in-package :stockpile-acre-clay)

(script
  (stockpile-items-forever "Acre Clay"
                           "gfx/terobjs/items/clay-acre"
                           "gfx/terobjs/stockpile-clay"
                           "Clay"))
