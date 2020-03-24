(load "data/scripts/lib/_common_stockpile.lisp")
(defpackage :stockpile-fibres
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config
        :stockpile-common))
(in-package :stockpile-fibres)

(script
  (stockpile-items-forever "Flax Fibres" 
                           "gfx/terobjs/items/flaxfibre"
                           "gfx/terobjs/stockpile-flaxfibre"
                           "Flax Fibres"))
