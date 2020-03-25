(load "data/scripts/lib/_common_stockpile.lisp")
(defpackage :stockpile-blocks
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config
        :stockpile-common))
(in-package :stockpile-blocks)

(script
  (stockpile-items-forever "Block of"
                           "gfx/terobjs/items/wblock-"
                           "gfx/terobjs/stockpile-wblock"
                           "Block of Wood"))
