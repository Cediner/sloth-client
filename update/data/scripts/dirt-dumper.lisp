(defpackage :dirt-dumper
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))

(in-package :dirt-dumper)

(defun get-dirt (stockpile)
  (mv-smart-move-to-gob stockpile)
  (mv-click-gob stockpile +right-button+ +mf-shift+)
  (wait-for-movement))

(defun dump-dirt (dump-tile)
  (mv-smart-move dump-tile)
  (loop
     for itm = (inventory-get-item-by-name (main-inventory) "Soil")
     while itm
     do (progn
          (item-drop-all-alike itm)
          (sleep 1))))
  
(script
 (let ((dump-tile (prompt-for-coord "Select a tile to dump dirt on")))
   (loop
      for gob = (gob-get-closest-by-filter
                 (lambda (g)
                   (string= (gob-name g) "gfx/terobjs/stockpile-soil")))
      while gob
      do (progn
           (get-dirt gob)
           (dump-dirt dump-tile)
           (sleep 1)))))
