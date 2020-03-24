(load "data/scripts/lib/_common_stockpile.lisp")
(defpackage :quarry-master
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config
        :stockpile-common))

(in-package :quarry-master)

(defparameter *stone-stockpile-gob* "gfx/terobjs/stockpile-stone")
(defparameter *stone-stockpile-name* "Stone")

(defun dig-until-full (dig-tile)
  (menu-use "paginae/act/dig")
  (sleep 0.5)
  (loop
     until (inventory-full (main-inventory))
     do (progn
          (check-stam-and-drink)
          (when (= (progress) -1.0)
            (mv-move-to dig-tile)
            (wait-for-movement))
          (Sleep 0.5)))
  (mv-click dig-tile +right-button+ +mf-none+))

(script
 (let* ((dig-tile (prompt-for-coord "Mark tile to dig from"))
        (bbox (get-bbox "Select an area to stockpile items in"))
        (tiles (bbox-tiles bbox))
        (stone-name (prompt-for-input "What is the name of the stone?")))
   (forever
    (dig-until-full dig-tile)
    (let ((itm (inventory-get-item-by-name (main-inventory) stone-name)))
      (when itm
        (stockpile-common:store-items itm *stone-stockpile-gob* *stone-stockpile-name*
                                      dig-tile bbox tiles))))))
