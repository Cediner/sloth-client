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

(defun drink-water1 (&optional (refill t))
  (if (and (held-item)
           (or (is-item-contents (held-item) "Water")
               (string= "Bucket" (item-name (held-item)))))
      ;;holding a bucket or something on the mouse, try to use it or refill if needed
      (progn
        (hk-unset-item +hotkey-1+)
        (wait-until-hotkey-is-unset +hotkey-1+)
        (hk-set-item +hotkey-1+)
        (wait-until-hotkey-is-set +hotkey-1+)
        (hk-use-item +hotkey-1+ +mf-none+)
        (when (and refill
                   (< (stamina) 100))
          (refill-water-from-hand)))
      ;;Not holding anything of value on cursor
      (progn
        (when (held-item)
          (mv-drop (gob-rc (my-gob)) +mf-none+))
        (wait-until (lambda () (null (held-item))))
        
        (loop
           for itm in (inventories-get-items-by-filter
                       (lambda (itm)
                         (let ((contents (item-get-contents itm)))
                           (and contents
                                (jeq (liquid-type) (contents-type contents))))))
           do (progn
                (item-interact itm +mf-none+)
                (wait-until (lambda () (flowermenu)))
                (flowermenu-select-by-name "Drink")
                (when (= (stamina) 100)
                  (return-from drink-water))))
        (when (and refill
                   (< (stamina) 100))
          (refill-water-from-inventory)))))
       
(defun check-stam-and-drink1 (&key (drink-at 40) (refill t))
  (if (< (stamina) drink-at)
      (progn
        (drink-water1 refill)
        t)
      nil))

(defun dig-until-full (dig-tile)
  (menu-use "paginae/act/dig")
  (sleep 0.5)
  (loop
     until (inventory-full (main-inventory))
     do (progn
          (check-stam-and-drink1)
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
