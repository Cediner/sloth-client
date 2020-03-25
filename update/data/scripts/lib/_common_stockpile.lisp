(defpackage :stockpile-common
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))

(in-package :stockpile-common)

(defun get-next-item-on-ground (item-gob-name)
  (gob-get-closest-by-filter
   (lambda (gob)
     (and (search item-gob-name (gob-name gob))
          (mv-find-path-to-gob gob)))))

(defun pickup-items (original-position item-gob-name)
  (loop
     for gob = (get-next-item-on-ground item-gob-name) 
     until (or (held-item) (null gob))
     do (let ((id (gob-id gob)))
          (mv-smart-move-to-gob gob)
          (mv-click-gob gob +right-button+ +mf-shift+)
          (wait-for-movement)))
  (when (held-item)
    (item-drop (held-item)))
  (loop
     until (< (coord2d-dist original-position (gob-rc (my-gob))) 5)
     do (mv-smart-move original-position)))
  

(defun store-items-in-stockpile (itm stockpile)
  (let ((path (mv-find-path-to-gob stockpile))
        (original-c (gob-rc (my-gob))))
    (if path
        (progn
          (mv-walk-path path)
          (item-take itm)
          (wait-until (lambda () (held-item)))
          (mv-interact-held-item-with-gob stockpile +mf-shift+)
          (wait-until (lambda () (null (held-item))) :timeout 5000)
          (mv-walk-path (mv-reverse-path path original-c))
          t)
        nil)))

(defun get-next-good-tile-for-stockpile (tiles)
  (let ((best-tile nil)
        (best-max 0)
        (my-tile (gob-rc (my-gob))))
    (dolist (tile tiles)
      (when (and (mv-los (coord-to-coord2d tile))
                 (or (null best-tile)
                     (< best-max (coord2d-dist (coord-to-coord2d tile) my-tile))))
        (setf best-tile tile)
        (setf best-max (coord2d-dist (coord-to-coord2d tile) my-tile))))
    (if best-tile
        (coord-to-coord2d best-tile)
        nil)))

(defun make-stockpile (itm stockpile-item-name stockpile-c)
  (item-take itm)
  (mv-interact-held-item-with-tile stockpile-c +mf-none+)
  (wait-for-placing-gob)
  (mv-place-gob stockpile-c 0 +left-button+ +mf-shift+)
  (wait-until (lambda () (stockpile-by-name stockpile-item-name))))
  

(defun store-items (itm stockpile-gob-name stockpile-item-name
                     original-position bbox tiles)
  (let ((gob (gob-get-closest-by-filter
              (lambda (gob)
                (and (bbox-within bbox (gob-rc gob))
                     (string= stockpile-gob-name (gob-name gob))
                     (< (gob-sdt gob) 31))))))
    (unless (and gob (store-items-in-stockpile itm gob))
      (let ((stockpile-c (get-next-good-tile-for-stockpile tiles)))
        (when stockpile-c
          (make-stockpile itm stockpile-item-name stockpile-c)
          (mv-move-to original-position)
          (wait-for-movement))))))

(defun stockpile-items-forever (item-inv-name item-gob-name stockpile-gob-name stockpile-item-name)
  (let ((bbox (get-bbox "Select an area to stockpile items in")))
    (let ((tiles (bbox-tiles bbox))
          (original-position (gob-rc (my-gob))))
      (forever
        (sleep 0.1)
        (pickup-items original-position item-gob-name)
        (mv-smart-move original-position)
        (let ((itm (inventory-get-items-by-filter
                    (main-inventory)
                    (lambda (itm)
                      (search item-inv-name (item-name itm))))))
          (when itm
            (store-items (car itm) stockpile-gob-name stockpile-item-name
                         original-position bbox tiles)))))))

(export '(stockpile-items-forever
          pickup-items
          store-items))
