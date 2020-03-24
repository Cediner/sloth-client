;;;; farm-area
;;;; Simple, quick, efficient farming script for harvesting and planting of plowed crops
;;;;  - Shouldn't be used for trellis crops
;;;;  - Since this harvests and plants via loftar's bulk commands it shouldn't be used for
;;;;    maximizing quality
(load "data/scripts/lib/_common_farming.lisp")
(defpackage :farm-area
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config
        :farming-common))

(in-package :farm-area)

(defun plant-crops (bb)
  (drink-water)
  (let ((seed (car (inventories-get-items-by-filter #'is-a-seed))))
    (when seed
      ;;Bring up the planter icon
      (item-interact seed +mf-shift+)
      ;;select our area
      (mv-select-area (bbox-ul bb) (bbox-ec bb))
      ;;let it go off until we don't see any movement -> done planting
      (loop
         until (or (not (check-for-movement (my-gob)))
                   (check-stam-and-drink)))
      ;;drop extras
      (let ((extra (inventories-get-items-by-filter #'is-a-seed)))
        (loop
           for itm in extra
           do (progn
                (item-drop itm)
                (sleep 0.01)))))))

(defun get-next-crop (bb)
  (gob-get-closest-by-filter
   (lambda (gob)
     (and (is-a-harvestable-crop gob +ground+)
          (bbox-within bb (gob-rc gob))))))

(defun pick-crops (bb)
  (loop
     for next-crop = (get-next-crop bb)
     until (or (inventory-full (main-inventory))
               (null next-crop))
     do (progn
          ;;open harvest
          (mv-click-gob next-crop +right-button+ +mf-shift+)
          (sleep 1)
          ;;select the area
          (mv-select-area (bbox-ul bb) (bbox-ec bb))
          ;;Wait until full inventory or we've stopped moving
          (loop
             until (or (inventory-full (main-inventory))
                       (not (check-for-movement (my-gob)))
                       (check-stam-and-drink))))))

(defun farm-area (bb)
  (loop
     for next-crop = (get-next-crop bb)
     while next-crop
     do (progn
          (pick-crops bb)
          (plant-crops bb))))

(script
  (let ((bbox (get-bbox "Select an area to farm within - Don't mix crops...")))
    (farm-area bbox)))
    
