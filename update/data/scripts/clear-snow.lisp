(defpackage :clear-snow
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config)
  (:export #:run))
(in-package :clear-snow)

(defun get-next-snow-tile (tiles)
	(let ((best nil)
		  (bestdist 0)
		  (mc (gob-rc (my-gob))))
		(dolist (tile tiles)
			(when (and 	(= (mc-get-tile tile) 54)
						(or (null best)
							(< (coord2d-dist mc (coord-to-coord2d tile)) bestdist)))
				(setf best tile)
				(setf bestdist (coord2d-dist mc (coord-to-coord2d tile)))
			)
		)
		(return-from get-next-snow-tile best)))
		
(defun dig-snow (tile)
	(check-stam-and-drink)
	(mv-move-to (coord-to-coord2d tile))
	(wait-for-movement)
	(menu-use "paginae/act/dig")
	(mv-move-to (coord-to-coord2d tile))
	(wait-for-progress)	
	(inventory-drop-all-items-by-name (main-inventory) "Snow")
    (mv-click (coord-to-coord2d tile) +right-button+ +mf-none+))

(script
	(let ((tiles (bbox-tiles (get-bbox "Select an area"))))
		(chat-send-message (area-chat) (format nil "~A" (length tiles)))
		(loop
			for tile = (get-next-snow-tile tiles)
			while tile
			do (progn 
				(dig-snow tile)
				(setf tiles 
						(remove-if (lambda (tt) 
										(and (= (coord-x tt) (coord-x tile)) 
											 (= (coord-y tt) (coord-y tile)))) 
									tiles))))))