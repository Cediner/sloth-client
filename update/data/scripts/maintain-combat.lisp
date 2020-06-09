(defpackage :maintain-combat
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :maintain-combat)

(defconstant +distance-from-angler+ 26)
				
(defun safe-spot (tar)
	(let* ((me (gob-rc (my-gob)))
		  (target (gob-rc tar))
		  (vect (coord2d-sub target me))
		  (dist (coord2d-dist target me))
		  (unit-vect (coord2d-div vect (coord2d dist dist)))
		  (move-distance (- dist +distance-from-angler+))
		  (move-vect (coord2d-mul unit-vect (coord2d move-distance move-distance))))
		(mv-move-to (coord2d-add move-vect me))
	)
)

(defun maintain-combat ()
	(let ((target (prompt-for-selected-gob "Select a gob"))
		  (spot (gob-rc (my-gob))))
		(loop until (is-gob-dead target) do 
			(progn
				(if (fs)
					(sleep .1)
					(progn
						(menu-use "paginae/act/atk")
						(mv-click-gob target +left-button+ +mf-none+)
						(wait-until (lambda () (fs)))
						(mv-click spot +right-button+ +mf-none+)
						(mv-move-to spot)
						(let ((rel (get-relation (gob-id target))))
							(peace-toggle rel)
						)
					)
				)
				(when (< (coord2d-dist (gob-rc (my-gob)) (gob-rc target)) +distance-from-angler+)
					(safe-spot target)
				)
			)
		)		
	)
)

(script
	(maintain-combat)
)