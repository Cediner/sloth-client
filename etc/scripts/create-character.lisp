(defpackage :create-character
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :create-character)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Loading/Creating new data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-data (file)
  (with-open-file (fd file :direction :input)
    (let ((points (read-from-string (read-line fd))))
      points)))
			  
(script
	(let ((points (load-data "data/scripts/CharacterCreate.dat")))
		(dolist (point points)
			(let ((destination  (coord2d-add (gob-rc (my-gob)) (coord2d (car point) (cdr point)))))
				(mv-move-to destination)
				(wait-for-movement)
			)
		)
	)
	;;Talk to wizard
	;;Talk to Secret thing
	;;Talk to 
)
