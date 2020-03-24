(defpackage :dirt-counter
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :dirt-counter)


(defconstant +beach+ 129)
(defconstant +skargard+ 172)
(defconstant +odeep+ 187)
(defconstant +owater+ 189)

(script 
	(let ((height-coords (bbox-dots (get-bbox "Select the height tile")))
		  (area-coords (bbox-dots (get-bbox "Select the area")))
		  (dirt-needed 0)
		  (coord-count 0)
		  (target-height)
		 )
		
		(dolist (c height-coords)
			(setq target-height (mc-get-z c))
		)
		
		(dolist (c area-coords)
			(let ((tile (mc-get-tile c)))
				(when (not (or (= tile +beach+) (= tile +skargard+) (= tile +odeep+) (= tile +owater+)))
					(let ((height (mc-get-z c)))
						(setq dirt-needed (+ dirt-needed (- target-height height)))
						(setq coord-count (+ coord-count 1))
					)
				)
			)
		)
		
		(chat-send-message (area-chat) (format nil "Dirt Needed: ~A Coord Count: ~A" dirt-needed coord-count))
	)
)