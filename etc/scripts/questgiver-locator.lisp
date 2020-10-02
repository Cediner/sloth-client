(defpackage :questgiver-locator
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :questgiver-locator)

(defun pointer-get-by-name (name)
  (let ((ptrs (pointers)))
  (loop for ptr in ptrs do
	(when (search name (pointer-name ptr))
        (return-from pointer-get-by-name ptr))
  )
  nil))

(defun find-line (x1 y1 x2 y2)
  (let ((m (/ (- y2 y1) (- x2 x1))))
  (values m
          (- y2 (* m x2)))))							   

(defun find-distance (slc elc slc2 elc2 ptr-name)
	(multiple-value-bind (m1 b1)
          (find-line (coord2d-x slc) (coord2d-y slc) (coord2d-x elc) (coord2d-y elc)) 
        (multiple-value-bind (m2 b2)
            (find-line (coord2d-x slc2) (coord2d-y slc2) (coord2d-x elc2) (coord2d-y elc2))
          (let* ((x (/ (- b1 b2) (- m2 m1)))
                 (y (+ (* m1 x) b1))
                 (dist (/ (coord2d-dist slc (coord2d x y)) 11)))
            (chat-send-message (bot-chat)
                               (format nil "~A is located at <~A, ~A>, ~A tiles from you."
                                       ptr-name x y dist))))))

(script
	(let ((slc (gob-rc (my-gob)))
		   (elc ())
		   (names ()))
		(loop for pointer in (pointers)
		do (progn
			(push (pointer-c pointer) elc)
			(push (pointer-name pointer) names)
		))
		(prompt-for-input "Move some tiles away then enter anything in chat to calculate questgiver distance")
		(let ((slc2 (gob-rc (my-gob))))
			(loop for el in elc
				  for nm in names
			do (let ((ptr (pointer-get-by-name nm)))
				(if ptr (find-distance slc el slc2 (pointer-c ptr) nm) (chat-send-message (bot-chat) "Questgiver not found now")))))))