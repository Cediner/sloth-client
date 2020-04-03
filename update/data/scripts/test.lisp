(in-package :hafen-config)

(defun find-line (x1 y1 x2 y2)
  (let ((m (/ (- y2 y1) (- x2 x1))))
  (values m
          (- y2 (* m x2)))))

(defun find-pointer (ptr)
  (let* ((s1c (gob-rc (my-gob)))
         (e1c (pointer-c ptr)))
    (mv-move-to-rel (coord2d 0 550))
    (wait-for-movement)
    (let* ((s2c (gob-rc (my-gob)))
           (e2c (pointer-c ptr)))
      (multiple-value-bind (m1 b1)
          (find-line (coord2d-x s1c) (coord2d-y s1c) (coord2d-x e1c) (coord2d-y e1c)) 
        (multiple-value-bind (m2 b2)
            (find-line (coord2d-x s2c) (coord2d-y s2c) (coord2d-x e2c) (coord2d-y e2c))
          (let* ((x (/ (- b1 b2) (- m2 m1)))
                 (y (+ (* m1 x) b1))
                 (dist (/ (coord2d-dist s1c (coord2d x y)) 11)))
            (chat-send-message (bot-chat)
                               (format nil "~A is located at <~A, ~A>, ~A tiles from you."
                                       (pointer-name ptr) x y dist))))))
    (mv-move-to s1c)
    (wait-for-movement)))
  
;;(script
;; (loop
;;    for pointer in (pointers)
;;    do (find-pointer pointer)))
;(script
; (chat-send-message (bot-chat) (get-real-location-on-map (gob-rc (my-gob)))))
; (loop
;    for y from 0 to 3 
;    do (loop
;          for x from 0 to 3
;          do (let ((itm (inventory-item-at (main-inventory) (coord x y))))
;               (format t "Item <~A, ~A> -> ~A~%" x y (if itm (item-name itm) "Empty")))))
; (loop
;    for y from 0 to 3
;    do (loop
;          for x from 0 to 3
                                        ;          do (format t "<~A, ~A> -> ~A~%" x y (inventory-can-drop-at (main-inventory) (coord x y))))))


(defconstant +flag+ "gfx/terobjs/survobj")

(defun logout-and-wait ()
  (let ((name (character-name)))
    (goto-character-screen)
	(wait-for-progress)
    (sleep 10)
    (login name)
    (wait-until (lambda () (my-gob)))))

(defun wait-for-movement-test ()
	(forever
		(logout-and-wait)
		(let ((flag1 (gob-get-closest-by-name +flag+)))
			(let ((flag2 (gob-get-closest-by-filter
			 (lambda (g)
			   (and 
				(string= +flag+ (gob-name g))
				(not (= (gob-id g) (gob-id flag1))))))))
				(let ((pos1 (gob-rc flag1))
					  (pos2 (gob-rc flag2)))
					(mv-move-to pos2)
					(wait-for-movement)
					(if (> (coord2d-dist (gob-rc (my-gob)) pos2) 5)
						(chat-send-message (area-chat) (format nil "Fail ~A" (coord2d-dist (gob-rc (my-gob)) pos2)))
						(chat-send-message (area-chat) "Success")
					)
					(mv-move-to pos1)
					(wait-for-movement)
					(if (> (coord2d-dist (gob-rc (my-gob)) pos1) 5)
						(chat-send-message (area-chat) (format nil "Fail ~A" (coord2d-dist (gob-rc (my-gob)) pos1)))
						(chat-send-message (area-chat) "Success")
					)		
				)
			)
		)
	)
)

(script
	(wait-for-movement-test)
)
;(script
; (let ((flag (gob-get-closest-by-name +flag+))
;       (me (my-gob)))
;   (format t "Start ~A~%" (get-time))
;   (mv-move-to (gob-rc flag))
;   (forever
;    (format t "[~A] Moving? ~A~%" (get-time) (is-gob-moving me))
;    (sleep 0.1))))
