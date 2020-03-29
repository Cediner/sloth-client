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

(defun logout-and-wait ()
  (let ((name (character-name)))
    (goto-character-screen)
    (sleep 15)
    (login name)
    (wait-until (lambda () (speed)))))

(defconstant +angler+ `("gfx/kritter/caveangler/caveangler"))

(script
 (multiple-value-bind (token role)
     (prompt-for-discord-info)
   (let ((lake-name (prompt-for-input "Enter the lake name"))
         (char-name (character-name))
         (toggle 0))
     (start-discord-session token)
     (forever
      (let ((targets (gob-get-all-by-filter
                      (lambda (gob) 
                        (and 
                         (member (gob-name gob) +angler+ :test 'equal)
                         (not (is-gob-dead gob)))))))
        (if (> (length targets) 0) 
            (if (= 0 toggle) 
                (progn
                  (send-discord-message +discord-bot-channel+
                                        (format nil "<@&~A> Spotted an angler at ~A"
                                                role
                                                lake-name))
                  (setq toggle 1))
                (sleep 20))
            (progn
              (goto-character-screen)
              (sleep (* 60 1))
              (login char-name)
              (setq toggle 0)
              (sleep 20))))))))
