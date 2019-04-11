(defpackage :dig-acre-clay
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config)
  (:export #:run))
(in-package :dig-acre-clay)

(defconstant +max-diff+ 9)
(defconstant +clay+ 167)

(defun get-bbox ()
  (msg-listen)
  (chat-send-message (bot-chat) "Select area to dig")
  (bbox-trigger)
  (let ((bbox nil))
    (loop
       until bbox
       do (progn
            (sleep 1)
           (when (msg-has-message)
             (let ((msg (msg-poll-message)))
               (when (string= "bot-select" (msg-subject msg))
                 (setf bbox (bbox-make (aref (msg-args msg) 0)
                                       (aref (msg-args msg) 1))))))))
    (msg-stop-listening)
    (msg-clear-messages)
    bbox))

(defconstant +coords+ (list (coord (* (east) +tilesz+) 0)
                            (coord (* (west) +tilesz+) 0)
                            (coord 0 (* (north) +tilesz+))
                            (coord 0 (* (south) +tilesz+))
                            (coord (* (east) +tilesz+) (* (north) +tilesz+))
                            (coord (* (east) +tilesz+) (* (south) +tilesz+))
                            (coord (* (west) +tilesz+) (* (north) +tilesz+))
                            (coord (* (west) +tilesz+) (* (south) +tilesz+))))

(defun is-tile-diggable (c)
  (let* ((my-z (mc-get-z c))
         (around (loop
                    for oc in +coords+
                    collect (- (mc-get-z (coord-add c oc)) my-z))))
    (if (and (= (mc-get-tile c) +clay+)
             (reduce (lambda (x y) (and x y))
                     (map 'list (lambda (x) (< x +max-diff+)) around)))
        around
        nil)))

(defun get-next-diggable-tile (coords)
  (let ((best-c nil)
        (best-min 1000))
    (dolist (c coords)
      (let* ((my-z (mc-get-z c))
             (around (is-tile-diggable c)))
        (when (and around
                   (or (null best-c)
                       (< (reduce #'min around) best-min)))
          (setf best-c c)
          (setf best-min (reduce #'min around)))))
    best-c))
        
(defun dig-tile (c)
  (let ((start-z (mc-get-z c)))
    (check-stam-and-drink)
    (when (held-item)
      (item-drop (held-item)))
    (menu-use "paginae/act/dig")
    (sleep 0.5)
    (mv-smart-move (coord-to-coord2d c))
    (wait-for-movement)
    (loop
       while (is-tile-diggable c)
       do (progn
            (inventories-drop-all-items-by-name "Acre Clay")
            (check-stam-and-drink)
            (when (= (progress) -1.0)
              (mv-move-to (coord-to-coord2d c))
              (wait-for-movement))
            (sleep 0.5)))
    (inventories-drop-all-items-by-name "Acre Clay")
    (mv-click (coord-to-coord2d c) +right-button+ +mf-none+)
    (mv-move-to (coord-to-coord2d c))))

(defun dig-area (bb)
  (let ((coords (bbox-dots bb)))
    (loop
       for next = (get-next-diggable-tile coords)
       while next
       do (dig-tile next))))

(script
 (let ((box (get-bbox)))
   (dig-area box)))
