(defpackage :stockpile-acre-clay
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :stockpile-acre-clay)

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

(defun pickup-acre-clay (original-position)
  (loop
     for gob = (gob-get-closest-by-filter
                (lambda (gob)
                  (and (string= "gfx/terobjs/items/clay-acre" (gob-name gob))
                       (mv-find-path-to-gob gob))))
     until (or (held-item) (null gob))
     do (let ((id (gob-id gob)))
          (mv-smart-move-to-gob gob)
          (mv-click-gob gob +right-button+ +mf-none+)
          (wait-until (lambda () (null (oc-get-gob id))) :timeout 1000)))
  (when (held-item)
    (item-drop (held-item)))
  (mv-smart-move original-position))

(defun store-acre-clay-in-stockpile (itm stockpile)
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
    (coord-to-coord2d best-tile)))

(defun make-stockpile (itm stockpile-c)
  (item-take itm)
  (mv-interact-held-item-with-tile stockpile-c +mf-none+)
  (wait-for-placing-gob)
  (mv-place-gob stockpile-c 0 +left-button+ +mf-shift+)
  (wait-until (lambda () (stockpile-by-name "Clay"))))
  

(defun store-acre-clay (itm original-position bbox tiles)
  (let ((gob (gob-get-closest-by-filter
              (lambda (gob)
                (and (bbox-within bbox (gob-rc gob))
                     (string= "gfx/terobjs/stockpile-clay" (gob-name gob))
                     (< (gob-sdt gob) 31))))))
    (unless (and gob (store-acre-clay-in-stockpile itm gob))
      (let ((stockpile-c (get-next-good-tile-for-stockpile tiles)))
        (make-stockpile itm stockpile-c)
        (mv-move-to original-position)
        (wait-for-movement)))))

(script
 (let ((bbox (get-bbox)))
   (let ((tiles (bbox-tiles bbox))
         (original-position (gob-rc (my-gob))))
     (forever
       (sleep 0.1)
       (pickup-acre-clay original-position)
       (mv-smart-move original-position)
       (let ((itm (inventory-get-item-by-name (main-inventory) "Acre Clay")))
         (when itm
           (store-acre-clay itm original-position bbox tiles)))))))
