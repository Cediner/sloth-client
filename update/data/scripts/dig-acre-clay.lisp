(defpackage :dig-acre-clay
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config)
  (:export #:run))
(in-package :dig-acre-clay)

(defconstant +max-diff+ 11)
(defconstant +clay+ 167)

(defconstant +coords+ (list (coord (* (east) +tilesz+) 0)
                            (coord (* (west) +tilesz+) 0)
                            (coord 0 (* (north) +tilesz+))
                            (coord 0 (* (south) +tilesz+))
                            (coord (* (east) +tilesz+) (* (north) +tilesz+))
                            (coord (* (east) +tilesz+) (* (south) +tilesz+))
                            (coord (* (west) +tilesz+) (* (north) +tilesz+))
                            (coord (* (west) +tilesz+) (* (south) +tilesz+))))

(defconstant +coords-2+ (list (coord (* (east) +tilesz+) 0)
                              (coord (* (west) +tilesz+) 0)
                              (coord (* (north) +tilesz+) 0)
                              (coord (* (south) +tilesz+) 0)))

(defun is-tile-diggable (c)
  (let* ((my-z (mc-get-z c))
         (around (loop
                    for oc in +coords+
                    collect (- (mc-get-z (coord-add c oc)) my-z)))
         (taround (loop
                     for oc in +coords-2+
                     collect (mc-get-tile (coord-add c oc)))))
    (if (and (= (mc-get-tile c) +clay+)
             (reduce (lambda (x y) (and x y))
                     (map 'list (lambda (x) (= x +clay+)) taround))
             (reduce (lambda (x y) (and x y))
                     (map 'list (lambda (x) (< x +max-diff+)) around)))
        around
        nil)))

(defun get-next-diggable-tile (coords)
  (let ((best-c nil)
        (best-val 0))
    (dolist (c coords)
      (let* ((my-z (mc-get-z c))
             (around (is-tile-diggable c)))
        (when (and around
                   (or (null best-c)
                       (> my-z best-val)))
          (setf best-c c)
          (setf best-val my-z))))
    best-c))

(defun check-clay-on-ground ()
  (loop
     while (> (length (gob-get-all-by-name "gfx/terobjs/items/clay-acre"))
              (* 8 10))
     do (sleep 0.1)))
        
(defun dig-tile (c)
  (let ((start-z (mc-get-z c))
        (c2d (coord-to-coord2d c)))
    (check-stam-and-drink)
    (when (held-item)
      (item-drop (held-item)))
    (check-clay-on-ground)
    (loop
       until (coord2d-eq (gob-rc (my-gob)) c2d)
       do (progn
            (backoff-randomly)
            (mv-smart-move c2d)))
    (menu-use "paginae/act/dig")
    (sleep 0.5)
    (loop
       while (is-tile-diggable c)
       do (progn
            (inventories-drop-all-items-by-name "Acre Clay")
            (check-stam-and-drink)
            (when (= (progress) -1.0)
              (mv-move-to c2d)
              (wait-for-movement))
            (sleep 0.5)))
    (inventories-drop-all-items-by-name "Acre Clay")
    (mv-click c2d +right-button+ +mf-none+)
    (mv-move-to c2d)))

(defun dig-area (bb)
  (let ((coords (bbox-dots bb)))
    (loop
       for next = (get-next-diggable-tile coords)
       while next
       do (progn
            (when (check-for-starving)
              (return-from dig-area))
            (dig-tile next)))))

(script
 (let ((box (get-bbox "Select an area to dig")))
   (dig-area box)))
