(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MCache API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +mcache+ "haven.MCache")
(java-func +mcache+ mc-get-z-1 "getz_safe" +coord+)
(java-func +mcache+ mc-get-tile-1 "gettile_safe" +coord+)
(defmacro mc-get-z (c)
  `(mc-get-z-1 (mc) (coord-div ,c (coord +tilesz+ +tilesz+))))
(defmacro mc-get-tile (c)
  `(mc-get-tile-1 (mc) (coord-div ,c (coord +tilesz+ +tilesz+))))
(defun mc-tilify (c)
  (let ((tsz (coord 11 11)))
    (coord-add (Coord-mul (coord-div c tsz) tsz) (coord-div tsz (coord 2 2)))))

(export '(mc-get-z mc-get-tile mc-tilify))
