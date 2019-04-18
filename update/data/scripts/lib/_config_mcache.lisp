(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MCache API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +mcache+ "haven.MCache")
(java-func +mcache+ mc-get-z-1 "getz_safe" +coord+)
(java-func +mcache+ mc-get-tile-1 "gettile_safe" +coord+)
(java-func +mcache+ mc-get-grid-id-1 "getgridid" +coord2d+)
(java-func +mcache+ mc-get-tile-offset-1 "gettileoffset" +coord2d+)
(defmacro mc-get-z (c)
  `(mc-get-z-1 (mc) (coord-div ,c (coord +tilesz+ +tilesz+))))
(defmacro mc-get-tile (c)
  `(mc-get-tile-1 (mc) (coord-div ,c (coord +tilesz+ +tilesz+))))

(defmacro mc-get-grid-id (c)
  `(mc-get-grid-id-1 (mc) ,c))
(defmacro mc-get-tile-offset (c)
  `(mc-get-tile-offset-1 (mc) ,c))

(defun mc-tilify (c)
  (let ((tsz (coord 11 11)))
    (coord-add (Coord-mul (coord-div c tsz) tsz) (coord-div tsz (coord 2 2)))))

(export '(mc-get-z mc-get-tile mc-tilify
          mc-get-grid-id mc-get-tile-offset))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Grid Lookups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +griddata+ "haven.sloth.io.GridData")
(java-func +script+ script-resolve-grid "resolvePosition" +long+)
(with-script-define resolve-grid script-resolve-grid grid-id)

(defun get-real-location-coord (c)
  (let* ((grid-id (mc-get-grid-id c))
         (offset (mc-get-tile-offset c))
         (offset-x (/ (coord-x offset) 100.0))
         (offset-y (/ (coord-y offset) 100.0)))
    (if (/= grid-id -1)
        (let ((location (resolve-grid grid-id)))
          (if location
              (coord2d (+ offset-x (coord-x location))
                       (+ offset-y (coord-y location)))
              nil))
        nil)))

(defun get-real-location-on-map (c)
  (let* ((grid-id (mc-get-grid-id c))
         (offset (mc-get-tile-offset c))
         (offset-x (/ (coord-x offset) 100.0))
         (offset-y (/ (coord-y offset) 100.0)))
    (if (/= grid-id -1)
        (let ((location (resolve-grid grid-id)))
          (if location
              (let ((x (+ offset-x (coord-x location)))
                    (y (+ offset-y (coord-y location))))
                (format nil "http://odditown.com/haven/map/#x=~,2f&y=~,2f&zoom=9" x y))
              "Unknown"))
        "Unknown")))

(export '(resolve-grid get-real-location-coord get-real-location-on-map))
