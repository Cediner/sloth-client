(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BBox aka MapMod selector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct bbox
  ul
  ec
  sz)

(defconstant +mapmod+ "haven.MapMod")
(defun bbox-trigger ()
  (widget-add (gui) (jnew +mapmod+ t) (coord 50 50)))

(defun bbox-make (ul ec)
  (make-bbox :ul ul :ec ec :sz (coord-sub ec ul)))

(defun bbox-ec (bb)
  (coord-add (bbox-ul bb)
             (coord (* (east) (coord-x (bbox-sz bb)))
                    (* (south) (coord-y (bbox-sz bb))))))

(defun bbox-within (bb coord)
  (let* ((sc (coord-mul (bbox-ul bb) (coord 11 11)))
	 (ec (coord-mul (bbox-ec bb) (coord 11 11)))
	 (br (coord-add
	      (coord (* (east) 11) (* (south) 11))
	      (coord
	       (max (coord-x sc) (coord-x ec))
	       (max (coord-y sc) (coord-y ec)))))
	 (ul (coord
	       (min (coord-x sc) (coord-x ec))
	       (min (coord-y sc) (coord-y ec)))))
    (if (and
         (or (and (>= (coord-x coord) (coord-x ul))
                  (<= (coord-x coord) (coord-x br)))
             (and (>= (coord-x coord) (coord-x br))
                  (<= (coord-x coord) (coord-x ul))))
         (or (and (>= (coord-y coord) (coord-y  ul))
                  (<= (coord-y coord) (coord-y br)))
             (and (>= (coord-y coord) (coord-y br))
                  (<= (coord-y coord) (coord-y ul)))))
        t
        nil)))

(defun bbox-gobs (bb)
  (let ((gobs (oc-get-all-gobs))
        (ret ()))
    (doarr (gob gobs)
           (when (bbox-within bb (gob-rc gob))
             (push gob ret)))
    ret))


(defun bbox-tiles (bb)
  (loop
     for x from 0 to (abs (coord-x (bbox-sz bb)))
     append (loop
               for y from 0 to (abs (coord-y (bbox-sz bb)))
               collect (mc-tilify
                        (coord-add (coord-mul (bbox-ul bb) (coord +tilesz+ +tilesz+))
                                   (coord (* (east) x +tilesz+ (if (< (coord-x (bbox-sz bb)) 0) -1 1))
                                          (* (south) y +tilesz+ (if (< (coord-y (bbox-sz bb)) 0) -1 1))))))))
  

(defun bbox-coords (bb)
  (let ((coords ()))
    (loop
       for x from 0 to (abs (coord-x (bbox-sz bb)))
       do (loop
             for y from 0 to (abs (coord-y (bbox-sz bb)))
             do (progn
                  (push (coord-add (coord-mul (bbox-ul bb) (coord +tilesz+ +tilesz+))
                                   (coord (* (east) x +tilesz+ (if (<  (coord-x (bbox-sz bb)) 0)
                                                                   -1
                                                                   1))
                                          (* (south) y +tilesz+ (if (<  (coord-y (bbox-sz bb)) 0)
                                                                    -1
                                                                 1))))
                        coords))))
    coords))

(defun bbox-dots (bb)
  (let ((coords ()))
    (dotimes (x (1+ (abs (coord-x (bbox-sz bb)))))
      (dotimes (y (1+ (abs (coord-y (bbox-sz bb)))))
        (let ((base (mc-tilify
                     (coord-add (coord-mul (bbox-ul bb) (coord +tilesz+ +tilesz+))
                                (coord (* (east) x +tilesz+ (if (<  (coord-x (bbox-sz bb)) 0)
                                                                -1
                                                                1))
                                       (* (south) y +tilesz+ (if (<  (coord-y (bbox-sz bb)) 0)
                                                                 -1
                                                                 1)))))))
          (pushnew (coord-add base (coord 6 6)) coords :test #'coord-eq)
          (pushnew (coord-add base (coord -5 6)) coords :test #'coord-eq)
          (pushnew (coord-add base (coord 6 -5)) coords :test #'coord-eq)
          (pushnew (coord-add base (coord -5 -5)) coords :test #'coord-eq))))
    coords))
             

(export '(bbox-trigger bbox-make bbox-ec bbox-within bbox-gobs bbox-tiles bbox-coords bbox-dots))
