(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Coord/Coord2d API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro coord (x y)
  `(jnew +coord+ ,x ,y))
(java-field coord-x "x")
(java-field coord-y "y")
(java-func +coord+ coord-add "add" +coord+)
(java-func +coord+ coord-sub "sub" +coord+)
(java-func +coord+ coord-div "div" +coord+)
(java-func +coord+ coord-mul "mul" +coord+)
(java-func +coord+ coord-dist "dist" +coord+)
(java-func +coord+ coord-string "toString")
(java-func +coord+ coord-eq "equals" +object+)
(defmacro coord-to-coord2d (c)
  `(jnew +coord2d+ ,c))
(defmacro fakec ()
  `(coord 1 1))
(defun coord-between (c tl s)
  (let ((br (coord-add tl s)))
    (and (>= (coord-x c) (coord-x tl))
         (>= (coord-y c) (coord-y tl))
         (< (coord-x c) (coord-x br))
         (< (coord-y c) (coord-y br)))))

  
(defmacro coord2d (x y)
  `(jnew +coord2d+ ,x ,y))
(java-field coord2d-x "x")
(java-field coord2d-y "y")
(java-func +coord2d+ coord2d-add "add" +coord2d+)
(java-func +coord2d+ coord2d-sub "sub" +coord2d+)
(java-func +coord2d+ coord2d-div "div" +coord2d+)
(java-func +coord2d+ coord2d-mul "mul" +coord2d+)
(java-func +coord2d+ coord2d-dist "dist" +coord2d+)
(java-func +coord2d+ coord2d-floor "floor" +coord2d+)
(java-func +coord2d+ coord2d-angle "angle" +coord2d+)
(java-func +coord2d+ coord2d-string "toString")
(java-func +coord2d+ coord2d-eq "equals" +object+)
(defun coord2d-to-coord (c2)
	(coord (floor (coord2d-x c2)) (floor (coord2d-y c2) ))
)

(defmacro coord3f (x y)
  `(jnew +coord3f+ ,x ,y))
(java-field coord3f-x "x")
(java-field coord3f-y "y")
(java-field coord3f-z "z")
(java-func +coord3f+ coord3f-add "add" +coord3f+)
(java-func +coord3f+ coord3f-sub "sub" +coord3f+)
(java-func +coord3f+ coord3f-div "div" +coord3f+)
(java-func +coord3f+ coord3f-mul "mul" +coord3f+)
(java-func +coord3f+ coord3f-dist "dist2d" +coord3f+)
(java-func +coord3f+ coord3f-string "toString")
(java-func +coord3f+ coord3f-eq "equals" +object+)


(export '(coord coord-x coord-y
          coord-add coord-sub coord-div coord-mul coord-dist coord-string coord-eq coord-to-coord2d
          fakec

          coord2d coord2d-x coord2d-y
          coord2d-add coord2d-sub coord2d-div coord2d-mul coord2d-dist coord2d-string coord2d-floor coord2d-angle coord2d-eq coord2d-to-coord

          coord3f coord3f-x coord3f-y coord3f-z
          coord3f-add coord3f-sub coord3f-div coord3f-mul coord3f-dist coord3f-string coord3f-eq))
