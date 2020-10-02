(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MapView API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +mapview+ "haven.MapView")
(java-field mv-plgob "rlplgob")
(java-func +mapview+ mv-has-moves "hasmoves")
(java-func +mapview+ mv-find-path-1 "findpath" +coord2d+)
(java-func +mapview+ mv-find-path-to-gob-1 "findpath" +gob+)
(java-func +mapview+ mv-move-to-1 "moveto" +coord2d+)
(java-func +mapview+ mv-move-to-rel-1 "relMove" +coord2d+)
(java-func +mapview+ mv-placing-gob-1 "placing")
;;(java-func +mapview+ mv-path-to-1 "pathto" +coord2d+)
;;(java-func +mapview+ mv-path-to-gob-1 "pathto" +gob+)
(java-func +mapview+ mv-clear-moves-1 "clearmovequeue")
(java-func +mapview+ mv-queue-move-1 "queuemove" +coord2d+) 
(java-func +mapview+ mv-los-1 "los" +coord2d+)
(java-func +mapview+ mv-los-gob-1 "los" +gob+)

(defmacro mv-clear-moves ()
  `(mv-clear-moves-1 (mv)))

(defmacro mv-queue-move (c)
  `(mv-queue-move-1 (mv) ,c))

(defmacro mv-move-to (mc)
  `(mv-move-to-1 (mv) ,mc))
(defmacro mv-move-to-rel (offset)
  `(mv-move-to-rel-1 (mv) ,offset))

(defmacro mv-los (mc)
  `(mv-los-1 (mv) ,mc))
(defmacro mv-los-gob (gob)
  `(mv-los-gob-1 (mv) ,gob))

(defmacro mv-find-path (c)
  `(mv-find-path-1 (mv) ,c))
(defmacro mv-find-path-to-gob (g)
  `(mv-find-path-to-gob-1 (mv) ,g))

(defmacro mv-click (c btn flags)
  `(wdgmsg (mv) "click" (fakec) (oc-posres ,c) ,btn ,flags))

(defmacro mv-click-gob (gob btn flags &key (overlay-id nil) (fastmesh-id nil))
  `(wdgmsg (mv) "click" (fakec) (oc-posres (gob-rc ,gob)) ,btn ,flags
           ,(if overlay-id 1 0)
           (gob-id ,gob) (oc-posres (gob-rc ,gob))
           ,(if overlay-id overlay-id 0)
           ,(if fastmesh-id fastmesh-id -1)))

;;Just like click the args are:
;;mouse-c map-c btn modifier-flags [contains-overlay gob-id gob-rc overlay-id fastmesh-id]
(defmacro mv-interact-held-item-with-gob (gob flags &key (overlay-id nil) (fastmesh-id nil))
  `(wdgmsg (mv) "itemact" (fakec) (oc-posres (gob-rc ,gob)) ,flags
           ,(if overlay-id 1 0)
           (gob-id ,gob) (oc-posres (gob-rc ,gob))
           ,(if overlay-id overlay-id 0)
           ,(if fastmesh-id fastmesh-id -1)))

(defmacro mv-interact-held-item-with-tile (tile flags)
  `(wdgmsg (mv) "itemact" (fakec) (oc-posres ,tile) ,flags))

(defmacro mv-select-area (sc ec)
  `(wdgmsg (mv) "sel" ,sc ,ec +mf-none+))

(defmacro mv-drop (coord flags)
  `(wdgmsg (mv) "drop" (fakec) (oc-posres ,coord) ,flags))

(defun mv-itemact (coord flags &optional gob)
  (if gob
      (wdgmsg (mv) "itemact" (fakec) coord flags)
      (wdgmsg (mv) "itemact" (fakec) coord flags 0 (gob-id gob) (oc-posres (gob-rc gob)) 0 -1)))

(defun mv-placing-gob ()
  (mv-placing-gob-1 (mv)))

(defmacro mv-place-gob (coord angle btn flags)
  `(wdgmsg (mv) "place" (oc-posres ,coord) ,angle ,btn ,flags))

(defmacro wait-for-placing-gob (&key (refresh 100) timeout)
  `(wait-until #'mv-placing-gob :refresh ,refresh :timeout ,timeout))

(defmacro wait-for-placing-gob-to-be-gone (&key (refresh 100) timeout)
  `(wait-until (lambda () (null (mv-placing-gob))) :refresh ,refresh :timeout ,timeout))
  
(export '(mv-move-to mv-move-to-rel mv-los mv-los-gob mv-plgob
          mv-find-path mv-find-path-to-gob
          mv-clear-moves mv-queue-move
          mv-click-gob mv-interact-held-item-with-gob mv-interact-held-item-with-tile
          mv-click mv-select-area mv-drop
          mv-placing-gob mv-place-gob wait-for-placing-gob wait-for-placing-gob-to-be-gone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Pathfinding bits
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +move+ "haven.sloth.script.pathfinding.Move")
(defmacro move (dest)
  `(jnew +move+ ,dest))
(java-func +move+ move-apply "apply" +mapview+)
(java-func +move+ move-destination "dest")

(defun mv-path-distance (moves)
  (reduce #'+ (loop
                 for i from 0 to (1- (length moves))
                 collect (coord2d-dist (if (> i 0)
                                           (move-destination (aref moves (1- i)))
                                           (gob-rc (my-gob)))
                                       (move-destination (aref moves i))))))

(defun mv-walk-path (moves)
  (doarr (move moves)
    (move-apply move (mv))
    (wait-for-movement :gob (my-gob))))

(defun mv-reverse-path (moves goal-c)
  (let ((nmoves (make-array (length moves))))
    (loop
       for j from 0 to (- (length moves) 2)
       for i = (- (- (length moves) 2) j)
       do (setf (aref nmoves i) (aref moves j)))
    (setf (aref nmoves (1- (length nmoves))) (move goal-c))
    nmoves))

(defun mv-path-to (mc)
  (let ((path (mv-find-path mc)))
    (mv-walk-path path)))

(defun mv-path-to-gob (gob)
  (let ((path (mv-find-path-to-gob gob)))
    (mv-walk-path path)))

(defun mv-smart-move (mc)
  (if (mv-los mc)
      (progn
        (mv-move-to mc)
        (wait-for-movement :gob (my-gob)))
      (mv-path-to mc)))

(defun mv-smart-move-to-gob (gob)
  (if (mv-los-gob gob)
      (progn
        (mv-move-to-gob gob)
        (wait-for-movement :gob (my-gob)))
      (mv-path-to-gob gob)))

(export '(move move-apply move-destination
          mv-walk-path mv-reverse-path mv-path-distance
          mv-path-to mv-path-to-gob mv-smart-move mv-smart-move-to-gob))
