(defpackage :debusher
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :debusher)

(defun chop (gob)
  (loop
     do (progn
          (mv-smart-move-to-gob gob)
		  (wait-for-movement)
          (mv-click-gob gob +right-button+ +mf-ctrl+)
          (wait-for-flowermenu)
          (when (flowermenu)
            (flowermenu-select-by-name "Chop"))
          (wait-for-progress))
     while (and (oc-get-gob (gob-id gob))
                (check-stam-and-drink))))

(script
 (let ((bad ())
	   (bbox (get-bbox "Select an area to debush")))
   (loop
      for gob = (gob-get-closest-by-filter
                 (lambda (g)
                   (and 
                    (search "bushes" (gob-name g))                    
                   (not (member (gob-id g) bad))
				   (bbox-within bbox (gob-rc g)))))
      while gob
      do (progn
		   (check-for-starving)
           (chop gob)
           (when (oc-get-gob (gob-id gob)) (push (gob-id gob) bad))))))
