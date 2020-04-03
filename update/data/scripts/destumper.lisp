(defpackage :destump
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :destump)

(defun destroy (gob)
  (loop
     do (progn
          (mv-smart-move-to-gob gob)
		  (wait-for-movement)
          (menu-use "paginae/act/dest")
          (mv-click-gob gob +left-button+ +mf-none+)
          (wait-for-progress)
          (mv-click-gob (my-gob) +right-button+ +mf-none+))
     while (and (oc-get-gob (gob-id gob))
                (check-stam-and-drink))))

(script
 (let ((bad ())
	   (bbox (get-bbox "Select an area to destump")))
   (loop
      for gob = (gob-get-closest-by-filter
                 (lambda (g)
                   (and 
                    (search "trees" (gob-name g)) 
                    (search "stump" (gob-name g))
                   (not (member (gob-id g) bad))
				   (bbox-within bbox (gob-rc g)))))
      while gob
      do (progn
		   (check-for-starving)
           (destroy gob)
           (when (oc-get-gob (gob-id gob))
             (push (gob-id gob) bad))))))
   
