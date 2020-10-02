(defpackage :fuel-smelter-12
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :fuel-smelter-12)

(defun fuel (gob)
  (let ((itms (inventories-get-items-by-filter (lambda (itm)
                                                 (string= "Coal" (item-name itm))))))
    (when (>= (length itms) 12)
      (loop
         for i from 1 to 12
         do (let ((itm (nth i itms)))
              (item-take itm)
              (wait-until (lambda () (held-item)))
              (mv-interact-held-item-with-gob gob +mf-none+)
              (wait-until (lambda () (null (held-item)))))))))

(script
 (let ((gob (prompt-for-selected-gob "Mark the smelter to fuel")))
   (fuel gob)))
