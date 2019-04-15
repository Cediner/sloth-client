(defpackage :destump
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :destump)

(defun destroy (gob)
  (mv-smart-move-to-gob gob)
  (menu-use "paginae/act/dest")
  (mv-click-gob gob +left-button+ +mf-none+)
  (wait-for-progress)
  (mv-click-gob (my-gob) +right-button+ +mf-none+)
  (check-stam-and-drink))

(script
  (loop
     for gob = (gob-get-closest-by-filter-and-path
                (lambda (g)
                  (and 
                   (search "trees" (gob-name g)) 
                   (search "stump" (gob-name g)))))
     while gob
     do (destroy gob)))
