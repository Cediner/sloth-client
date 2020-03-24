(defpackage :destroy-objs
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :destroy-objs)

(defun destroy (gob)
  (mv-smart-move-to-gob gob)
  (menu-use "paginae/act/dest")
  (mv-click-gob gob +left-button+ +mf-none+)
  (wait-for-progress)
  (mv-click-gob (my-gob) +right-button+ +mf-none+)
  (check-stam-and-drink))

(script
  (let ((name (gob-name (prompt-for-selected-gob "Mark an object that you want to be destroyed"))))
    (loop
       for gob = (gob-get-closest-by-name-and-path name)
       while gob
       do (destroy gob))))
