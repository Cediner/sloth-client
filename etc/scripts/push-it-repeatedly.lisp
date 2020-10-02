(defpackage :push-it-repeatedly
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))

(in-package :push-it-repeatedly)

(script
  (let ((gid 0)
        (last-move 0))
    (let ((gob (prompt-for-selected-gob "Mark an object that you want to push")))
      (setf gid (gob-id gob)))
    (loop
       for gob = (oc-get-gob gid)
       while gob
       do (progn
            (menu-use "paginae/act/push")
            (sleep 0.5)
            (mv-click-gob gob +left-button+ +mf-none+)
            (sleep 1)))))
            
