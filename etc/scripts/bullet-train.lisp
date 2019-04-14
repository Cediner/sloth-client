(defpackage :bullet-train
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :bullet-train)

(script
  (let ((last-chair nil)
        (cur-chair nil))
    (loop
       do (let ((next-chair (gob-get-closest-by-filter
                             (lambda (gob)
                               (and (or (null last-chair)
                                        (not (jeq gob last-chair)))
                                    (or (null cur-chair)
                                        (not (jeq gob cur-chair)))
                                    (search "chair" (gob-name gob)))))))
            (mv-click-gob next-chair +right-button+ +mf-none+)
            (loop
               until (coord2d-eq (gob-rc next-chair) (gob-rc (my-gob))))
            (setf last-chair cur-chair)
            (setf cur-chair next-chair)))))
