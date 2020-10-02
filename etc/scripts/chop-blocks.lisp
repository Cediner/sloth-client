(defpackage :chop-blocks
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))

(in-package :chop-blocks)

(defun get-next-log ()
  (gob-get-closest-by-filter
   (lambda (gob)
     (search "log" (gob-name gob)))))

(defun chop-log (log)
  (mv-smart-move-to-gob log)
  (loop
     while (oc-get-gob (gob-id log))
     do (progn
          (mv-drop (gob-rc log) +mf-none+)
          (sleep 1)
          (mv-click-gob log +right-button+ +mf-none+)
          (wait-for-flowermenu)
          (when (flowermenu)
            (flowermenu-select-by-name "Chop into blocks")
            (wait-for-progress-to-start)
            (loop
               until (= (progress) -1.0))))))

          
(script
 (loop
    for log = (get-next-log)
    while log
    do (chop-log log)))
