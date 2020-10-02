(load "data/scripts/lib/_common_scout.lisp")
(defpackage :scout
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config
        :scout-common))
(in-package :scout)

(script
 (multiple-value-bind (token role)
     (prompt-for-discord-info)
   (let ((spotted-gobs (make-hash-table :test 'equal))
         (tick 0))
     (start-discord-session token)
     (forever
      (scan-for-targets role spotted-gobs tick)
      (check-if-any-spotted-have-left role spotted-gobs tick)
      (incf tick)
      (sleep 1)))))
