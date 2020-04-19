(load "data/scripts/lib/_common_scout.lisp")
(defpackage :peek-scout
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config
        :scout-common))
(in-package :peek-scout)

(defconstant +peek-limit+ 25000)

(defun peek-outside (door)
  (let ((gob (gob-get-closest-by-name door)))
    (if gob
        (progn
          (mv-click-gob gob +right-button+ +mf-shift+)
          t)
        nil)))

(defun cancel-peek ()
  (mv-click (coord2d 1 1) +right-button+ +mf-none+))
 
(script
 (multiple-value-bind (token role)
     (prompt-for-discord-info)
   (let ((door (gob-name (prompt-for-selected-gob "Select the door object to peek through")))
         (spotted-gobs (make-hash-table :test 'equal))
         (tick 0)
         (state :inside)
         (started-peek 0))
     (start-discord-session token)
     (forever
      (if (eq state :inside)
          (when (peek-outside door)
            (setf state :outside)
           (setf started-peek (get-time)))
          (progn
            (scan-for-targets role spotted-gobs tick)
            (check-if-any-spotted-have-left role spotted-gobs tick)
            (incf tick)
            (when (>= (- (get-time) started-peek) +peek-limit+)
              (cancel-peek)
              (setf state :inside))))
      (sleep 0.5)))))
             
           
