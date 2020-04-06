(load "data/scripts/lib/_common_scout.lisp")
(defpackage :peak-scout
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config
        :scout-common))
(in-package :peak-scout)

(defconstant +peak-limit+ 25000)

(defun peak-outside (door)
  (let ((gob (gob-get-closest-by-name door)))
    (if gob
        (progn
          (mv-click-gob gob +right-button+ +mf-shift+)
          t)
        nil)))

(defun cancel-peak ()
  (mv-click (coord2d 1 1) +right-button+ +mf-none+))
 
(script
 (multiple-value-bind (token role)
     (prompt-for-discord-info)
   (let ((door (gob-name (prompt-for-selected-gob "Select the door object to peak through")))
         (spotted-gobs (make-hash-table :test 'equal))
         (tick 0)
         (state :inside)
         (started-peak 0))
     (start-discord-session token)
     (forever
      (if (eq state :inside)
          (when (peak-outside door)
            (setf state :outside)
           (setf started-peak (get-time)))
          (progn
            (scan-for-targets role spotted-gobs tick)
            (check-if-any-spotted-have-left role spotted-gobs tick)
            (incf tick)
            (when (>= (- (get-time) started-peak) +peak-limit+)
              (cancel-peak)
              (setf state :inside))))
      (sleep 0.5)))))
             
           
