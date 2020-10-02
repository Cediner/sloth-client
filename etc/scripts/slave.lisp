(defpackage :slave
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))

(in-package :slave)

;;Commands
;;gob based
(defparameter *go-to-gob* "go-to-gob")
(defparameter *follow* "follow-gob")
(defparameter *lift* "lift-gob")
(defparameter *destroy* "destroy-gob")
(defparameter *chop* "chop-gob")
(defparameter *chip* "chip-gob")
;;tile based
(defparameter *go-to* "go-to")
(defparameter *dig* "dig")
(defparameter *place-item* "place-item")
(defparameter *shoot-at-tile* "shoot-at-tile")


;;Check our messages for the next state, nil if no change
(defun check-messages ()
  (if (msg-has-message)
      (let* ((msg (msg-poll-message))
             (command (msg-subject msg))
             (args (msg-args msg)))
        (format t "Message [~A] - ~A~%" command args)
        (cond ((string= command *go-to*)
               `(:goto ,(aref args 0)))
              ((string= command *dig*)
               `(:dig ,(aref args 0)))
              ((string= command *place-item*)
               `(:place-item ,(aref args 0)))
              ((string= command *shoot-at-tile*)
               `(:shoot-at-tile ,(aref args 0)))
              ((string= command *go-to-gob*)
               `(:go-to ,(aref args 1)))
              ((string= command *follow*)
               `(:follow ,(aref args 0) ,(aref args 1)))
              ((string= command *lift*)
               `(:lift ,(aref args 0) ,(aref args 1)))
              ((string= command *destroy*)
               `(:destroy ,(aref args 0) ,(aref args 1)))
              ((string= command *chop*)
               `(:chop ,(aref args 0) ,(aref args 1)))
              ((string= command *chip*)
               `(:chip ,(aref args 0) ,(aref args 1)))
              (t nil)))
      nil))

(defun goto (position)
  (let ((path (mv-find-path position)))
    (when path
      (mv-clear-moves)
      (loop
         for i from 0 to (1- (length path))
         do (mv-queue-move (move-destination (aref path i))))))
  `(:waiting-for-command))

(script
 (let ((state `(:waiting-for-command)))
   ;;Start up listener
   (msg-listen "(go-to)|(place-item)|(shoot-at-tile)")
   (forever
    ;;update state from messages
    (let ((nstate (check-messages)))
      (when nstate
        (setf state nstate)))
    ;;Perform any action given by the state
    (let ((command (car state)))
      (setf state
            (cond
              ((eq command :goto)
               (goto (car (cdr state))))
              (t `(:waiting-for-command)))))
    (sleep  0.2))))
