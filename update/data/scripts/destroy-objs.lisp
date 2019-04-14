(defpackage :destroy-objs
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :destroy-objs)

(defun get-selected-gob ()
  (msg-listen)
  (chat-send-message (bot-chat) "Mark an object that you want to be destroyed")
  (let ((gob nil))
    (loop
       until gob
       do (progn
            (sleep 1)
            (loop
               while (and (msg-has-message) (null gob))
               do (let ((msg (msg-poll-message)))
                    (when (string= "click-gob" (msg-subject msg))
                      (setf gob (aref (msg-args msg) 0)))))))
    (msg-stop-listening)
    (msg-clear-messages)
    gob))

(defun destroy (gob)
  (mv-smart-move-to-gob gob)
  (menu-use "paginae/act/dest")
  (mv-click-gob gob +left-button+ +mf-none+)
  (wait-for-progress)
  (mv-click-gob (my-gob) +right-button+ +mf-none+))

(script
  (let ((name (gob-name (get-selected-gob))))
    (loop
       for gob = (gob-get-closest-by-name name)
       while gob
       do (destroy gob))))
    
