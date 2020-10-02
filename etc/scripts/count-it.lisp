(defpackage :count-it
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))

(in-package :count-it)

(script
  (let ((name (gob-name (prompt-for-selected-gob "Mark an object to count"))))
    (chat-send-message (bot-chat)
                       (format nil "There are ~A ~A in view~%"
                               (length (gob-get-all-by-name name))
                               name))))
