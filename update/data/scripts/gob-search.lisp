(defpackage :gob-search
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :gob-search)

(script	
	(let ((s (prompt-for-input "Enter a search string for gobs")))
		(let ((gobs (gob-get-all-by-filter (lambda (gob) (search s (gob-name gob))))))
			(chat-send-message (bot-chat) (format nil "~A gobs found" (length gobs)))
			(dolist (gob gobs)
				(chat-send-message (party-chat) (format nil "$Mark{~A,20000}" (gob-id gob)))
				(sleep .5)
			)
		)
	)
)