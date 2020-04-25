(defpackage :party-perms
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :party-perms)

(script	
	(chat-send-message (bot-chat) "This bot looks in partyperms.txt for character names to invite to party when they come online.")
	(let ((input (open "partyperms.txt" :if-does-not-exist nil))
		  (names ())
		  (statuses ()))
		(when input
			(loop for line = (read-line input nil)
				while line do (progn 
					(push line names)
					(push +buddy-unknown+ statuses)))
			(forever
				(let ((new-statuses ()))
					(loop 
					  for name in names 
					  for status in statuses
					  do(let ((buddy (kin-get-buddy-by-name name)))
							(if buddy
								(let ((currentstatus (buddy-online buddy))) 
									(push currentstatus new-statuses)
									(when (and (not (= status currentstatus)) (= currentstatus +buddy-online+))
										(buddy-invite buddy)))
								(push +buddy-unknown+ new-statuses))))
					(setq statuses new-statuses))
				(sleep 5)))))