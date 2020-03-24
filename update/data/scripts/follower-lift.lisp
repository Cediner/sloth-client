(defpackage :follower-lift
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))

(in-package :follower-lift)

(script
	(let (
		(lift-target (prompt-for-selected-gob "Select what you'd like a follower to pick up"))
		)
		(chat-send-message (area-chat) (format nil "(dispatch follower-request (number ~A)(string lift))" (gob-id (my-gob))))
		(chat-send-message (party-chat) (format nil "(dispatch follower-request (number ~A)(string lift))" (gob-id (my-gob))))
		(let (
			(follower-response (wait-for-filtered-message "(follower-response)" (lambda (msg) (= (gob-id (my-gob)) (aref (msg-args msg) 2)))))
			)
			(chat-send-message (party-chat) (format nil "(dispatch follower-command (number ~A)(string lift)(number ~A)(number ~A))" (gob-id (my-gob)) (aref (msg-args follower-response) 0) (gob-id lift-target)))
			(chat-send-message (area-chat) (format nil "(dispatch follower-command (number ~A)(string lift)(number ~A)(number ~A))" (gob-id (my-gob)) (aref (msg-args follower-response) 0) (gob-id lift-target)))
		)
	)
)