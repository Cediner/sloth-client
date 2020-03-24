(defpackage :dan-it
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))

(in-package :dan-it)

(defun follow (gid)
	(forever
		
	)
)

(defun main()
	(let ((gid 0))
		(let (
			(gob (prompt-for-selected-gob "Mark the object you wish to follow"))
			)
			(setf gid (gob-id gob))
		)
		(follow gid)
	)

)

(defun prompt-for-selected-gob-type (prompt type)
	(chat-send-message (bot-chat) prompt)
	(let (
		(gob (aref (msg-args (wait-for-filtered-message "(click-gob)" (lambda (msg) (is-gob-a (aref (msg-args msg) 0) type)))) 0))
		)
		gob
	)
)

(defun handle-command (command target)
	(chat-send-message (area-chat) (format nil "Command: ~A Target: ~A" command target)) 
	(when (string= command "lift")
		(progn
			(let (
				(gob (oc-get-gob target))
				)
				(mv-walk-path (mv-find-path-to-gob gob))
				(wait-for-movement)
				(menu-use "paginae/act/lift")
				(mv-click-gob gob +left-button+ +mf-none+)
				(wait-for-movement)
			)
		)
	)
)

(defun handle-request (command gid)
	;;(chat-send-message (area-chat) (format nil "Command: ~A" command)) 
	(chat-send-message (area-chat) (format nil "(dispatch follower-response (number ~A)(string lift)(number ~A))" (gob-id (my-gob)) gid))
	(chat-send-message (party-chat) (format nil "(dispatch follower-response (number ~A)(string lift)(number ~A))" (gob-id (my-gob)) gid))
)

(script
	(let (
		(gid (gob-id (prompt-for-selected-gob-type "Select the user you'd like to parrot" +human+)))
		(follow t)
		)
		(msg-listen "(follower-command)|(follower-request)")
		(let (
			(last-move 0)
			(last-known-pos (gob-rc (oc-get-gob gid)))
			)		
			(forever
				(let (
					(command (loop-through-messages (lambda (msg) (= gid (aref (msg-args msg) 0)))))
					)
					(when command
						(progn
							(if (string= "follower-command" (msg-subject command))
								(when (or (null (aref (msg-args command) 2)) (= (gob-id (my-gob)) (aref (msg-args command) 2)))
									(handle-command (aref (msg-args command) 1) (aref (msg-args command) 3))								
								)
								(handle-request (aref (msg-args command) 1) gid)
							)
						)
					)
				)
				(when follow
					(progn
						(let (
							(gob (oc-get-gob gid))
							)
							(if gob
								(progn
									(setf last-known-pos (gob-rc gob))
									(when (> (- (get-time) last-move 300))
										(let ((path (mv-find-path-to-gob gob)))
											(when path
												(mv-clear-moves)
												(loop for i from 0 to (1- (length path))
													do (mv-queue-move (move-destination (aref path i)))
												)
											)
										)
									)
								)
								(chat-send-message (area-chat) "Help my master is missing!")
							)
						)
					)
				)
				(sleep .5)
			)
		)
		(msg-stop-listening)
		(msg-clear-messages)
	)
)