(defpackage :gild-bot
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :gild-bot)

(defconstant +commands+ `("Reset" "Check" "Gild"))

(defun check ()
	(let ((bandy (inventory-get-by-name "Bandy")))
		(if bandy
			(let ((artifact (inventory-item-at bandy (coord 0 0)))
				  (gilding (inventory-item-at bandy (coord 1 0))))
				(if (and artifact gilding)
					(progn
						(item-take gilding)
						(wait-until (lambda () (held-item)))
						(item-interact-with-held-item artifact +mf-none+)
						(wait-for-flowermenu)
						(chat-send-message (area-chat) (flowermenu-get-petal-name 0))
						(inventory-place-item bandy (coord 1 0)))
					(chat-send-message (area-chat) "Missing the artifact or gilding")))
			(chat-send-message (area-chat) "Open a bandy window and place the items you'd like to check gilding chance with"))))

(defun reset ()
	(let ((bandy (inventory-get-by-name "Bandy")))
		(when (and bandy (held-item))
			(inventory-place-item bandy (coord 1 0))
			(wait-until (lambda () (null (held-item))))))
    (mv-move-to (gob-rc (my-gob))))

(defun save (name artifact gilding)
	(let ((aname (item-name artifact))
		  (aq (item-quality artifact))
		  (gname (item-name gilding))
		  (gq (item-quality gilding)))
		(ensure-directories-exist "data/scripts/gildings/")
		(with-open-file (fd (format nil "data/scripts/gildings/~A~A.dat" name (get-time))
							:direction :output
							:if-exists :supersede)
			(format fd "Artifact: ~A ~A~%" aname aq)
			(format fd "Gilding: ~A ~A~%" gname gq))))

(defun gild (name)
	(let ((bandy (inventory-get-by-name "Bandy")))
		(if bandy
			(let ((artifact (inventory-item-at bandy (coord 0 0)))
				  (gilding (inventory-item-at bandy (coord 1 0))))
				(if (and artifact gilding)
					(progn
						(save name artifact gilding)
						(item-take gilding)
						(wait-until (lambda () (held-item)))
						(item-interact-with-held-item artifact +mf-none+)
						(wait-for-flowermenu)
						(flowermenu-select 0))
					(chat-send-message (area-chat) "Missing the artifact or gilding")))
			(chat-send-message (area-chat) "Open a bandy window and place the items you'd like to gild"))))

(script
	(let ((hs (prompt-for-input "Enter the HS of this character")))
		(msg-listen "(area-msg)")
		(forever
			(let ((message (loop-through-messages (lambda (msg) (member (aref (msg-args msg) 0) +commands+ :test (lambda (a b) (string-equal a b)))))))
				(when message 
					(let ((sender (aref (msg-args message) 1))
						  (command (aref (msg-args message) 0)))
						(if (string= sender "???")
							(chat-send-message (area-chat) (format nil "Please Kin HS: ~A before gilding." hs))
							(cond 
								((string-equal command "Reset") (reset))
								((string-equal command "Check") (check))
								((string-equal command "Gild") (gild sender)))))))
			(sleep .5))))