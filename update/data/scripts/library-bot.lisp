(defpackage :library-bot
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :library-bot)

(defconstant +commands+ `("Request" "Return" "Reset"))

(defun request (name iname quality)
	(let ((item (inventory-get-item-by-name (main-inventory) item-name)))
		(if item
			(let ((bandy (inventory-get-by-name "Bandy")))
				(if bandy
					(progn
						(item-transfer item)					
						(send-discord-message "axe-alerts" (format nil "~A took the q~A ~A" name quality iname))
						(mv-move-to (gob-rc (my-gob)))
					)
					(chat-send-message (area-chat) "Open a bandy window, then say Request")))
			(chat-send-message (area-chat) (format nil "q~A ~A is missing, check #axe-alerts to see who last requested it" quality iname)))))
			
(defun ret (name iname quality)
	(let ((bandy (inventory-get-by-name "Bandy")))
		(if bandy
			(let ((item (inventory-get-item-by-name bandy iname)))
				(if (and item (= (item-quality item) quality))
					(progn
						(item-transfer item)
						(send-discord-message "axe-alerts" (format nil "~A returned the q~A ~A" name quality iname))
						(mv-move-to (gob-rc (my-gob)))
					)
					(chat-send-message (area-chat) "Place the axe in then say Return")))
			(chat-send-message (area-chat) "Open a bandy window, place in the axe, then say Return"))))
			
(defun reset()
	(mv-move-to (gob-rc (my-gob)))
)
			
(script
 (multiple-value-bind (token role)
     (prompt-for-discord-info)
	(let ((hs (prompt-for-input "Enter the HS of this character"))
		  (item (inventory-get-item-by-name (main-inventory) (prompt-for-input "Enter the name of the item"))))
		(if item
			(let ((iname (item-name item))
				  (quality (item-quality item)))
				(start-discord-session token)
				(msg-listen "(area-msg)")
				(forever
					(let ((message (loop-through-messages (lambda (msg) (member (aref (msg-args msg) 0) +commands+ :test 'equal)))))
						(when message 
							(let ((sender (aref (msg-args message) 1))
								  (command (aref (msg-args message) 0)))
								(if (string= sender "???")
									(chat-send-message (area-chat) (format nil "Please Kin HS: ~A before gilding." hs))
									(cond 
										((string= command "Request") (request sender iname quality))
										((string= command "Return") (ret sender iname quality))
										((string= command "Reset") (reset)))))))
					(sleep .5)))
			(chat-send-message (bot-chat) "Item not found")))))