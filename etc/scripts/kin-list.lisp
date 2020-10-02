(defpackage :kin-list
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :kin-list)

(defun kin-get-buddy-ids ()
	(let ((ret ())
		 )
		(dolist (buddy (kin-get-buddies))
			(push (buddy-id buddy) ret)
		)
		ret
	)
)

(defun get-color-const (color-word)
	(cond ((string= (string-downcase color-word) "white") (return-from get-color-const +buddy-group-white+))
		  ((string= (string-downcase color-word) "green") (return-from get-color-const +buddy-group-green+))
		  ((string= (string-downcase color-word) "red") (return-from get-color-const +buddy-group-red+))
		  ((string= (string-downcase color-word) "blue") (return-from get-color-const +buddy-group-blue+))
		  ((string= (string-downcase color-word) "aqua") (return-from get-color-const +buddy-group-aqua+))
		  ((string= (string-downcase color-word) "yellow") (return-from get-color-const +buddy-group-yellow+))
		  ((string= (string-downcase color-word) "purple") (return-from get-color-const +buddy-group-purple+))
		  ((string= (string-downcase color-word) "pink") (return-from get-color-const +buddy-group-pink+))
	)
	(return-from get-color-const +buddy-group-white+)
)

(script
	(let ((input (open "hearthsecrets.txt" :if-does-not-exist nil))
		  (old (kin-get-buddy-ids))
		  (color (get-color-const (prompt-for-input "Enter the color you would like to set your new friends to")))
		 )
		(when input
			(loop for line = (read-line input nil)
				while line do (progn 
					(kin-add-by-secret line)
					(sleep .5)
				)
			)
			(close input)
			(sleep 2)
			(dolist (new (kin-get-buddies))
				(if (not(member (buddy-id new) old))
						(buddy-change-group new color)
				)
			)
		)
	)
)