(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Kin/Friends List/BuddyWnd stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(java-field gui-buddies "buddies")

(defconstant +buddywnd+ "haven.BuddyWnd")
(defun buddy ()
  (gui-buddies (ui-gui (ui))))

(java-func +buddywnd+ buddy-buddies "buddies")

(defmacro kin-add-by-secret (secret)
  `(wdgmsg (buddy) "bypwd" ,secret))

(defmacro kin-set-secret (secret)
  `(wdgmsg (buddy) "pwd" ,secret))

(defmacro kin-set-presentation-name (name)
  `(wdgmsg (buddy) "pname" ,name))

(defmacro kin-look-at-buddy (buddy-id)
  `(wdgmsg (buddy) "ch" ,buddy-id))

;;Return: A list not array
(defun kin-get-buddies ()
  (listify (buddy-buddies (buddy))))

(defconstant +buddy+ "haven.BuddyWnd$Buddy")
(defconstant +buddy-online+   1)
(defconstant +buddy-offline+  0)
(defconstant +buddy-unknown+ -1)
(defconstant +buddy-group-white+  0)
(defconstant +buddy-group-green+  1)
(defconstant +buddy-group-red+    2)
(defconstant +buddy-group-blue+   3)
(defconstant +buddy-group-aqua+   4)
(defconstant +buddy-group-yellow+ 5)
(defconstant +buddy-group-purple+ 6)
(defconstant +buddy-group-pink+   7)
(java-field buddy-id "id")
(java-field buddy-name "name")
(java-field buddy-online "online")
(java-field buddy-group "group")
(java-field buddy-seen "seen")
(java-func +buddy+ buddy-forget "forget")
(java-func +buddy+ buddy-dekin "endkin")
(java-func +buddy+ buddy-chat "chat")
(java-func +buddy+ buddy-invite "invite")
(java-func +buddy+ buddy-describe "describe")
(java-func +buddy+ buddy-change-name "chname" +string+)
(java-func +buddy+ buddy-change-group "chgrp" +int+)

(export '(kin-add-by-secret kin-set-secret kin-set-presentation-name kin-look-at-buddy
          kin-get-buddies
          +buddy-online+ +buddy-offline+ +buddy-unknown+
          +buddy-group-white+ +buddy-group-green+ +buddy-group-red+ +buddy-group-blue+
          +buddy-group-aqua+ +buddy-group-yellow+ +buddy-group-purple+ +buddy-group-pink+
          buddy-id buddy-name buddy-online buddy-group buddy-seen
          buddy-forget buddy-dekin buddy-chat buddy-invite buddy-describe
          buddy-change-name buddy-change-group))
