(in-package :hafen-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Script API and Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +message+ "haven.sloth.script.Script.Message")
(java-sfunc +script+ my-script "myself")
(defmacro with-script-define (lisp-fun-name script-fun &rest args)
  `(defmacro ,lisp-fun-name ,args
     `(,',script-fun (my-script) ,,@args)))

(java-field script-session "session")
(java-func +script+ script-listen "listen" +string+)
(java-func +script+ script-stop-listening "stopListening")
(java-func +script+ script-clear-messages "clearmsgs")
(java-func +script+ script-has-message "hasmsg")
(java-func +script+ script-poll-message "pollmsg")
(with-script-define msg-listen-1 script-listen)
(with-script-define msg-stop-listening script-stop-listening)
(with-script-define msg-clear-messages script-clear-messages)
(with-script-define msg-has-message script-has-message)

(defun msg-listen (&optional (filter ".+"))
  (msg-listen-1 filter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; msg-poll-message
;; Returns the next message queued up, if any, or throws an exception if none.
;;
;; Known Messages:
;;   Chat Related
;;      Bot Chat - Subject: "msg" Args: (text)
;;      Area Chat - Subject: "area-msg" Args: (text, from-name)
;;      Realm Chat - Subject: "realm-msg" Args: (text, from-name)
;;      Village chat -  Subject: "village-msg" Args: (text, from-name)
;;      Party Chat - Subject: "pt-msg" Args: (text, from-name)
;;      Private Chat
;;         Inbound - Subject: "priv-in-msg" Args: (text, from-name)
;;         Outbound - Subject: "priv-out-msg" Args: (text)
;;   Tile Selection - Subject: "bot-select" Args( starting-coord, ending-coord )
;;   Mark Gob - Subject: "click-gob" Arg: (gob)
;;   Mark Tile - Subject: "click-tile" Arg: (tile-coord)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-script-define msg-poll-message script-poll-message)

(java-field msg-sender "sender")
(java-field msg-subject "msg")
(java-field msg-args "args")


;;Basics from session
(defmacro with-session-define (lisp-fun-name session-fun &rest args)
  `(defmacro ,lisp-fun-name ,args
     `(,',session-fun (script-session (my-script)) ,,@args)))
  
(java-func +session+ session-ui "getUI")
(java-func +session+ session-username "username")
(java-func +session+ session-character-name "chrname")
(java-field session-north "north")
(java-field session-south "south")
(java-field session-west "west")
(java-field session-east "east")

(with-session-define ui session-ui)
(with-session-define username session-username)
(with-session-define character-name session-character-name)
(with-session-define north session-north)
(with-session-define south session-south)
(with-session-define west session-west)
(with-session-define east session-east)

;;Helpers
(java-field ui-gui "gui")
(java-field ui-sess "sess")
(java-field sess-glob "glob")
(java-field glob-oc "oc")
(java-field glob-mc "map")
(java-field gui-map "map")

;;;;shortcuts
(defun gui ()
  (ui-gui (ui)))
(defun mv ()
  (gui-map (ui-gui (ui))))
(defun oc ()
  (glob-oc (sess-glob (ui-sess (ui)))))
(defun mc ()
  (glob-mc (sess-glob (ui-sess (ui)))))

(export '(msg-listen msg-stop-listening msg-clear-messages msg-has-message msg-poll-message
          msg-sender msg-subject msg-args
          ui username character-name north south west east
          gui mv oc mc))
