(in-package :hafen-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Logging for the scripts
;;;; The Chat section of this could use some work
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +chat+ "haven.ChatUI")
(defconstant +channel+ "haven.ChatUI$EntryChannel")
(defconstant +privchat+ "haven.ChatUI$PrivChat")
(defconstant +gameui+ "haven.GameUI")
(java-field gui-chat "chat")
(java-field chat-area "area")
(java-field chat-party "party")
(java-field chat-realm "realm")
(java-field chat-village "village")
(java-field gui-bot-chat "botlog")
(java-func +chat+ chat-privchats "privchats")
(java-func +gameui+ sys-msg "msg" +string+)
(java-func +script+ log-msg "log" +string+)
(java-func +channel+ channel-send "send" +string+)
(java-func +privchat+ privchat-name "name")

(defun chat ()
  (gui-chat (ui-gui (ui))))

(defun area-chat ()
  (chat-area (chat)))
(defun party-chat ()
  (chat-party (chat)))
(defun village-chat ()
  (chat-village (chat)))
(defun bot-chat ()
  (gui-bot-chat (ui-gui (ui))))
(defun realm-chat ()
  (chat-realm (chat)))

(defun privchats ()
  (chat-privchats (chat)))

(defun privchat-by-name (name)
  (doarr (chat (privchats))
    (when (string= name (privchat-name chat))
      (return-from privchat-by-name chat)))
  nil)


(defun misc-sysprint (msg)
  (sys-msg (ui-gui (ui)) msg))

(defun misc-log (msg)
  (log-msg (my-script) msg))

(defun chat-send-message (chat msg)
  (if (jeq chat (gui-bot-chat (ui-gui (ui))))
      (uimsg chat "msg" (format nil "[Bot] ~A" msg) (svar "java.awt.Color" "RED") 1)
      (channel-send chat msg)))
  
(export '(chat area-chat party-chat village-chat bot-chat realm-chat privchats privchat-by-name
          privchat-name
          chat-send-message
          misc-sysprint misc-log))
