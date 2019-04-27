(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Discord 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(java-func +script+ script-send-discord-message "sendDiscordMessage" +string+ +string+)
(java-func +script+ script-start-discord-session "startDiscord" +string+)
(java-func +script+ script-end-discord "endDiscord")
(with-script-define send-discord-message script-send-discord-message channel message)
(with-script-define start-discord-session script-start-discord-session token)
(with-script-define end-discord-session script-end-discord)

(defconstant +discord-bot-channel+ "bot-alerts")

(defun prompt-for-discord-info ()
  (msg-listen)
  (widget-add (gui) (jnew "haven.sloth.gui.DiscordHelper") (coord 50 50))
  (let ((token nil)
        (role nil))
    (loop
       until (and token role)
       do (progn
            (sleep 1)
            (loop
               while (and (msg-has-message)
                          (null token)
                          (null role))
               do (let ((msg (msg-poll-message)))
                    (when (string= "discord" (msg-subject msg))
                      (setf token (aref (msg-args msg) 0)
                            role (aref (msg-args msg) 1)))))))
    (msg-stop-listening)
    (msg-clear-messages)
    (values token role)))

(export '(send-discord-message start-discord-session end-discord-session
          prompt-for-discord-info
          +discord-bot-channel+))
