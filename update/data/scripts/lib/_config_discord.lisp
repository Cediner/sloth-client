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

(export '(send-discord-message start-discord-session end-discord-session
          +discord-bot-channel+))
