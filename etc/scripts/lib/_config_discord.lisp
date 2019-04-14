(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Discord 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(java-func +script+ script-send-discord-message "sendDiscordMessage" +string+ +string+ +string+)
(with-script-define send-discord-message script-send-discord-message token channel message)

(defconstant +discord-bot-channel+ "bot-alerts")

(export '(send-discord-message +discord-bot-channel+))
