(defpackage :angler-scout
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :angler-scout)

(defconstant +angler+ `("gfx/kritter/caveangler/caveangler"))

(script
 (multiple-value-bind (token role)
     (prompt-for-discord-info)
   (let ((lake-name (prompt-for-input "Enter the lake name"))
         (char-name (character-name))
         (toggle 0))
     (start-discord-session token)
     (forever
      (let ((targets (gob-get-all-by-filter
                      (lambda (gob) 
                        (and 
                         (member (gob-name gob) +angler+ :test 'equal)
                         (not (is-gob-dead gob)))))))
        (if (> (length targets) 0) 
            (if (= 0 toggle) 
                (progn
                  (send-discord-message +discord-bot-channel+
                                        (format nil "<@&~A> Spotted an angler at ~A"
                                                role
                                                lake-name))
                  (setq toggle 1))
                (sleep 20))
            (progn
              (goto-character-screen)
              (sleep (* 60 20))
              (login char-name)
              (setq toggle 0)
              (sleep 20))))))))
