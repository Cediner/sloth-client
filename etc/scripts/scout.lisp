(defpackage :scout
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :scout)

(defconstant +alert-refresh+ (* 3 60 1000))

(defstruct spotted-gob
  id
  name
  last-spotted
  last-alert
  tick)

(defun scan-for-targets (token role spotted-gobs tick)
  (let ((targets (gob-get-all-by-filter
                  (lambda (gob)
                    (or (string= (gob-name gob) "gfx/terobjs/vehicle/knarr")
                        (and (jeq (gob-type gob) +human+)
                             (not (is-gob-friendly gob))
                             (not (jeq gob (my-gob)))))))))
    (loop
       for target in targets
       do (let ((name (if (jeq (gob-type target) +human+) (gob-kinname target) "knarr"))
                (extra (if (jeq (gob-type target) +human+)
                           (let ((equs (gob-equipment target)))
                             (if equs
                                 (apply #'concatenate 'string
                                        (loop
                                           for equ in (listify equs)
                                           collect (format nil "~A~%" equ)))
                                 nil))
                           nil)))
            (cond
              ((and (gethash (gob-id target) spotted-gobs)
                    (> (- (get-time) (spotted-gob-last-alert (gethash (gob-id target) spotted-gobs)))
                       +alert-refresh+))
               (let ((gob (gethash (gob-id target) spotted-gobs)))
                 (setf (spotted-gob-last-spotted gob) (gob-rc target))
                 (setf (spotted-gob-tick gob) tick)
                 (setf (spotted-gob-last-alert gob) (get-time))
                 (send-discord-message token +discord-bot-channel+
                                       (format nil "<@&~A> Still spotted a ~A [~A] @ ~A"
                                               role
                                               name
                                               (gob-id target)
                                               (get-real-location-on-map (gob-rc target))))))
              ((gethash (gob-id target) spotted-gobs)
               (let ((gob (gethash (gob-id target) spotted-gobs)))
                 (setf (spotted-gob-last-spotted gob) (gob-rc target))
                 (setf (spotted-gob-tick gob) tick)))
              (t ;;new target
               (let ((gob (make-spotted-gob :id (gob-id target)
                                            :name name
                                            :last-spotted (gob-rc target)
                                            :last-alert (get-time)
                                            :tick tick)))
                 (setf (gethash (spotted-gob-id gob) spotted-gobs) gob)
                 (send-discord-message token +discord-bot-channel+
                                       (format nil "<@&~A> Spotted a ~A [~A] @ ~A"
                                               role
                                               name
                                               (gob-id target)
                                               (get-real-location-on-map (gob-rc target))))
                 (when extra
                   (send-discord-message token +discord-bot-channel+
                                         (format nil "They were wearing:~%~A" extra))))))))))

(defun check-if-any-spotted-have-left (token role spotted-gobs tick)
  (loop
     for id being the hash-keys in spotted-gobs
     when (/= tick (spotted-gob-tick (gethash id spotted-gobs)))
     do (let ((gob (gethash id spotted-gobs)))
          (remhash id spotted-gobs)
          (send-discord-message token +discord-bot-channel+
                                (format nil "<@&~A> Last spotted a ~A [~A] @ ~A"
                                        role
                                        (spotted-gob-name gob)
                                        (spotted-gob-id gob)
                                        (get-real-location-on-map (spotted-gob-last-spotted gob)))))))

(script
  (let ((token (prompt-for-input "Enter your discord bot token"))
        (role (prompt-for-input "Enter the mention role ID"))
        (spotted-gobs (make-hash-table :test 'equal))
        (tick 0))
    (forever
      (scan-for-targets token role spotted-gobs tick)
      (check-if-any-spotted-have-left token role spotted-gobs tick)
      (incf tick)
      (sleep 1))))
