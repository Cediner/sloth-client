(defpackage :scout-common
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :scout-common)


(defconstant +alert-refresh+ (* 3 60 1000))
(defconstant +targets+
  `("gfx/terobjs/vehicle/knarr"
    "gfx/terobjs/vehicle/snekkja"
    "gfx/terobjs/vehicle/rowboat"))


(defstruct spotted-gob
  id
  name
  first-rc-at
  last-rc-at
  last-spotted
  last-alert
  tick)

(defun scan-for-targets-custom-message (role spotted-gobs tick message)
  (let ((targets (gob-get-all-by-filter
                  (lambda (gob)
                    (or (member (gob-name gob) +targets+ :test 'equal)
                        (and (jeq (gob-type gob) +human+)
                             (not (is-gob-friendly gob))
                             (gob-equipment gob)
                             (not (jeq gob (my-gob)))))))))
    (loop
       for target in targets
       do (let ((name (if (jeq (gob-type target) +human+) (gob-kinname target) (gob-name target)))
                (extra (if (jeq (gob-type target) +human+)
                           (let ((equs (gob-equipment target)))
                             (apply #'concatenate 'string
                                    (loop
                                       for equ in (listify equs)
                                       collect (format nil "~A~%" equ))))
                             nil)))
            (cond
              ((and (gethash (gob-id target) spotted-gobs)
                    (> (- (get-time) (spotted-gob-last-alert (gethash (gob-id target) spotted-gobs)))
                       +alert-refresh+))
               (let ((gob (gethash (gob-id target) spotted-gobs)))
                 (setf (spotted-gob-last-spotted gob) (gob-rc target))
                 (setf (spotted-gob-last-rc-at gob) (get-real-location-coord (gob-rc target)))
                 (setf (spotted-gob-tick gob) tick)
                 (setf (spotted-gob-last-alert gob) (get-time))
                 (send-discord-message "player-alerts" 
                                       (format nil "<@&~A> Still spotted a ~A [~A] @ ~A"
                                               role
                                               name
                                               (gob-id target)
                                               message))))
              ((gethash (gob-id target) spotted-gobs)
               (let ((gob (gethash (gob-id target) spotted-gobs)))
                 (setf (spotted-gob-last-spotted gob) (gob-rc target))
                 (setf (spotted-gob-last-rc-at gob) (get-real-location-coord (gob-rc target)))
                 (setf (spotted-gob-tick gob) tick)))
              (t ;;new target
               (let ((gob (make-spotted-gob :id (gob-id target)
                                            :name name
                                            :first-rc-at (get-real-location-coord (gob-rc target))
                                            :last-spotted (gob-rc target)
                                            :last-alert (get-time)
                                            :tick tick)))
                 (setf (gethash (spotted-gob-id gob) spotted-gobs) gob)
                 (send-discord-message "player-alerts"
                                       (format nil
                                               (if extra
                                                   "<@&~A> Spotted a ~A [~A] @ ~A Wearing:~%~A"
                                                   "<@&~A> Spotted a ~A [~A] @ ~A~A")
                                               role
                                               name
                                               (gob-id target)
                                               message
                                               (if extra extra ""))))))))))
											   
(defun scan-for-targets (role spotted-gobs tick)
  (let ((targets (gob-get-all-by-filter
                  (lambda (gob)
                    (or (member (gob-name gob) +targets+ :test 'equal)
                        (and (jeq (gob-type gob) +human+)
                             (not (is-gob-friendly gob))
                             (gob-equipment gob)
                             (not (jeq gob (my-gob)))))))))
    (loop
       for target in targets
       do (let ((name (if (jeq (gob-type target) +human+) (gob-kinname target) (gob-name target)))
                (extra (if (jeq (gob-type target) +human+)
                           (let ((equs (gob-equipment target)))
                             (apply #'concatenate 'string
                                    (loop
                                       for equ in (listify equs)
                                       collect (format nil "~A~%" equ))))
                             nil)))
            (cond
              ((and (gethash (gob-id target) spotted-gobs)
                    (> (- (get-time) (spotted-gob-last-alert (gethash (gob-id target) spotted-gobs)))
                       +alert-refresh+))
               (let ((gob (gethash (gob-id target) spotted-gobs)))
                 (setf (spotted-gob-last-spotted gob) (gob-rc target))
                 (setf (spotted-gob-last-rc-at gob) (get-real-location-coord (gob-rc target)))
                 (setf (spotted-gob-tick gob) tick)
                 (setf (spotted-gob-last-alert gob) (get-time))
                 (send-discord-message "player-alerts" 
                                       (format nil "<@&~A> Still spotted a ~A [~A] @ ~A"
                                               role
                                               name
                                               (gob-id target)
                                               (get-real-location-on-map (gob-rc target))))))
              ((gethash (gob-id target) spotted-gobs)
               (let ((gob (gethash (gob-id target) spotted-gobs)))
                 (setf (spotted-gob-last-spotted gob) (gob-rc target))
                 (setf (spotted-gob-last-rc-at gob) (get-real-location-coord (gob-rc target)))
                 (setf (spotted-gob-tick gob) tick)))
              (t ;;new target
               (let ((gob (make-spotted-gob :id (gob-id target)
                                            :name name
                                            :first-rc-at (get-real-location-coord (gob-rc target))
                                            :last-spotted (gob-rc target)
                                            :last-alert (get-time)
                                            :tick tick)))
                 (setf (gethash (spotted-gob-id gob) spotted-gobs) gob)
                 (send-discord-message "player-alerts"
                                       (format nil
                                               (if extra
                                                   "<@&~A> Spotted a ~A [~A] @ ~A Wearing:~%~A"
                                                   "<@&~A> Spotted a ~A [~A] @ ~A~A")
                                               role
                                               name
                                               (gob-id target)
                                               (get-real-location-on-map (gob-rc target))
                                               (if extra extra ""))))))))))

(defun describe-missing-gob (gob)
  (let* ((first-pos (spotted-gob-first-rc-at gob))
         (last-pos (spotted-gob-last-rc-at gob))
         (real-loc (get-real-location-on-map (spotted-gob-last-spotted gob))))
    (format nil "~A [~A] @ ~A - ~A -> ~A"
            (spotted-gob-name gob)
            (spotted-gob-id gob)
            real-loc
            (if first-pos (coord2d-string first-pos) "Unknown")
            (if last-pos (coord2d-string last-pos) "immediately left"))))
    
(defun check-if-any-spotted-have-left (role spotted-gobs tick)
  (loop
     for id being the hash-keys in spotted-gobs
     when (/= tick (spotted-gob-tick (gethash id spotted-gobs)))
     do (let ((gob (gethash id spotted-gobs)))
          (remhash id spotted-gobs)
          (let ((msg (describe-missing-gob gob)))
            (send-discord-message "player-alerts"
                                  (format nil "<@&~A> Last spotted a ~A"
                                          role
                                          msg))))))
(export '(scan-for-targets
          scan-for-targets-custom-message
          describe-missing-gob
          spotted-gob
          check-if-any-spotted-have-left))
