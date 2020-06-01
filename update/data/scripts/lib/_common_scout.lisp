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
  first-spotted-at
  avg-speed
  max-speed
  gear
  first-rc-at
  last-rc-at
  last-spotted
  last-alert
  tick)


(defun describe-gear (gear)
  (if gear
      (apply #'concatenate 'string
             (loop
              for equ in (listify gear)
              collect (format nil "~A~%" equ)))
      ""))

(defun scan-for-targets-custom-message (role spotted-gobs tick message)
  (let ((targets (gob-get-all-by-filter
                  (lambda (gob)
                    (or (member (gob-name gob) +targets+ :test 'equal)
                        (and (jeq (gob-type gob) +human+)
                             (not (is-gob-friendly gob))
                             (not (= (gob-id gob) (mv-plgob (mv))))))))))
    (loop
       for target in targets
       do (let ((name (if (jeq (gob-type target) +human+) (gob-kinname target) (gob-name target)))
                (gear (if (jeq (gob-type target) +human+) (gob-equipment target) nil)))
            (cond
              ((and (gethash (gob-id target) spotted-gobs)
                    (> (- (get-time) (spotted-gob-last-alert (gethash (gob-id target) spotted-gobs)))
                       +alert-refresh+))
               (let ((gob (gethash (gob-id target) spotted-gobs)))
                 (setf (spotted-gob-last-spotted gob) (gob-rc target))
                 (setf (spotted-gob-last-rc-at gob) (get-real-location-coord (gob-rc target)))
                 (setf (spotted-gob-avg-speed gob) (/ (+ (spotted-gob-avg-speed gob) (gob-v target)) 2))
                 (setf (spotted-gob-max-speed gob) (max (spotted-gob-avg-speed gob) (gob-v target)))
                 (setf (spotted-gob-tick gob) tick)
                 (setf (spotted-gob-last-alert gob) (get-time))
                 (send-discord-message "player-alerts" 
                                       (format nil "<@&~A> Still spotted a ~A [~A] [Avg Speed ~A] [Max Speed ~A] @ ~A"
                                               role
                                               name
                                               (gob-id target)
                                               (spotted-gob-avg-speed gob)
                                               (spotted-gob-max-speed gob)
                                               message))))
              ((gethash (gob-id target) spotted-gobs)
               (let ((gob (gethash (gob-id target) spotted-gobs)))
                 (setf (spotted-gob-last-spotted gob) (gob-rc target))
                 (setf (spotted-gob-last-rc-at gob) (get-real-location-coord (gob-rc target)))
                 (unless (equalp (spotted-gob-gear gob) gear)
                   (setf (spotted-gob-gear gob) gear)
                   (send-discord-message "player-alerts"
                                         (format nil "<@&~A> Gob ~A [~A] is now wearing ~A~%"
                                                 role
                                                 (spotted-gob-name gob)
                                                 (spotted-gob-id gob)
                                                 (describe-gear (spotted-gob-gear gob)))))
                 (setf (spotted-gob-avg-speed gob) (/ (+ (spotted-gob-avg-speed gob) (gob-v target)) 2))
                 (setf (spotted-gob-max-speed gob) (max (spotted-gob-avg-speed gob) (gob-v target)))
                 (setf (spotted-gob-tick gob) tick)))
              (t ;;new target
               (let ((gob (make-spotted-gob :id (gob-id target)
                                            :name name
                                            :avg-speed (gob-v target)
                                            :max-speed (gob-v target)
                                            :gear gear
                                            :first-spotted-at (get-time)
                                            :first-rc-at (get-real-location-coord (gob-rc target))
                                            :last-spotted (gob-rc target)
                                            :last-alert (get-time)
                                            :tick tick)))
                 (setf (gethash (spotted-gob-id gob) spotted-gobs) gob)
                 (send-discord-message "player-alerts"
                                       (format nil
                                               (if gear
                                                   "<@&~A> Spotted a ~A [~A] going ~A tiles/s @ ~A Wearing:~%~A"
                                                   "<@&~A> Spotted a ~A [~A] going ~A tiles/s @ ~A~A")
                                               role
                                               name
                                               (gob-id target)
                                               (spotted-gob-max-speed gob)
                                               message
                                               (describe-gear (spotted-gob-gear gob)))))))))))
											   
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
                 (setf (spotted-gob-avg-speed gob) (/ (+ (spotted-gob-avg-speed gob) (gob-v target)) 2))
                 (setf (spotted-gob-max-speed gob) (max (spotted-gob-avg-speed gob) (gob-v target)))
                 (setf (spotted-gob-tick gob) tick)
                 (setf (spotted-gob-last-alert gob) (get-time))
                 (send-discord-message "player-alerts" 
                                       (format nil "<@&~A> Still spotted a ~A [~A] [Avg Speed ~A] [Max Speed ~A] @ ~A"
                                               role
                                               name
                                               (gob-id target)
                                               (spotted-gob-avg-speed gob)
                                               (spotted-gob-max-speed gob)
                                               (get-real-location-on-map (gob-rc target))))))
              ((gethash (gob-id target) spotted-gobs)
               (let ((gob (gethash (gob-id target) spotted-gobs)))
                 (setf (spotted-gob-last-spotted gob) (gob-rc target))
                 (setf (spotted-gob-last-rc-at gob) (get-real-location-coord (gob-rc target)))
                 (setf (spotted-gob-avg-speed gob) (/ (+ (spotted-gob-avg-speed gob) (gob-v target)) 2))
                 (setf (spotted-gob-max-speed gob) (max (spotted-gob-avg-speed gob) (gob-v target)))
                 (setf (spotted-gob-tick gob) tick)))
              (t ;;new target
               (let ((gob (make-spotted-gob :id (gob-id target)
                                            :name name
                                            :avg-speed (gob-v target)
                                            :max-speed (gob-v target)
                                            :first-spotted-at (get-time)
                                            :first-rc-at (get-real-location-coord (gob-rc target))
                                            :last-spotted (gob-rc target)
                                            :last-alert (get-time)
                                            :tick tick)))
                 (setf (gethash (spotted-gob-id gob) spotted-gobs) gob)
                 (send-discord-message "player-alerts"
                                       (format nil
                                               (if extra
                                                   "<@&~A> Spotted a ~A [~A] going ~A tiles/s @ ~A Wearing:~%~A"
                                                   "<@&~A> Spotted a ~A [~A] going ~A tiles/s @ ~A~A")
                                               role
                                               name
                                               (gob-id target)
                                               (gob-v target)
                                               (get-real-location-on-map (gob-rc target))
                                               (if extra extra ""))))))))))

(defun describe-missing-gob (gob)
  (let* ((first-pos (spotted-gob-first-rc-at gob))
         (last-pos (spotted-gob-last-rc-at gob))
         (real-loc (get-real-location-on-map (spotted-gob-last-spotted gob))))
    (format nil "~A [~A] [Avg Speed ~A] [Max Speed ~A] [Duration ~A] @ ~A - ~A -> ~A"
            (spotted-gob-name gob)
            (spotted-gob-id gob)
            (spotted-gob-avg-speed gob)
            (spotted-gob-max-speed gob)
            (- (get-time) (spotted-gob-first-spotted-at gob))
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
