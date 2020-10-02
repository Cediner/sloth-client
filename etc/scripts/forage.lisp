(defpackage :forage
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :forage)

(defconstant +map-reload-time+ (* 21 60))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Loading/Creating new data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-data (file)
  (with-open-file (fd file :direction :input)
    (let ((radius (read-from-string (read-line fd)))
          (use-boat (read-from-string (read-line fd)))
          (crawl (read-from-string (read-line fd)))
          (logout-on-players (read-from-string (read-line fd)))
          (logout-on-animals (read-from-string (read-line fd)))
          (foragables (read-from-string (read-line fd)))
          (points (read-from-string (read-line fd)))
          (rpoints (read-from-string (read-line fd))))
      (values radius use-boat crawl logout-on-players logout-on-animals
              foragables points rpoints))))

(defun normalize-points (points)
  (let ((pts ()))
    (doarr (pt points)
      (setf pts (append pts `((,(coord2d-x pt) . ,(coord2d-y pt))))))
    pts))

(defun save-data (file radius use-boat crawl logout-on-players logout-on-animals foragables points rpoints)
  (ensure-directories-exist "data/scripts/forage/")
  (with-open-file (fd file
                      :direction :output
                      :if-exists :supersede)
    (format fd "~A~%" radius)
    (format fd "~A~%" use-boat)
    (format fd "~A~%" crawl)
    (format fd "~A~%" logout-on-players)
    (format fd "~A~%" logout-on-animals)
    (format fd "~S~%" foragables)
    (format fd "~A~%" points)
    (format fd "~A~%" rpoints)))

(defun get-data ()
  (msg-listen)
  (widget-add (gui) (jnew "haven.sloth.gui.ForageWizardWnd") (coord 50 50))
  (let ((done nil)
        (new nil)
        (radius 45)
        (use-boat nil)
        (crawl nil)
        (logout-on-players nil)
        (logout-on-animals nil)
        (foragables ())
        (points ())
        (rpoints ()))
    (loop
       until done
       do (progn
            (sleep 1)
            (loop
               while (and (msg-has-message) (not done))
               do (let ((msg (msg-poll-message)))
                    (cond
                      ((string= "cancel" (msg-subject msg))
                       (setf done t))
                      ((string= "load-data" (msg-subject msg))
                       (setf done t)
                       (multiple-value-bind (r b c lp la f p rp)
                           (load-data (aref (msg-args msg) 0))
                         (setf radius r
                               use-boat b
                               crawl c
                               logout-on-players lp
                               logout-on-animals la
                               foragables f
                               points p
                               rpoints rp)))
                      ((string= "new-data" (msg-subject msg))
                       (progn
                         (setf done t)
                         (setf new t)
                         (setf radius (aref (msg-args msg) 1))
                         (setf use-boat (aref (msg-args msg) 2))
                         (setf crawl (aref (msg-args msg) 3))
                         (setf logout-on-players (aref (msg-args msg) 4))
                         (setf logout-on-animals (aref (msg-args msg) 5))
                         (setf foragables (listify (aref (msg-args msg) 6)))
                         (setf points (normalize-points (aref (msg-args msg) 7)))
                         (setf rpoints (normalize-points (aref (msg-args msg) 8)))
                         (save-data (aref (msg-args msg) 0) radius use-boat crawl
                                    logout-on-players logout-on-animals
                                    foragables points rpoints))))))))
    (msg-stop-listening)
    (msg-clear-messages)
    (values new radius use-boat crawl
            logout-on-players logout-on-animals
            foragables points rpoints)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Forage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun logout-and-wait (crawl)
  (let ((name (character-name)))
    (goto-character-screen)
    (sleep +map-reload-time+)
    (login name)
    (wait-until (lambda () (speed)))
    (when crawl
      (speed-set +crawl+))))

(defun check-if-full ()
  (if (inventory-full (main-inventory))
      ;;Check for nearby containers to offload to
      (if (inventory-full (main-inventory))
          (progn
            ;;If we're still full log out
            (goto-character-screen)
            t)
          nil)
      nil))

(defun get-foragable (gob)
  (let ((attempt 0))
    ;;Attempt to get this foragable up to 3 times 
    (loop
       while (< attempt 3)
       do (progn
            ;;path to it
            (mv-path-to (gob-rc gob))
            ;;wait until we hit it
            (if (< (coord2d-dist (gob-rc gob) (gob-rc (my-gob))) 11)
                (progn
                  (mv-click-gob gob +right-button+ +mf-none+)
                  (wait-for-flowermenu)
                  (flowermenu-select 0)
                  ;;If somehow we still didn't get it just try again if possible
                  (wait-until (lambda () (null (oc-get-gob (gob-id gob)))) :timeout 5000)
                  (if (null (oc-get-gob (gob-id gob)))
                      (setf attempt 3)
                      (incf attempt)))
                ;;Attempt again if we didn't path fully to it
                (progn
                  (backoff-randomly)
                  (incf attempt)))))
    ;;put this onto the ignorelist if we couldn't get it
    (if (null (oc-get-gob (gob-id gob)))
        t
        nil)))  

(defun search (radius foragables ignore-list)
  (let ((gobs (gob-get-all-by-filter
               (lambda (gob)
                 (and (member (gob-name gob) foragables :test 'string=)
                      (not (member (gob-id gob) ignore-list))
                      (<= (coord2d-dist (gob-rc (my-gob)) (gob-rc gob)) (* radius 11)))))))
    (when gobs
      (mv-move-to (gob-rc (my-gob)))
      (wait-for-movement)
      (let ((startc (gob-rc (my-gob))))
        (dolist (gob gobs)
          (when (not (get-foragable gob))
            (push (gob-id gob) ignore-list))
          ;;Go back to our starting coordinate before going after another foragable
          (loop
             until (< (coord2d-dist startc (gob-rc (my-gob))) 11)
             do (progn
                  (backoff-randomly)
                  (mv-path-to startc)))))))
  ignore-list)

(defun forage-path (radius use-boat crawl logout-on-players logout-on-animals foragables points)
  (let ((ignore-list ()))
    (dolist (point points)
      (let ((destination (coord2d-add (gob-rc (my-gob)) (coord2d (car point) (cdr point)))))
        (loop
           while (not (coord2d-eq (gob-rc (my-gob)) destination))
           do (progn
                ;;Make sure we have inventory room
                (when (check-if-full)
                  (return-from forage-path nil))
                ;;Check for unknown people or bad animals
                (when (gob-get-all-by-filter
                       (lambda (gob)
                         (or (and logout-on-players
                                  (jeq (gob-type gob) +human+)
                                  (not (is-gob-friendly gob))
                                  (not (jeq gob (my-gob))))
                             (and logout-on-animals
                                  (jeq (gob-type gob) +animal+)
                                  (is-gob-dangerous gob)))))
                  (mv-move-to (gob-rc (my-gob)))
                  (wait-for-movement)
                  (let ((delta (coord2d-sub destination (gob-rc (my-gob)))))
                    (logout-and-wait crawl)
                    (setf destination (coord2d-add (gob-rc (my-gob)) delta))
                    (setf ignore-list ())))
                ;;search for foragables
                (setf ignore-list (search radius foragables ignore-list))
                ;;make sure we keep moving to the destination
                (when (not (is-gob-moving (my-gob)))
                  (mv-move-to destination))
                (sleep 1))))))
  t)

(defun forage (new-path radius use-boat crawl
               logout-on-players logout-on-animals
               foragables points rpoints)
  ;;walk back to start if new path
  (when new-path
    (when (not (forage-path radius use-boat crawl logout-on-players logout-on-animals foragables rpoints))
      (return-from forage))
    (logout-and-wait crawl))
  ;;Run the forage path
  (forever
   ;;forward pass
   (when (not (forage-path radius use-boat crawl logout-on-players logout-on-animals foragables points))
     (return-from forage))
   (logout-and-wait crawl)
   ;;reverse pass
   (when (not (forage-path radius use-boat crawl logout-on-players logout-on-animals foragables rpoints))
     (return-from forage))
   (logout-and-wait crawl)))
  
(script
 (multiple-value-bind (new-path radius use-boat logout-on-players logout-on-animals crawl foragables points rpoints)
     (get-data)
   (when points
     (forage new-path radius use-boat crawl
             logout-on-players logout-on-animals
             foragables points rpoints))))
