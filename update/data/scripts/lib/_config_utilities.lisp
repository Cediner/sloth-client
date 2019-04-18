(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Misc Utilities / Helpers / Predefined actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TODO: Refill empty barrels from well or nearby water - Long term, this is already overkill as is
(defun refill-water-from-hand ()
  (let ((gob (gob-get-closest-by-filter (lambda (gob) (gob-overlay-has gob "gfx/terobjs/barrel-water")))))
    (when gob
      (let ((original-c (gob-rc (my-gob)))
            (path (mv-find-path-to-gob gob)))
        (when path
          (mv-walk-path path)
          (mv-interact-held-item-with-gob gob +mf-none+)
          (wait-until (lambda () (item-get-contents (held-item))))
          (mv-walk-path (mv-reverse-path path original-c)))))))
      

(defun refill-water-from-inventory ()
  (let ((original-c (gob-rc (my-gob))))
    (loop
       for itm in (inventories-get-items-by-filter
                   (lambda (itm)
                     (and (search "Water" (item-name itm))
                          (null (item-get-contents itm)))))
       do (let ((pos (item-position itm))
                (inv (item-inventory itm))
                (gob (gob-get-closest-by-filter
                      (lambda (gob) (gob-overlay-has gob "gfx/terobjs/barrel-water")))))
            (if gob
                (progn
                  (mv-smart-move-to-gob gob)
                  (item-take itm)
                  (wait-until (lambda () (held-item)))
                  (mv-interact-held-item-with-gob gob +mf-none+)
                  (wait-until (lambda () (item-get-contents (held-item))))
                  (inventory-place-item inv pos))
                (progn
                  (mv-smart-move original-c)
                  (return-from refill-water-from-inventory)))))
    (mv-smart-move original-c)))

              ;; We can either drink from skins/flasks in inventory/belt or via item on mouse
;; item on mouse needs to be done via hotkey since loftar killed off flowermenus
;; while holding objects. This will always use +hotkey-1+ for that
;; ideally if the item on mouse runs out of water it should check for a water barrel
;; and try to refill and repeat
;;TODO: Handle item in equipment slot for hand with water - Not usually the case, but should handle it
(defun drink-water (&optional (refill t))
  (if (and (held-item)
           (or (is-item-contents (held-item) "Water")
               (string= "Bucket" (item-name (held-item)))))
      ;;holding a bucket or something on the mouse, try to use it or refill if needed
      (progn
        (hk-unset-item +hotkey-1+)
        (wait-until-hotkey-is-unset +hotkey-1+)
        (hk-set-item +hotkey-1+)
        (wait-until-hotkey-is-set +hotkey-1+)
        (hk-use-item +hotkey-1+ +mf-none+)
        (wait-for-progress)
        (when (and refill
                   (< (stamina) 100))
          (refill-water-from-hand)))
      ;;Not holding anything of value on cursor
      (progn
        (when (held-item)
          (item-drop (held-item)))
        (wait-until (lambda () (null (held-item))))
        
        (loop
           for itm in (inventories-get-items-by-filter
                       (lambda (itm)
                         (let ((contents (item-get-contents itm)))
                           (and contents
                                (jeq (liquid-type) (contents-type contents))))))
           do (progn
                (item-interact itm +mf-none+)
                (wait-until (lambda () (flowermenu)))
                (flowermenu-select-by-name "Drink")
                (wait-for-progress)
                (when (= (stamina) 100)
                  (return-from drink-water))))
        (when (and refill
                   (< (stamina) 100))
          (refill-water-from-inventory)))))
       
(defun check-stam-and-drink (&key (drink-at 40) (refill t))
  (if (< (stamina) drink-at)
      (progn
        (drink-water refill)
        t)
      nil))


(defun check-for-starving (&optional (starve-at 2000))
  (if (< (energy) starve-at)
      (progn
        (misc-log "Logging out in order to not starve the character...")
        (goto-character-screen)
        t)
      nil))

(defun get-bbox (msg)
  (msg-listen)
  (chat-send-message (bot-chat) msg)
  (bbox-trigger)
  (let ((bbox nil))
    (loop
       until bbox
       do (progn
            (sleep 1)
            (when (msg-has-message)
              (let ((msg (msg-poll-message)))
                (when (string= "bot-select" (msg-subject msg))
                  (setf bbox (bbox-make (aref (msg-args msg) 0)
                                        (aref (msg-args msg) 1))))))))
    (msg-stop-listening)
    (msg-clear-messages)
    bbox))

(defun check-for-movement (gob &key (wait-for 5))
  (let ((start-c (gob-rc gob)))
    (sleep wait-for)
    (if (coord2d-eq (gob-rc gob) start-c)
        nil
        t)))

(defun backoff-randomly ()
  (mv-move-to-rel (coord2d (+ (random 11) 3)
                           (+ (random 11) 3)))
  (wait-for-movement :gob (my-gob)))

(defun prompt-for-input (prompt)
  (msg-listen)
  (chat-send-message (bot-chat) prompt)
  (let ((ret nil))
    (loop
       until ret 
       do (progn
            (loop
               while (and (msg-has-message) (null ret))
               do (when (msg-has-message)
                    (let ((msg (msg-poll-message)))
                      (when (and (string= "msg" (msg-subject msg))
                                 (jeq (bot-chat) (msg-sender msg)))
                        (setf ret (aref (msg-args msg) 0))))))
            (sleep 1)))
    (msg-stop-listening)
    (msg-clear-messages)
    ret))

(defun prompt-for-selected-gob (prompt)
  (msg-listen)
  (chat-send-message (bot-chat) prompt)
  (let ((gob nil))
    (loop
       until gob
       do (progn
            (sleep 1)
            (loop
               while (and (msg-has-message) (null gob))
               do (let ((msg (msg-poll-message)))
                    (when (string= "click-gob" (msg-subject msg))
                      (setf gob (aref (msg-args msg) 0)))))))
    (msg-stop-listening)
    (msg-clear-messages)
    gob))

(export '(check-stam-and-drink drink-water refill-water-from-hand refill-water-from-inventory
          check-for-starving
          check-for-movement
          backoff-randomly
          prompt-for-input prompt-for-selected-gob
          get-bbox))
