(defpackage :trough-filler
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :trough-filler)

(defconstant +fodder-gob-names+ (list "gfx/terobjs/items/carrot" "gfx/terobjs/items/beetleaves" "gfx/terobjs/items/beet" "gfx/terobjs/items/pumpkin" "gfx/terobjs/items/pumpkinflesh"))
(defconstant +fodder-item-names+ (list "Carrot" "Beetroot Leaves" "Beet" "Pumpkin" "Pumpkin Flesh"))
(defconstant +trough-gob-name+ "gfx/terobjs/trough")

(defun inventory-get-item-by-filter (inv filter-fun)
  (doarr (itm (inventory-items inv))
    (when (funcall filter-fun itm)
      (return-from inventory-get-item-by-filter itm)))
  nil)

(defun prompt-for-selected-tile (prompt)
  (msg-listen)
  (chat-send-message (bot-chat) prompt)
  (let ((tile nil))
    (loop
       until tile
       do (progn
            (sleep 1)
            (loop
               while (and (msg-has-message) (null tile))
               do (let ((msg (msg-poll-message)))
                    (when (string= "click-tile" (msg-subject msg))
                      (setf tile (aref (msg-args msg) 0)))))))
    (msg-stop-listening)
    (msg-clear-messages)
    tile))

(defun travel-to-tile (tile)
	(loop
		until (< (coord2d-dist tile (gob-rc (my-gob))) 5)
		do (mv-smart-move tile))
)

(defun pickup-fodder (tile bbox)
	(travel-to-tile tile)
	(loop 
		for gob = (gob-get-closest-by-filter (lambda (gob) 
					(and (bbox-within bbox (gob-rc gob))
						 (member (gob-name gob) +fodder-gob-names+ :test #'string=))))
		until (or (held-item) (null gob))
		do (progn
			(mv-smart-move-to-gob gob)
			(mv-click-gob gob +right-button+ +mf-shift+)
			(wait-for-movement)
		)
	)
	(travel-to-tile tile)
)

(defun pickup-item ()
	(when (null (held-item))
		(progn
			(let ((item (inventory-get-item-by-filter (main-inventory) (lambda (item) 
														(member (item-name item) +fodder-item-names+ :test #'string=))))
				 )
				(when item
					(item-take item)
					(wait-until (lambda() (held-item)))
				)
			)
		)
	)
)

(defun fill-troughs (tile bbox)
	(travel-to-tile tile)
	(loop
		for gob = (gob-get-closest-by-filter (lambda (gob) 
					(and (bbox-within bbox (gob-rc gob))
						 (< (gob-sdt gob) 31)
						 (string= (gob-name gob) +trough-gob-name+))))
		until (or (null gob) (null (held-item)))
		do (progn	
			(mv-smart-move-to-gob gob)
			(mv-interact-held-item-with-gob gob +mf-shift+)
			(sleep.1)
			(pickup-item)
		)
	)
	(travel-to-tile tile)
)
	
(script
	(let ((trough-tile (prompt-for-selected-tile "Select a tile in render of the troughs"))
		  (trough-area (get-bbox "Select the area the troughs are in"))
		  (fodder-tile (prompt-for-selected-tile "Select a tile in render of the fodder"))
		  (fodder-area (get-bbox "Select the area the fodder items are in"))
		  )
		(forever
			(pickup-fodder fodder-tile fodder-area)
			(pickup-item)
			(when (held-item)
			(fill-troughs trough-tile trough-area))
		)
	)
)