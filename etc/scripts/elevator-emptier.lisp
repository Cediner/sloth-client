(load "data/scripts/lib/_common_stockpile.lisp")
(defpackage :elevator-emptier
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config
        :stockpile-common))
(in-package :elevator-emptier)

(defconstant +elevator-inventory+ "Mine Elevator")

(defconstant +ores+ `("Cassiterite" "Chalcopyrite" "Cinnabar" "Malachite" "Peacock Ore" "Heavy Earth" "Iron Ochre" "Bloodstone" "Black Ore" "Galena" "Silvershine" "Horn Silver" "Direvein" "Schrifterz" "Leaf Ore"))

(script
	(let ((original-position (gob-rc (my-gob)))
		  (inv-gob (prompt-for-selected-gob "Select the elevator you'd like to have emptied"))
		  (stockpile-area (get-bbox "Select the area to stockpile stone")))
		(forever
			(mv-path-to-gob inv-gob)
			(mv-click-gob inv-gob +right-button+ +mf-none+)
			(wait-until (lambda () (inventory-get-by-name +elevator-inventory+)))
			(chat-send-message (bot-chat) "Elevator Inventory found")
			(let ((inv (inventory-get-by-name +elevator-inventory+)))
				(loop until (inventory-full (main-inventory))
					do (progn
						(when (> (inventory-used-slots inv) 0)
							(inventory-transfer-items inv (main-inventory) (inventory-used-slots inv))
						)
						(sleep .5)
					)
				)
			)
			(mv-path-to original-position)
			(let ((itm (inventory-get-items-by-filter
                    (main-inventory)
                    (lambda (itm)
                      (member (item-name itm) +ores+ :test 'equal)))))
				(when itm (store-items (car itm) "gfx/terobjs/stockpile-ore" "Ore" original-position stockpile-area (bbox-tiles stockpile-area))))		
			(mv-path-to original-position)
		)
	)
)