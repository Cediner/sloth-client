(load "data/scripts/lib/_common_stockpile.lisp")
(defpackage :fish-bot
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config
        :stockpile-common))

(in-package :fish-bot)

(defconstant +fishing-rod+ "Bushcraft Fishingpole")
(defconstant +strings+ `("Spindly Taproot" "Flax Fibres" "Cattail Fibres" "Hide Strap"))
(defconstant +baits+ `("Entrails" "Chum Bait" "Earthworm" "Waterstrider"))
(defconstant +hooks+ `("Bone Hook" "Chitin Hook"))
(defconstant +fish+ `("Asp" "Brill" "Bream" "Burbot" "Catfish" "Carp" "Chub" "Eel" "Grayling" "Ide" 
						"Lavaret" "Perch" "Pike" "Plaice" "Roach" "Ruffe" "Salmon" "Smelt" "Sturgeon" 
						"Tench" "Trout" "Silver Bream" "Zander" "Zope" "Bass" "Cod" "Haddock" "Herring" 
						"Mackerel" "Mullet" "Pomfret" "Rose Fish" "Saithe" "Seahorse" "Whiting" 
						"Abyss Gazer" "Cavelacanth" "Cave Sculpin" "Pale Ghostfish"))


(defun equipment-get-by-equip-type (eqtype)
	(let ((equips (equipment-get-all-items))
		)
		(doarr (equip equips)
			(when (string= (equip-type-name eqtype) (equip-type-name (equip-type equip)))
				(return-from equipment-get-by-equip-type equip)
			)
		)
	)
)

(defun get-fishing-rod ()
	(let (
		  (rhand (equipment-get-by-equip-type (right-hand)))
		  (lhand (equipment-get-by-equip-type (left-hand)))
		 )
		(if (and rhand
				(string= +fishing-rod+ (item-name (equip-item rhand))))
			(return-from get-fishing-rod (equip-item rhand))
		)
		(if (and lhand
				(string= +fishing-rod+ (item-name (equip-item lhand))))
			(return-from get-fishing-rod (equip-item lhand))
		)
	)	
)

(defun check-or-equip (itemlist rod)
	(let(
		(contents (item-get-all-contents rod))
		(have-item nil)
		)
		(doarr (content contents)
			(if (member content itemlist :test 'equal)
				(setf have-item t)
			)
		)
		(when (not have-item)
			;;look in inventory for item and equip
			(progn
				(let (
						(items (inventories-get-items-by-filter (lambda (itm) (member (item-name itm) itemlist :test 'equal))))
					)
					(if (> (length items) 0)
						(progn 							
							(item-take (car items))
							(wait-until (lambda () (held-item)))
							(item-interact-with-held-item rod +mf-none+)
							(setf have-item t)
						)
					)
				)
			)
		)
		(return-from check-or-equip have-item)
	)	
)

(defun equip-fishing-rod (rod)
	(if (and (check-or-equip +strings+ rod)
			(check-or-equip +baits+ rod)
			(check-or-equip +hooks+ rod)
		)
		(return-from equip-fishing-rod t)
		(return-from equip-fishing-rod nil)
	)
)

(defun fish(coord)
	(menu-use "paginae/act/fish")
	(mv-click coord +left-button+ +mf-none+)
	(wait-for-movement)
	(wait-for-progress)
	(mv-click coord +right-button+ +mf-none+)
)

(defun get-not-main-inventory ()
	(let (
		(invs (inventories))
		)
		(doarr (inv invs)
			(when (not (string= (inventory-name inv) (inventory-name (main-inventory))))
				(return-from get-not-main-inventory inv)
			)
		)
	)
)

(defun get-supplies (container fishing-rod)
	(mv-click-gob container +right-button+ +mf-none+)
	(wait-for-movement)
	(sleep .1)
	(if (equip-fishing-rod fishing-rod)
		(progn
			(let (
					(bait-in-inventory (inventory-get-items-by-filter (main-inventory) (lambda (itm) (member (item-name itm) +baits+ :test 'equal))))
					(bait-not-in-inventory (inventory-get-items-by-filter (get-not-main-inventory) (lambda (itm) (member (item-name itm) +baits+ :test 'equal))))
				)
				(loop until (or (< (length bait-not-in-inventory) 1) (> (length bait-in-inventory) 4))
					do (progn
						(item-transfer (car bait-not-in-inventory))
						(sleep .1)
						(setf bait-in-inventory (inventory-get-items-by-filter (main-inventory) (lambda (itm) (member (item-name itm) +baits+ :test 'equal))))
						(setf bait-not-in-inventory (inventory-get-items-by-filter (get-not-main-inventory) (lambda (itm) (member (item-name itm) +baits+ :test 'equal))))
					)
				)
			)
		)
	)
)

(defun stockpile-fish (bbox)
	(let (
		(items (inventories-get-items-by-filter (lambda (itm) (member (item-name itm) +fish+ :test 'equal))))
		)
		(when (> (length items) 0)
			(store-items (car items) "gfx/terobjs/stockpile-fish" "Fish" (gob-rc (my-gob)) bbox (bbox-tiles bbox))
		)
	)
)

(script
	(let (
		  (material-container (prompt-for-selected-gob "Mark the container holding fishing materials"))
		  (fishing-coord (prompt-for-coord "Mark the fishing spot"))
		  (stockpile-area (get-bbox "Select the fish stockpile area"))
		  (fishing-rod (get-fishing-rod))
		)
		(if fishing-rod
			(forever	
				(stockpile-fish stockpile-area)
				(if (not (equip-fishing-rod fishing-rod))
					(progn 
						(get-supplies material-container fishing-rod)
					)
				)
				(fish fishing-coord)
				(sleep .5)
			)
			(chat-send-message (bot-chat) "NO FISHING ROD EQUIPPED")
		)
	)
)
