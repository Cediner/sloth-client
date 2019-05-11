(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Inventory
;;;; Inventories are grids (column, row)
;;;; column and row both start at 0, 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +inv+ "haven.Inventory")
(java-func +session+ session-main-inventory "getMainInventory")
(java-func +session+ session-study-inventory "getStudyInventory")
(java-func +session+ session-belt-inventory "getBeltInventory")
(java-func +session+ session-number-of-inventories "numberOfInventories")
(java-func +session+ session-get-inventory "getInventory" +int+)
(java-func +session+ session-get-inventories "inventories")

(with-session-define main-inventory session-main-inventory)
(with-session-define study-inventory session-study-inventory)
(with-session-define belt-inventory session-belt-inventory)
(with-session-define inventory-count session-number-of-inventories)
(with-session-define inventory session-get-inventory id)
(with-session-define inventories session-get-inventories)

(java-func +inv+ inventory-name "name")
(java-func +inv+ inventory-items "items")
(java-func +inv+ inventory-size "totalSlots")
(java-func +inv+ inventory-used-slots "usedSlots")
;;(java-func +inv+ inventory-can-drop-at "canDropAt" +coord+)
;;(java-func +inv+ inventory-item-at "itemAt" +coord+)

(defun inventory-can-drop-at (inv coord)
  (null (inventory-item-at inv coord)))

(defun inventory-item-at (inv coord)
  (let ((found (loop
                  for itm in (listify (inventory-items inv))
                  when (coord-between coord (item-position itm) (item-size itm))
                  return itm)))
    (if found
        found
        nil)))

(defmacro inventory-free-slots (inv)
  `(- (inventory-size ,inv) (inventory-used-slots ,inv)))

(defmacro inventory-full (inv)
  `(zerop (- (inventory-size ,inv) (inventory-used-slots ,inv))))

(defmacro inventory-place-item (inv position)
  `(wdgmsg ,inv "drop" ,position))

(defmacro inventory-transfer-items (inv-from inv-to amount)
  `(wdgmsg ,inv-from "invxf" (widget-id ,inv-to) ,amount))

(defun inventory-get-item-by-name (inv name)
  (doarr (itm (inventory-items inv))
    (when (string= name (item-name itm))
      (return-from inventory-get-item-by-name itm)))
  nil)

(defun inventory-get-items-by-name (inv name)
  (let ((ret ()))
    (doarr (itm (inventory-items inv))
      (when (string= name (item-name itm))
        (push itm ret)))
    ret))
  
(defun inventory-get-items-by-filter (inv filter-fun)
  (let ((ret ()))
    (doarr (itm (inventory-items inv))
      (when (funcall filter-fun itm)
        (push itm ret)))
    ret))

(defun inventory-drop-all-items-by-name (inv name)
  (doarr (itm (inventory-items inv))
    (when (string= name (item-name itm))
      (item-drop itm)
      (sleep 0.05))))

(defun inventory-drop-all-items-alike (inv item)
  (when (string/= (item-name item) "")
    (inventory-drop-all-items-by-name inv (item-name item))))

(defun inventories-drop-all-items-by-name (name)
  (doarr (inv (inventories))
    (inventory-drop-all-items-by-name inv name)))

(defun inventories-drop-all-items-alike (item)
  (inventories-drop-all-items-by-name (item-name item)))

(defun inventories-get-items-by-filter (filter-fun)
  (let ((ret ()))
    (doarr (inv (inventories))
      (doarr (itm (inventory-items inv))
        (when (funcall filter-fun itm)
          (push itm ret))))
    ret))

(export '(main-inventory study-inventory belt-inventory
          inventory-count inventory inventories
          inventory-name inventory-items inventory-size inventory-used-slots inventory-can-drop-at inventory-item-at
          inventory-free-slots inventory-full inventory-place-item
          inventory-transfer-items
          inventory-get-item-by-name inventory-get-items-by-name inventory-get-items-by-filter
          inventory-drop-all-items-by-name inventory-drop-all-items-alike
          inventories-drop-all-items-by-name inventories-drop-all-items-alike inventories-get-items-by-filter))
