(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Item
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +item+ "haven.GItem")
(defconstant +contents+ "haven.sloth.gui.item.ContentData")
(defconstant +content-type+ "haven.sloth.io.ItemData$ContainerType")
(defconstant +item-sz+ (coord 33 33))
(defmacro liquid-type ()
  `(svar +content-type+ "LIQUID"))
(defmacro weight-type ()
  `(svar +content-type+ "WEIGHT"))
(defmacro seed-type ()
  `(svar +content-type+ "SEED"))

(java-func +session+ session-held-item "getHeldItem")
(java-func +item+ item-name "rnm")
(java-func +item+ item-is-food "isFood")
(java-func +item+ item-get-contents "hasContents")
(java-func +item+ item-get-all-contents "getRawContents")
(java-func +item+ item-witem "witem")
(java-field item-quality "quality")
(java-field item-amount "num")
(java-field item-meter "meter")
(java-field contents-name "name")
(java-field contents-type "type")
(java-field contents-current "current")
(java-field contents-max "max")

(defun is-item-contents (itm name)
  (let ((contents (item-get-contents itm)))
    (if (and contents
             (string= name (contents-name contents)))
        contents
        nil)))

(with-session-define held-item session-held-item)

(defmacro item-position (itm)
  `(coord-div (widget-c (item-witem ,itm)) +item-sz+))

(defmacro item-size (itm)
  `(coord-div (widget-sz (item-witem ,itm)) +item-sz+))

(defmacro item-inventory (itm)
  `(widget-parent (item-witem ,itm)))

(defmacro item-transfer (itm)
  `(wdgmsg ,itm "transfer" (fakec) 1))
(defmacro item-transfer-all-alike (itm)
  `(wdgmsg ,itm "transfer" (fakec) -1))
(defmacro item-drop (itm)
  `(wdgmsg ,itm "drop" (fakec)))
(defmacro item-take (itm)
  `(wdgmsg ,itm "take" (fakec)))
;;ie: right clicking flask to drink
(defmacro item-interact (itm mouse-flags)
  `(wdgmsg ,itm "iact" (fakec) ,mouse-flags))
;;This is right clicking an item while holding another
(defmacro item-interact-with-held-item (itm mouse-flags)
  `(wdgmsg ,itm "itemact" ,mouse-flags))

(export '(liquid-type weight-type seed-type
          item-position item-size item-inventory
          item-name item-is-food item-get-contents item-witem
          item-get-all-contents
          item-quality item-amount item-meter
          contents-name contents-type contents-current contents-max
          is-item-contents
          held-item
          item-transfer item-transfer-all-alike
          item-drop
          item-take
          item-interact item-interact-with-held-item))
