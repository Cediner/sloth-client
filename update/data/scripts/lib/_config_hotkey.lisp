;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Hotkeys
;;;; Need to be careful here as you'll overwrite normal hotkeys
;;;; There's 144 hotkeys total -> Try to only use the last 4 as the normal player
;;;; belts use 0 -> 140
;;;; You should only use +hotkey-1+ -> +hotkey-4+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :hafen-config)

(defconstant +hotkey-1+ 141)
(defconstant +hotkey-2+ 142)
(defconstant +hotkey-3+ 143)
(java-field hk-items "belt")

(defmacro hk-use-item (slot flags)
  `(wdgmsg (gui) "belt" ,slot 1 ,flags))
(defmacro hk-set-item (slot)
  `(wdgmsg (gui) "setbelt" ,slot 0))
(defmacro hk-unset-item (slot)
  `(wdgmsg (gui) "setbelt" ,slot 1))

(defmacro is-hotkey-set (slot)
  `(not (null (aref (hk-items (gui)) ,slot))))

(defmacro wait-until-hotkey-is-set (slot)
  `(wait-until (lambda () (is-hotkey-set ,slot))))

(defmacro wait-until-hotkey-is-unset (slot)
  `(wait-until (lambda () (not (is-hotkey-set ,slot)))))


(export '(+hotkey-1+ +hotkey-2+ +hotkey-3+
          hk-use-item hk-set-item hk-unset-item
          is-hotkey-set
          wait-until-hotkey-is-set wait-until-hotkey-is-unset))
