(in-package :hafen-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Everything Combat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +fightview+ "haven.Fightview")
(defconstant +fightsess+ "haven.Fightsess")
(defconstant +card+ "haven.sloth.gui.fight.Card")
(defconstant +attack+ "haven.sloth.gui.fight.Attack")
(defconstant +maneuver+ "haven.sloth.gui.fight.Maneuver")
(defconstant +restoration+ "haven.sloth.gui.fight.Restoration")
(java-field gui-fv "fv")
(java-field gui-fs "fs")
(defun fv ()
  (gui-fv (ui-gui (ui))))
(defun fs ()
  (gui-fs (ui-gui (ui))))

(java-func +fightsess+ fs-actions "actions")
(defun actions ()
  (let ((ret ()))
    (doarr (act (fs-actions (fs)))
      (when act
        (push act ret)))
    ret))

(java-field action-id "id")
(java-field action-card "card")
(java-field action-number-of-cards "cards")
(java-func "haven.Fightsess$Action" action-is-discovered "isDiscovered")
(defmacro fight-use-action (action-id)
  `(wdgmsg (fs) "use" ,action-id 1 +mf-none+))

(java-field card-name "name")
;;Argument -> # of cards for this card
(java-func +card+ card-cooldown "cooldown" +int+)
(defun attackp (card)
  (jinstance-of-p card +attack+))
(defun maneuverp (card)
  (jinstance-of-p card +maneuver+))
(defun restorationp (card)
  (jinstance-of-p card +restoration+))
