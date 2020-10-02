(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Charlist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(java-func +session+ session-charlist "getCharlist")
(with-session-define charlist session-charlist)

(defmacro login (character-name)
  `(wdgmsg (charlist) "play" ,character-name))

(defmacro goto-character-screen ()
  `(wdgmsg (gui) "act" "lo" "cs"))

(export '(charlist login goto-character-screen))
