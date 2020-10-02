(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Pointers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +pointers+ "haven.sloth.script.PointerData")
(java-func +session+ session-pointers "getPointers")
(with-session-define pointers-1 session-pointers)
(defmacro pointers ()
  `(listify (pointers-1)))

(java-func +pointers+ pointer-name "name")
(java-func +pointers+ pointer-c "c")

(export '(pointers
          pointer-name pointer-c))
