(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; OCache API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +ocache+ "haven.OCache")
(java-func +ocache+ oc-get-all-gobs-1 "getallgobs")
(java-func +ocache+ oc-get-gob-1 "getgob" +long+)
(defmacro oc-get-gob (id)
  `(oc-get-gob-1 (oc) ,id))
(defmacro oc-get-all-gobs ()
  `(oc-get-all-gobs-1 (oc)))
(defun oc-posres (mc)
  (coord2d-floor mc (svar +ocache+ "posres")))

(export '(oc-get-gob oc-get-all-gobs oc-posres))
