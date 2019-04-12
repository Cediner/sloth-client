(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Widget/UI API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +widget+ "haven.Widget")
(defconstant +ui+ "haven.UI")
(java-func +widget+ wdgmsg-1 "wdgmsg" +string+ +object-array+)
(java-func +widget+ uimsg-1 "uimsg" +string+ +object-array+)
(defun wdgmsg (wdg msg &rest args)
  (wdgmsg-1 wdg msg (jnew-array-from-list "java.lang.Object" args)))

(defun uimsg (wdg msg &rest args)
  (uimsg-1 wdg msg (jnew-array-from-list "java.lang.Object" args)))

(java-field widget-parent "parent")
(java-field widget-c "c")
(java-func +widget+ widget-id "wdgid")
(java-func +widget+ widget-add "add" +widget+ +coord+)

(java-field ui-next-id-1 "next_predicted_id")
(java-func +ui+ ui-force-wdgmsg-1 "wdgmsg" +int+ +string+ +object-array+)
(defmacro ui-next-id ()
  `(ui-next-id-1 (ui)))
(defun ui-force-wdgmsg (id msg &rest args)
  (ui-force-wdgmsg-1 (ui) id msg (jnew-array-from-list "java.lang.Object" args)))

(export '(wdgmsg
          widget-add widget-c widget-id widget-parent
          ui-next-id ui-force-wdgmsg))
