(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MenuGrid
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +menu+ "haven.MenuGrid")
(java-field gui-menu "menu")
(defmacro menu ()
  `(gui-menu (gui)))
(java-func +menu+ mg-use-1 "use" +string+)
(defmacro menu-use (res)
  `(mg-use-1 (menu) ,res))

(export '(menu menu-use))
