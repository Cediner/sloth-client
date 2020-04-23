(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Flower Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(java-func +session+ session-flowermenu "getFlowermenu")
(java-field flowermenu-options "opts")
(java-field flowermenu-petal-name "name")
(java-field flowermenu-petal-num "num")
(with-session-define flowermenu session-flowermenu)

(defmacro wait-for-flowermenu ()
  `(wait-until (lambda () (flowermenu)) :timeout 3000))

(defmacro flowermenu-select (ind)
  `(wdgmsg (flowermenu) "cl" ,ind))

(defun flowermenu-select-by-name (name)
  (doarr (petal (flowermenu-options (flowermenu)))
    (when (string= (flowermenu-petal-name petal) name)
      (flowermenu-select (flowermenu-petal-num petal))
      (return-from flowermenu-select-by-name t)))
  nil)
  
(defun flowermenu-get-petal-name (ind)
  (doarr (petal (flowermenu-options (flowermenu)))
    (when (= (flowermenu-petal-num petal) ind)
      (return-from flowermenu-get-petal-name (flowermenu-petal-name petal))))
  nil)

(export '(flowermenu wait-for-flowermenu flowermenu-select flowermenu-select-by-name flowermenu-get-petal-name))
