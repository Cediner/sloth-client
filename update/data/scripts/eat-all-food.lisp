(defpackage :eat-all-food
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))

(in-package :eat-all-food)

(script
 (let ((itms (inventories-get-items-by-filter #'item-is-food))
       (id (ui-next-id)))
   (loop
      for itm in itms
      do (progn
           (item-interact itm +mf-none+)
           (ui-force-wdgmsg id "cl" 0 +mf-none+)
           (incf id)))))
