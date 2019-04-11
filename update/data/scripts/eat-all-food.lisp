(defpackage :eat-all-food
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))

(in-package :eat-all-food)

(script
 (let ((itms (inventories-get-items-by-filter #'item-is-food)))
   (loop
      for itm in itms
      do (let ((id (ui-next-id)))
           (item-interact itm +mf-none+)
           (ui-force-wdgmsg id "cl" 0 +mf-none+)
           (wait-until (lambda () (= (1+ id) (ui-next-id))))))))
