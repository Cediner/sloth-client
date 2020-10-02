(defpackage :auto-drink
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :hafen-config)



(script
 (forever
  (check-stam-and-drink :key drink-at 75)
  (sleep 1)))
