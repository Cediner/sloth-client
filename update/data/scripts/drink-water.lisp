(defpackage :drink-water
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :drink-water)

(script
  (drink-water nil))
