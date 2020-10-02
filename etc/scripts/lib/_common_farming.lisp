(defpackage :farming-common
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :farming-common)

(defstruct crop
  name
  kind
  finish-stage) ;; stages go from 0 -> finish-stage!

(defconstant +trellis+ :trellis)
(defconstant +ground+ :ground)
(defconstant +crops+
  (list
   (make-crop :name "gfx/terobjs/plants/flax" :kind +ground+ :finish-stage '(3))
   (make-crop :name "gfx/terobjs/plants/barley" :kind +ground+ :finish-stage '(3))
   (make-crop :name "gfx/terobjs/plants/carrot" :kind +ground+ :finish-stage '(3))
   (make-crop :name "gfx/terobjs/plants/poppy" :kind +ground+ :finish-stage '(4))
   (make-crop :name "gfx/terobjs/plants/hemp" :kind +ground+ :finish-stage '(4))
   (make-crop :name "gfx/terobjs/plants/pipeweed" :kind +trellis+ :finish-stage '(4))
   (make-crop :name "gfx/terobjs/plants/beet" :kind +ground+ :finish-stage '(3))
   (make-crop :name "gfx/terobjs/plants/hops" :kind +trellis+ :finish-stage '(6))
   (make-crop :name "gfx/terobjs/plants/peas" :kind +trellis+ :finish-stage '(4))
   (make-crop :name "gfx/terobjs/plants/yellowonion" :kind +ground+ :finish-stage '(3))
   (make-crop :name "gfx/terobjs/plants/pumpkin" :kind +ground+ :finish-stage '(4))
   (make-crop :name "gfx/terobjs/plants/pepper" :kind +trellis+ :finish-stage '(6))
   (make-crop :name "gfx/terobjs/plants/wine" :kind +trellis+ :finish-stage '(6))
   (make-crop :name "gfx/terobjs/plants/redonion" :kind +ground+ :finish-stage '(3))
   (make-crop :name "gfx/terobjs/plants/turnip" :kind +ground+ :finish-stage '(3))
   (make-crop :name "gfx/terobjs/plants/cucumber" :kind +trellis+ :finish-stage '(4))
   (make-crop :name "gfx/terobjs/plants/leek" :kind +ground+ :finish-stage '(4))
   (make-crop :name "gfx/terobjs/plants/lettuce" :kind +ground+ :finish-stage '(4))
   (make-crop :name "gfx/terobjs/plants/wheat" :kindd +ground+ :finish-stage '(3))
   (make-crop :name "gfx/terobjs/plants/millet" :kind +ground+ :finish-stage '(3))))
(defconstant +crop-lookup+ (make-hash-table :test 'equal))
(loop
   for crop in +crops+
   do (setf (gethash (crop-name crop) +crop-lookup+) crop))

(defun is-a-harvestable-crop (gob kind)
  (if (jeq +plant+ (gob-type gob))
      (let* ((name (gob-name gob))
             (stage (gob-sdt gob))
             (crop (gethash name +crop-lookup+)))
        (if (and crop
                 (eq kind (crop-kind crop))
                 (member stage (crop-finish-stage crop)))
            t
            nil))
      nil))

(defun is-a-seed (itm)
  (let ((name (item-name itm)))
    (or (search "seeds" name)
        (string= "gfx/invobjs/beet" name)
        (string= "gfx/invobjs/carrot" name))))

(export '(+trellis+ +ground+
          is-a-harvestable-crop is-a-seed))
