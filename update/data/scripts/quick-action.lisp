(defpackage :quick-action
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))

(in-package :quick-action)

(defconstant +sit-objs+
  `("gfx/terobjs/furn/longbench"))
(defconstant +light-objs+
  `("gfx/terobjs/kiln"
    "gfx/terobjs/smelter"
    "gfx/terobjs/tarkiln"
    "gfx/terobjs/fineryforage"
    "gfx/terobjs/steelcrucible"
    "gfx/terobjs/pow"
    "gfx/terobjs/oven"
    "gfx/terobjs/cauldron"))
(defconstant +signpost-objs+
  `("gfx/terobjs/road/milestone-wood-e"
    "gfx/terobjs/road/milestone-wood-m"
    "gfx/terobjs/road/milestone-stone-e"
    "gfx/terobjs/road/milestone-stone-m"))
(defconstant +gate-objs+
  `("gfx/terobjs/arch/brickwallgate"
    "gfx/terobjs/arch/brickwallbiggate"
    "gfx/terobjs/arch/palisadegate"
    "gfx/terobjs/arch/palisadebiggate"
    "gfx/terobjs/arch/drystonewallgate"
    "gfx/terobjs/arch/drystonewallbiggate"
    "gfx/terobjs/arch/polegate"
    "gfx/terobjs/arch/polebiggate"))
(defconstant +shoo-objs+
  `("gfx/kritter/cattle/cattle"
    "gfx/kritter/sheep/sheep"
    "gfx/kritter/cattle/calf"
    "gfx/kritter/sheep/lamb"
    "gfx/kritter/horse/foal"
    "gfx/kritter/pig/piglet"
    "gfx/kritter/pig/hog"
    "gfx/kritter/pig/sow"))
(defconstant +ride-objs+
  `("gfx/kritter/horse/stallion"
    "gfx/kritter/horse/mare"))
(defconstant +open-objs+
  `("gfx/terobjs/arch/cellardoor"
    "gfx/terobjs/cupboard"
    "gfx/terobjs/stockpile"
    "gfx/terobjs/furn"
    "gfx/terobjs/swheel"
    "gfx/terobjs/chest"
    "gfx/terobjs/largechest"
    "gfx/terobjs/winepress"
    "gfx/terobjs/curdingtub"
    "gfx/terobjs/crate"
    "gfx/terobjs/coffer"
    "gfx/terobjs/loom"
    "gfx/terobjs/meatgrinder"
    "gfx/terobjs/cheeserack"
    "gfx/terobjs/arch/logcabin-door"
    "gfx/terobjs/arch/greathall-door"
    "gfx/terobjs/arch/stonemansion-door"
    "gfx/terobjs/arch/stonestead-door"
    "gfx/terobjs/arch/stonetower-door"
    "gfx/terobjs/arch/upstairs"
    "gfx/terobjs/arch/cellarstairs"
    "gfx/terobjs/chickencoop"
    "gfx/terobjs/ttub"
    "gfx/terobjs/dreca"
    "gfx/terobjs/htable"
    "gfx/terobjs/vehicle/wagon"
    "gfx/terobjs/vehicle/rowboat"
    
    "gfx/terobjs/kiln"
    "gfx/terobjs/smelter"
    "gfx/terobjs/tarkiln"
    "gfx/terobjs/fineryforage"
    "gfx/terobjs/steelcrucible"
    "gfx/terobjs/oven"
    "gfx/terobjs/cauldron"))

(defun check-obj (obj-name best-dist)
  (let ((obj (gob-get-closest-by-name obj-name)))
    (if (and obj
             (< (coord2d-dist (gob-rc (my-gob)) (gob-rc obj)) best-dist))
        obj
        nil)))

(defmacro check-group (objs act-val act-dist act-obj act)
  (let ((obj-sym (gensym))
        (ret-sym (gensym)))
    `(dolist (,obj-sym ,objs)
       (let ((,ret-sym (check-obj ,obj-sym ,act-dist)))
         (when ,ret-sym
           (setf ,act ,act-val
                 ,act-obj ,ret-sym
                 ,act-dist (coord2d-dist (gob-rc (my-gob)) (gob-rc ,ret-sym))))))))

(defun get-act ()
  (let ((firebrand (if (held-item)
                       (or (string= "Lit Torch" (item-name (held-item)))
                           (string= "Box of Matches" (item-name (held-item))))
                       nil))
        (act :nil)
        (act-obj nil)
        (act-dist 99999999999))
    (check-group +sit-objs+ :sit act-dist act-obj act)
    (check-group +ride-objs+ :ride act-dist act-obj act)
    (check-group +open-objs+ :open act-dist act-obj act)
    (check-group +gate-objs+ :gate act-dist act-obj act)
    (check-group +signpost-objs+ :signpost act-dist act-obj act)
    (check-group +shoo-objs+ :shoo act-dist act-obj act)
    (when firebrand
      (check-ground +light-objs+ :light-obj act-dist act-obj act))
    (values act act-obj)))

(script
  (multiple-value-bind (act obj)
      (get-act)
    (cond
      ((member act '(:light-obj :gate :signpost :open :sit))
       (mv-click-gob obj +right-button+ +mf-none+))
      ((member act '(:shoo :ride))
       (mv-smart-move-to-gob obj)
       (mv-click-gob obj +right-button+ +mf-none+)
       (wait-for-flowermenu)
       (when (flowermenu)
         (flowermenu-select-by-name (if (eq act :shoo) "Shoo" "Giddyup!")))))))
