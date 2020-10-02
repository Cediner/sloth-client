(defpackage :hafen-config
  (:use :common-lisp :cl-user :java)
  (:export
   #:+left-button+ #:+mid-button+ #:+right-button+
   #:+mf-none+ #:+mf-shift+ #:+mf-ctrl+ #:+mf-alt+
   #:+mf-shift-ctrl+ #:+mf-shift-alt+ #:+mf-ctrl-alt+ #:+mf-shift-ctrl-alt+
   #:+tilesz+
   #:+crawl+ #:+walk+ #:+run+ #:+sprint+))

(in-package :hafen-config)

;;;;Constants
(defconstant +float+ "float")
(defconstant +floatc+ "java.lang.Float")
(defconstant +int+ "int")
(defconstant +long+ "long")
(defconstant +object+ "java.lang.Object")
(defconstant +object-array+ "[Ljava.lang.Object;")
(defconstant +string+ "java.lang.String")
(defconstant +double+ "double")
(defconstant +boolean+ "boolean")

(defconstant +script+ "haven.sloth.script.Script")
(defconstant +context+ "haven.sloth.script.Context")
(defconstant +session+ "haven.sloth.script.SessionDetails")

(defconstant +ui+ "haven.UI")
(defconstant +gui+ "haven.GameUI")

(defconstant +gob+ "haven.Gob")
(defconstant +type+ "haven.sloth.gob.Type")

(defconstant +coord+ "haven.Coord")
(defconstant +coord2d+ "haven.Coord2d")
(defconstant +coord3f+ "haven.Coord3f")

;;;;map-constants
(defconstant +tilesz+ 11)
;;;;ui-Constants
(defconstant +left-button+ 1)
(defconstant +mid-button+ 2)
(defconstant +right-button+ 3)
(defconstant +mf-none+ 0)
(defconstant +mf-shift+ 1)
(defconstant +mf-ctrl+ 2)
(defconstant +mf-shift-ctrl+ 3)
(defconstant +mf-alt+ 4)
(defconstant +mf-shift-alt+ 5)
(defconstant +mf-ctrl-alt+ 6)
(defconstant +mf-shift-ctrl-alt+ 7)

;;;;construct-Constants
;;;;img-Constants
;;;;chat-Constants
(defconstant +achat+ "Area Chat")
(defconstant +vchat+ "Village")
(defconstant +party+ "Party")
(defconstant +bchat+ "Bot-Chat")
;;;;speed-Constants
(defconstant +crawl+ 0)
(defconstant +walk+ 1)
(defconstant +run+ 2)
(defconstant +sprint+ 3)
;;;;inv-constants
;;;;misc-constants
(defconstant +not-found+ -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Load in additional parts of hafen-config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "data/scripts/lib/_config_core.lisp")
(load "data/scripts/lib/_config_script.lisp")
(load "data/scripts/lib/_config_discord.lisp")
(load "data/scripts/lib/_config_logging_and_chat.lisp")
(load "data/scripts/lib/_config_widget.lisp")
(load "data/scripts/lib/_config_coords.lisp")
(load "data/scripts/lib/_config_hotkey.lisp")
(load "data/scripts/lib/_config_ocache.lisp")
(load "data/scripts/lib/_config_mcache.lisp")
(load "data/scripts/lib/_config_mapview.lisp")
(load "data/scripts/lib/_config_gob.lisp")
(load "data/scripts/lib/_config_meters.lisp")
(load "data/scripts/lib/_config_charlist.lisp")
(load "data/scripts/lib/_config_menugrid.lisp")
(load "data/scripts/lib/_config_item.lisp")
(load "data/scripts/lib/_config_inventory.lisp")
(load "data/scripts/lib/_config_progress.lisp")
(load "data/scripts/lib/_config_flowermenu.lisp")
(load "data/scripts/lib/_config_isbox.lisp")
(load "data/scripts/lib/_config_kin.lisp")
(load "data/scripts/lib/_config_fight.lisp")
(load "data/scripts/lib/_config_pointer.lisp")
(load "data/scripts/lib/_config_bbox.lisp")
(load "data/scripts/lib/_config_utilities.lisp")
(load "data/scripts/lib/_config_equip.lisp")
