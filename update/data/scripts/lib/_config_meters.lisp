(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Meters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(java-field session-shp "shp")
(java-field session-hhp "hhp")
(java-field session-mhp "mhp")
(java-field session-stam "stam")
(java-field session-energy "energy")

(with-session-define shp session-shp)
(with-session-define hhp session-hhp)
(with-session-define mhp session-mhp)
(with-session-define stamina session-stam)
(with-session-define energy session-energy)

(export '(shp hhp mhp stamina energy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SpeedGet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +speed+ "haven.Speedget")
(java-func +session+ session-speedget "getSpeedget")
(java-func +speed+ speed-set-1 "set" +int+)
(java-field speed-cur-1 "cur")
(java-field speed-max-1 "max")
 
(with-session-define speed session-speedget)
(defmacro speed-cur ()
  `(speed-cur-1 (speed)))
(defmacro speed-max ()
  `(speed-max-1 (speed)))
(defmacro speed-set (speed)
  `(speed-set-1 (speed) ,speed))

(export '(speed speed-cur speed-max speed-set))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; VMeters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +vmeter+ "haven.VMeter")
(java-func +session+ session-vmeters "getVMeters")
(with-session-define vmeters-1 session-vmeters)
(defmacro vmeters ()
  `(listify (vmeters-1)))
(java-func +vmeter+ vmeter-amount "amount")
(java-func +vmeter+ vmeter-owner "owner")

(export '(vmeters
          vmeter-amount vmeter-owner))
