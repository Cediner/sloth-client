(in-package :hafen-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Progress Bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(java-field progress-1 "prog")
(defmacro progress ()
  `(progress-1 (gui)))

(defmacro wait-for-progress-to-start ()
  `(wait-until (lambda () (>= (progress) 0.0)) :timeout 2000))

(defmacro wait-for-progress-to-finish ()
  `(wait-until (lambda () (= (progress) -1.0))))

(defmacro wait-for-progress ()
  `(progn
     (wait-for-progress-to-start)
     (wait-for-progress-to-finish)))
  
(export '(progress wait-for-progress wait-for-progress-to-start wait-for-progress-to-finish))
