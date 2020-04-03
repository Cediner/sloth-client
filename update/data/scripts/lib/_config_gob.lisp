(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Gob API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Gob Types
(defconstant +plant+ (svar +type+ "PLANT"))
(defconstant +human+ (svar +type+ "HUMAN"))
(defconstant +animal+ (svar +type+ "ANIMAL"))
(defconstant +tamed-animal+ (svar +type+ "TAMEDANIMAL"))
(defconstant +small-animal+ (svar +type+ "SMALLANIMAL"))
(defconstant +water-vehicle+ (svar +type+ "WATERVEHICLE"))
(defconstant +vehicle+ (svar +type+ "VEHICLE"))

(java-func +gob+ gob-c "getc")
(java-func +gob+ gob-name "name")
(java-func +gob+ gob-sdt "sdt")
(java-func +gob+ gob-overlays "overlays")
;;friendly
(java-func +gob+ is-gob-dead "isDead")
(java-func +gob+ is-gob-friendly "isFriendly")
(java-func +gob+ is-gob-dangerous "isDangerous")
(java-func +gob+ is-gob-moving-1 "moving")
;;human-specific functions
(java-func +gob+ gob-kinname "kinname")
(java-func +gob+ gob-equipment "equipment")
;;cavein_dust
(java-field gob-id "id")
(java-field gob-rc "rc")
(java-field gob-type "type")
(java-field gob-angle "a")

;;type -> Gob Types
(defmacro is-gob-a (gob type)
  `(equal (ivar ,gob "type") ,type))

(defun my-gob ()
  (oc-get-gob (mv-plgob (mv))))

(defun is-gob-moving (gob)
  (or (is-gob-moving-1 gob)
      (and (= (mv-plgob (mv)) (gob-id gob))
           (mv-has-moves (mv)))))

(defun wait-for-movement (&key (gob (my-gob)) (test #'is-gob-moving))
  (progn
     (wait-for-movement-to-start :gob gob :test test)
     (wait-for-movement-to-finish :gob gob :test test)))
  
(defun wait-for-movement-to-start (&key (gob (my-gob)) (test #'is-gob-moving))
  (let ((start-c (gob-rc gob)))
    (wait-until (lambda () (or (funcall test gob)
                               (not (coord2d-eq start-c (gob-rc gob)))))
                :timeout 2000)))

(defun wait-for-movement-to-finish (&key (gob (my-gob)) (test #'is-gob-moving))
  (sleep 0.5)
  (wait-until (lambda ()
                (not (funcall test gob)))))

(defun gob-get-by-name (name)
  (let ((gobs (oc-get-all-gobs)))
    (doarr (gob gobs)
      (when (search name (gob-name gob))
        (return-from gob-get-by-name gob)))
    nil))

(defun gob-get-all-by-filter (filter-fun)
  (let ((gobs (oc-get-all-gobs))
        (ret ()))
    (doarr (gob gobs)
      (when (funcall filter-fun gob)
        (push gob ret)))
    ret))

(defmacro gob-get-all-by-name (name)
  `(gob-get-all-by-filter (lambda (gob) (string= ,name (gob-name gob)))))

(defun gob-get-closest-by-filter-and-path (filter-fun)
  (let ((me (my-gob)))
    (if me
        (let ((gobs (gob-get-all-by-filter filter-fun))
              (best nil)
              (bestdist 0)
              (mc (gob-rc me)))
          (dolist (gob gobs)
            (let* ((path (mv-find-path-to-gob gob))
                   (dist (if path (mv-path-distance path) 99999999)))
              (when (and path
                         (or (null best)
                             (< dist bestdist)))
                (setf best gob)
                (setf bestdist dist))))
          best)
        nil)))

(defun gob-get-closest-by-filter (filter-fun)
  (let ((me (my-gob)))
    (if me
        (let ((gobs (gob-get-all-by-filter filter-fun))
              (best nil)
              (bestdist 0)
              (mc (gob-rc me)))
          (dolist (gob gobs)
            (when (or (null best)
                      (< (coord2d-dist (gob-rc gob) mc) bestdist))
              (setf best gob)
              (setf bestdist (coord2d-dist (gob-rc gob) mc))))
          best)
        nil)))

(defmacro gob-get-closest-by-name-and-path (name)
  `(gob-get-closest-by-filter-and-path (lambda (gob) (string= ,name (gob-name gob)))))

(defmacro gob-get-closest-by-name (name)
  `(gob-get-closest-by-filter (lambda (gob) (string= ,name (gob-name gob)))))

(export '(+plant+ +human+ +animal+ +tamed-animal+ +small-animal+ +water-vehicle+ +vehicle+
          gob-c gob-name gob-type gob-overlays gob-sdt is-gob-dead
          gob-kinname gob-equipment
          gob-id gob-rc
          is-gob-a my-gob
          is-gob-moving is-gob-friendly is-gob-dangerous
          wait-for-movement
          wait-for-movement-to-start
          wait-for-movement-to-finish
          gob-get-by-name gob-get-all-by-name gob-get-all-by-filter
          gob-get-closest-by-filter-and-path
          gob-get-closest-by-name-and-path
          gob-get-closest-by-filter gob-get-closest-by-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Gob Overlay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +gob-overlay+ "haven.Gob$Overlay")
(java-func +gob-overlay+ gob-overlay-name "name")
(java-func +gob-overlay+ gob-overlay-id "id")

(defun gob-overlay-has (gob overlay-name)
  (doarr (ol (gob-overlays gob))
    (when (string= (gob-overlay-name ol) overlay-name)
      (return-from gob-overlay-has t)))
  nil)

(defun gob-get-overlay-by-name (gob overlay-name)
  (doarr (ol (gob-overlays gob))
    (when (string= (gob-overlay-name ol) overlay-name)
      (return-from gob-get-overlay-by-name ol)))
  nil)

(export '(gob-overlays gob-overlay-name gob-overlay-id gob-overlay-has gob-get-overlay-by-name))
