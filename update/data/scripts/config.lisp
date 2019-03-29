(defpackage :hafen-config
  (:use :common-lisp :cl-user :java :jss)
  (export
   ))

(in-package :hafen-config)

;;;;Constants
(defconstant +float+ "float")
(defconstant +floatc+ "java.lang.Float")
(defconstant +int+ "int")
(defconstant +long+ "long")
(defconstant +object+ "java.lang.Object")
(defconstant +string+ "java.lang.String")
(defconstant +double+ "double")
(defconstant +context+ "haven.sloth.script.Context")
(defconstant +ui+ "haven.UI")
(defconstant +gui+ "haven.GameUI")
(defconstant +map+ "haven.MapView")

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
(defmacro svar (class var) ;;static vars
  `(jfield ,class ,var))
(defmacro ivar (instance var) ;;instanced vars
  `(jfield ,var ,instance))

(defmacro forever (&body body)
  `(loop
     do (progn ,@body)))

(defmacro while ((con) &body body)
  `(loop
     while ,con
     do (progn ,@body)))

(defmacro doarr ((var arr) &body body)
  `(loop
     for i from 0 to (1- (length ,arr))
     do (let ((,var (aref ,arr i)))
          (progn ,@body))))

(defmacro java-func (class
                     lisp-func-name
                     java-func-name
                     &rest args)
  (let ((argv `(obj-ref)))
    (dotimes (i (length args))
      (setf argv (append argv `(,(intern (string-upcase (format nil "arg-~A" i)))))))
    `(defun ,lisp-func-name ,argv
       (checkintp)
       (jcall (jmethod ,class ,java-func-name ,@args) ,@argv))))

(defmacro java-sfunc (class
                      lisp-func-name
                      java-func-name
                      argc)
  (let ((argv `()))
    (dotimes (i argc)
      (setf argv (append argv `(,(intern (string-upcase (format nil "arg-~A" i)))))))
    `(defun ,lisp-func-name ,argv
       (checkintp)
       (jstatic ,java-func-name ,class ,@argv))))

(defun print-excp (exp)
  (let ((msg (jcall "getMessage" exp)))
    (unless (search "InterruptedException" msg)
      (misc-sysprint "An error has occured, check your log files to see why")
      (misc-log (jcall "getMessage" exp)))))

(defmacro script (&body body)3
  `(lambda ()
     (handler-case
         (progn
           ,@body
           (misc-log "Successful run"))
       (java-exception (msg)
         (progn
           (print-excp msg)))
       (error (msg)
         (format t "~A~%" msg)))))

(defmacro ui ()
  `(svar +context+ "ui"))

(defmacro gui ()
  (let ((var (gensym)))
    `(let ((,var (ui)))
       (if ,var
           (ivar ,var "gui")
           nil))))

(defmacro map ()
  (let ((var (gensym)))
    `(let ((,var (gui)))
       (if ,var
           (ivar ,var "map")
           nil))))
