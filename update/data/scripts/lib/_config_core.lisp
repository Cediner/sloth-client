(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java builtins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro get-time ()
  `(jstatic "currentTimeMillis" "java.lang.System"))

(defun jeq (x y)
  (= (jstatic "identityHashCode" "java.lang.System" x)
     (jstatic "identityHashCode" "java.lang.System" y)))

(export '(get-time jeq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper macros
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

(defun wait-until (getter &key (refresh 100) timeout )
  (let ((st (/ refresh 1000))
        (start (get-time)))
    (loop
       for ret = (funcall getter)
       when ret return ret
       do (progn
            (sleep st)
            (when (and timeout (>= (- (get-time) start) timeout))
              (return-from wait-until nil))))))
  
(defmacro doarr ((var arr) &body body)
  (let ((arr-sym (gensym)))
    `(let ((,arr-sym ,arr))
       (when ,arr-sym
         (loop
            for i from 0 to (1- (length ,arr-sym))
            do (let ((,var (aref ,arr-sym i)))
                 (progn ,@body)))))))

(defmacro checkintp ()
  `(jstatic "checkintp" +script+))

(defmacro java-sfield (lisp-fun-name java-class java-field-name)
  `(defmacro ,lisp-fun-name ()
     (let ((cls ,java-class)
           (field ,java-field-name))
     `(jfield ,cls ,field))))

(defmacro java-field (lisp-macro-name java-field-name)
  `(defmacro ,lisp-macro-name (arg)
     (let ((field ,java-field-name))
       `(jfield ,field ,arg))))

(defmacro java-func (class
                     lisp-func-name
                     java-func-name
                     &rest args)
  (let ((argv `(obj-ref)))
    (dotimes (i (length args))
      (setf argv (append argv `(,(intern (string-upcase (format nil "arg-~A" i)))))))
    `(defun ,lisp-func-name ,argv
       (checkintp)
       #|(format t "(jcall (jmethod ~A ~A))~%" ,class ,java-func-name)
       (loop for frame in (cddr (sys:backtrace))
          do (format t "~A~%" frame))|#
       (jcall (jmethod ,class ,java-func-name ,@args) ,@argv))))

(defmacro java-sfunc (class
                      lisp-func-name
                      java-func-name
                      &optional (argc 0))
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
      (format t "~A~%" (jcall "getMessage" exp))
      (misc-log (jcall "getMessage" exp)))))

(defmacro debug-script (&body body)
  `(handler-bind
       ((java-exception (msg) (print-excp msg))
        (error (msg) (format t "~A~%" msg)))
     (progn
       ,@body
       (misg-log "Successful run"))))

(defmacro script (&body body)
  `(handler-case
       (progn
         ,@body
         (misc-log "Successful run"))
     (java-exception (msg)
         (progn
           (print-excp msg)))
     (error (msg)
       (format t "~A~%" msg))))

(defun listify (arr)
  (let ((ret ()))
    (doarr (itm arr)
      (push itm ret))
    ret))

(export '(svar ivar forever while wait-until doarr checkintp java-sfield java-field java-func java-sfunc print-excp script listify))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Optional - Java builtin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +optional+ "java.util.Optional")
(java-func +optional+ optional-get "get")
(java-func +optional+ optional-is-present "isPresent")

(export '(optional-get optional-is-present))

