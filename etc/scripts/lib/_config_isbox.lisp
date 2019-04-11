(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ISBox / Stockpiles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +isbox+ "haven.ISBox")
(java-func +isbox+ stockpile-name "materialName")
(java-func +isbox+ stockpile-take-all "takeall")
(java-func +isbox+ stockpile-add-all "addall")
(java-func +isbox+ stockpile-take-some "takesome" +int+)
(java-func +isbox+ stockpile-add-some "addsome" +int+)
(java-field stockpile-current-size "cur")
(java-field stockpile-total-size "total")

(java-func +session+ session-isboxes "getISBoxes")
(with-session-define stockpiles session-isboxes)

(defun stockpiles-by-filter (filter-fun)
  (let ((stockpiles (stockpiles)))
    (if stockpiles
        (loop
           for i from 0 to (1- (length stockpiles))
           when (funcall filter-fun (aref stockpiles i))
           collect (aref stockpiles i))
        ())))

(defmacro stockpiles-by-name (name)
  `(stockpiles-by-filter (lambda (stockpile)
                           (string= ,name (stockpile-name stockpile)))))

(defun stockpile-by-name (name)
  (let ((stockpiles (stockpiles-by-filter
                     (lambda (stockpile)
                       (string= name (stockpile-name stockpile))))))
    (if stockpiles
        (car stockpiles)
        nil)))

(export '(stockpile-name stockpile-take-all stockpile-add-all stockpile-take-some stockpile-add-some
          stockpile-current-size stockpile-total-size
          stockpiles
          stockpiles-by-filter
          stockpiles-by-name stockpile-by-name))
