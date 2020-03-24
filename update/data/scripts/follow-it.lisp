(defpackage :follow-it
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))

(in-package :follow-it)

;;;;TODO: if they were next to a house, or minehole -> likely went in it. we should too
(defun check-where-gob-went (lpos)
  )

(defun follow (gid)
  (let ((last-move 0)
        (last-known-pos (gob-rc (oc-get-gob gid))))
    (forever
      (let ((gob (oc-get-gob gid)))
        (if gob
            (progn
              (setf last-known-pos (gob-rc gob))
              (when (> (- (get-time) last-move 300))
                (let ((path (mv-find-path-to-gob gob)))
                  (when path
                    (mv-clear-moves)
                    (loop
                       for i from 0 to (1- (length path))
                       do (mv-queue-move (move-destination (aref path i)))))))
              (check-where-gob-went last-known-pos)))
        (sleep 0.1)))))

(script
  (let ((gid 0))
    (let ((gob (prompt-for-selected-gob "Mark an object that you want to follow")))
      (setf gid (gob-id gob)))
    (follow gid)))
