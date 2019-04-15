(in-package :hafen-config)

(script
  (let ((path (mv-find-path-to-gob (gob-get-closest-by-name "gfx/terobjs/vehicle/wagon"))))
    (when path
      (format t "~A~%" (mv-path-distance path)))))
