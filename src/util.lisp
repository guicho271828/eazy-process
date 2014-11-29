(in-package :eazy-process)

(cl-syntax:use-syntax :cl-interpol)
(cl-syntax:use-syntax :annot)

(defun map-line (stream)
  (iter (for line = (read-line stream nil nil))
        (while line)
        (collect line)))
