
(in-package :eazy-process)

(defun mktempfifo ()
  (let* ((dir (mkdtemp "/tmp/lisp."))
         (path (format nil "~a/fifo" dir)))
    (mkfifo path 777)
    path))
