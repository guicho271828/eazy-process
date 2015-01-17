
(in-package :eazy-process)


(defun environment ()
  (let ((e (iolib.syscalls:os-environ)))
    (iter (for i from 0)
          (for ptr = (mem-aref e :pointer i))
          (for str = (foreign-string-to-lisp ptr))
          (while str)
          (collect str))))
