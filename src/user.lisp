(in-package :eazy-process)

(cl-syntax:use-syntax :cl-interpol)
(cl-syntax:use-syntax :annot)

@export
(defun whoami ()
  (string-trim #?"\n" (shell-command "whoami")))
