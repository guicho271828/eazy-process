(in-package :eazy-process)

(cl-syntax:use-syntax :cl-interpol)
(cl-syntax:use-syntax :annot)

;; accessors

;; fork

;; exec

@export
(defun fd (pid num)
  "return the pathname of file descriptor in procfs. pid can be a process id or :self"
  (etypecase pid
    ((or symbol string number) (pathname #?"/proc/self/fd/$(num)"))))

;; (with-open-file (s (fd 14299 1) :direction :output :if-exists :overwrite)
;;   (format s "hi!"))


@export
(defun pgid (&optional (pid (pid)))
  (eazy-process.swig:getpgid pid))
(defun setpgid (pgid &optional (pid (pid)))
  (eazy-process.swig:setpgid pid pgid))
(defsetf pgid setpgid)

