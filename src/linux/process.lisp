
(in-package :eazy-process)

(defun fd-as-pathname (process fd)
  "Return the pathname in the proc file system.
Lisp can retrieve the output of the subprocess by opening this file."
  (proc :self :fd (fd process fd)))

(defun pgid (process) (stat process :pgrp))
(defun ppid (process) (stat process :ppid))

;; (defun pipe-process (proc1 proc2)
;;   )

(defun all-processes ()
  (iter (for path in (iolib.os:list-directory *procfs-root*))
        (ignore-errors
          (collect 
              (parse-integer
               (iolib.pathnames:file-path-file-name path))))))

(defun threads (pid)
  (iter (for path in (iolib.os:list-directory (proc pid :task)))
        (collect 
            (parse-integer
             (iolib.pathnames:file-path-file-name path)))))

(defun subprocesses (target-ppid)
  (iter (for pid in (all-processes))
        ;; since the process might be dead and the directory could
        ;; disappear
        (let ((ppid (ignore-errors (stat pid :ppid))))
          (when ppid
            (when (= ppid target-ppid)
              (collect pid))))))
