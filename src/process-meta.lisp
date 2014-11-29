(in-package :eazy-process)

(cl-syntax:use-syntax :cl-interpol)
(cl-syntax:use-syntax :annot)

@export
(defun subprocesses (ppid)
  (let ((str (shell-command #?"pgrep -P $(ppid)")))
    (with-input-from-string (s str)
      (iter (for pid = (read s nil nil))
            (while pid)
            (collect pid)))))


