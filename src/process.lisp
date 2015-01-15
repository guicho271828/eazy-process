
(in-package :eazy-process.impl)

(defclass process ()
  ((#:pid :reader pid :initarg :pid)
   (#:fds :reader fds :initarg :fds))
  (:documentation
   "A class representing a process.
Properties of a process are accessible through several functions."))

(defmethod print-object ((p process) s)
  (print-unreadable-object (p s :type t)
    (format s "PID: ~A" (pid p))))

(defun %make-process (pid fds)
  (let ((process (make-instance 'process :pid pid :fds fds)))
    (finalize
     process
     (lambda ()
       (ignore-errors ; in case pid has already dead
         (when (zerop (waitpid pid iolib/syscalls:WNOHANG))
           (format t "~&; Killing ~a" pid)
           (kill pid 15)
           (when (zerop (waitpid pid iolib/syscalls:WNOHANG))
             (format t "~&; Force killing ~a" pid)
             (kill pid 9)
             (waitpid pid 0))))))
    process))


;; Note: without calling waitpid, the child becomes a zombie process.
;; child process should be waited when the process object is GC'ed.

(defun wait (process &optional option)
  "option is one of :nohang, :untraced, :continued.
Returns (values (boolean exited-p) (integer exitstatus) (integer waitpid-status)).
For the further investigation of waitpid-status, use iolib/syscalls:WIFSIGNALED etc."
  (let ((status
         (waitpid (pid process)
                  (case option
                    (:nohang iolib/syscalls:WNOHANG)
                    (:untraced iolib/syscalls:WUNTRACED)
                    (:continued iolib/syscalls:WCONTINUED)
                    (t 0)))))
    ;; lisp does not open any of these files
    ;; (map nil (lambda (fd) (when fd (iolib/syscalls:close fd))) (fds process))
    (values (iolib/syscalls:WIFEXITED status)
            (iolib/syscalls:WEXITSTATUS status)
            status)))

(defun fd (process fd)
  "Read the fd-th file descriptor of the child process"
  (aref (fds process) fd))

(defun fd-as-pathname (process fd)
  "Access the fd as a file in the proc file system. Lisp can retrieve the
output of the subprocess by opening this file."
  (proc process :fd fd))

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

(defun tasks (pid)
  (iter (for path in (iolib.os:list-directory (proc pid :tasks)))
        (collect 
            (parse-integer
             (iolib.pathnames:file-path-file-name path)))))

(defun subprocesses (ppid)
  (iter (for pid in (all-processes))
        (when (= ppid (stat pid :ppid))
          (collect pid))))
