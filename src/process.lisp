
(in-package :eazy-process)

(defclass process ()
  ((#:pid :reader pid :initarg :pid)
   (#:fds :reader %fds :initarg :fds)
   (#:external-format :accessor external-format)
   (#:streams :accessor streams :initarg :streams))
  (:documentation
   "A class representing a process.
Properties of a process are accessible through several functions."))

(defmethod print-object ((p process) s)
  (print-unreadable-object (p s :type t)
    (format s "PID: ~A" (pid p))))

(defun fds (process)
  "Returns an fresh array of fds available from lisp process."
  (copy-array (%fds process)))

(defun %make-process (pid fds)
  (let* ((streams (make-array (length fds) :initial-element nil))
         (process (make-instance 'process
                                 :pid pid
                                 :fds fds
                                 :streams streams)))
    (finalize process
              (lambda () (%finalize-process pid 15 streams)))
    process))

(defun %finalize-process (pid sig streams)
  "Waitpid the process. If the process is alive, kill it with SIG first,
then with sigkill."
  ;; This function should not contain reference to the process itself
  ;; because it prevents process object from GC-ing.
  (%close-streams streams)
  (handler-case ; in case pid does not exist
      (when (zerop (waitpid pid iolib/syscalls:WNOHANG))
        (format t "~&; Killing ~a~&" pid)
        (kill pid sig)
        (when (zerop (waitpid pid iolib/syscalls:WNOHANG))
          (format t "~&; Force killing ~a~&" pid)
          (kill pid 9)
          (waitpid pid 0)))
    (iolib.syscalls:syscall-error (c)
      (declare (ignore c))
      ;; (format t "~&; Process ~a does not exist!~&" pid)
      nil)))

(defun %close-streams (streams)
  (map nil
       (lambda (s)
         (when (streamp s)
           ;; flush-output will cause SIGPIPE when the process is dead?
           (close s :abort (output-stream-p s))))
       streams))

(defun finalize-process (process &optional (sig 15))
  (%finalize-process (pid process) sig (streams process)))

;; Note: without calling waitpid, the child becomes a zombie process.
;; child process should be waited when the process object is GC'ed.

(declaim (inline wait))
(defun wait (process &optional option)
  "option is one of :nohang, :untraced, :continued.
Returns many values:
 (boolean ifexited)
 (integer exitstatus)
 (boolean ifsignalled)
 (integer termsig)
 (boolean coredump)
 (boolean ifstopped)
 (integer stopsig)
 (boolean ifcontinued).

When the value is inappropriate, some integer values may return NIL.
For details see man wait(2)."
  (multiple-value-bind (success status)
      (waitpid (pid process)
               (case option
                 (:nohang iolib/syscalls:WNOHANG)
                 (:untraced iolib/syscalls:WUNTRACED)
                 (:continued iolib/syscalls:WCONTINUED)
                 (t 0)))
    (when (minusp success)
      (error 'isys:syscall-error
             :message "waitpid failed"
             :code (isys:errno)
             :syscall "waitpid"))
    (values
     (iolib/syscalls:WIFEXITED status)
     (when (iolib/syscalls:WIFEXITED status)
       (iolib/syscalls:WEXITSTATUS status))
     (iolib/syscalls:WIFSIGNALED status)
     (when (iolib/syscalls:WIFSIGNALED status)
       (iolib/syscalls:WTERMSIG status))
     (when (iolib/syscalls:WIFSIGNALED status)
       (iolib/syscalls:WCOREDUMP status))
     (iolib/syscalls:WIFSTOPPED status)
     (when (iolib/syscalls:WIFSTOPPED status)
       (iolib/syscalls:WSTOPSIG status))
     (iolib/syscalls:WIFCONTINUED status)
     status)))

(defun fd (process n)
  "Returns the file descriptor of the lisp process.
The returned fd is connected to the n-th fd of the child process through a pipe.
Example:

 (fd P 1) ; --> 5 

This means that the 5'th fd of the lisp process is connected to the 1st fd of the process P."
  (aref (fds process) n))

 
