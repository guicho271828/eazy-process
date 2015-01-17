(in-package :eazy-process)

#|

*  sb-ext:run-program compatibility

|#

(defmacro assert-ignored (thing)
  `(format *error-output*
           "~&Value for ~a is specified as ~a, but it is ignored in eazy-process.~&"
           ',thing ,thing))

(defun run-program (program args
                    &key
                      ENV
                      (ENVIRONMENT (or ENV (environment)))
                      (WAIT T)
                      SEARCH
                      PTY
                      INPUT
                      IF-INPUT-DOES-NOT-EXIST
                      OUTPUT
                      (IF-OUTPUT-EXISTS :ERROR)
                      (ERROR :OUTPUT)
                      (IF-ERROR-EXISTS :ERROR)
                      STATUS-HOOK
                      (EXTERNAL-FORMAT :DEFAULT)
                      DIRECTORY)
  (assert-ignored IF-INPUT-DOES-NOT-EXIST)
  (assert-ignored IF-OUTPUT-EXISTS)
  (assert-ignored IF-ERROR-EXISTS)
  (assert-ignored status-hook)
  ;; pty

  (cffi:convert-from-foreign (iolib.syscalls:os-environ) :pointer)

  (let ((p (shell
            (cons program args)
            +fdspecs-default+
            (if directory
                (cons (format nil "PWD=~a" directory) environment)
                environment)
            search)))
    (setf (external-format p) external-format)
    (if wait
        (wait p)
        p)))


(defun process-kill (process)
  (finalize-process process))

(defun process-wait (process &optional check-for-stopped)
  (wait process (when check-for-stopped :untraced)))

(defun process-input (process)
  (setf (aref (streams process) 0)
        (open (fd-as-pathname process 0)
              :direction :output
              :if-exists :overwrite
              :external-format (external-format process))))

(defun process-output (process)
  (setf (aref (streams process) 0)
        (open (fd-as-pathname process 1)
              :direction :input
              :external-format (external-format process))))

(defun process-error (process)
  (setf (aref (streams process) 0)
        (open (fd-as-pathname process 2)
              :direction :input
              :external-format (external-format process))))

(defun process-close (process)
  (%close-streams (streams process)))
