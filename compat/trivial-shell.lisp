(in-package :eazy-process)

#|

*  trivial-shell compatibility

Trivial-shell is "A simple Common-Lisp interface to the underlying Operating System".
It runs a shell command in a sequencial manner.
In this compatibility version,
the underlying interpreter no longer have to be a bourne-compatible-shell.
e.g. you can use perl -e 'XXX'.

Goal: simple interface compatible to trivial-shell @
https://github.com/gwkkwg/trivial-shell.git

API: there are a few modifications.
Timeout-related stuff and EXIT are removed.

trivial-shell:*BOURNE-COMPATIBLE-SHELL* --- *interpreter*
trivial-shell:GET-ENV-VAR               --- get-env-var
trivial-shell:SHELL-COMMAND             --- shell-command
trivial-shell:*SHELL-SEARCH-PATHS*      --- DEPRECATED
trivial-shell:EXIT                      --- NOTINCLUDED
trivial-shell:TIMEOUT-ERROR             --- NOTINCLUDED
trivial-shell:TIMEOUT-ERROR-COMMAND     --- NOTINCLUDED
trivial-shell:WITH-TIMEOUT              --- NOTINCLUDED

|#

(defparameter *interpreter* "sh -c"
  "Command line string which is invoked in order to run the subshell. Default value is \"sh -c\".
The value is then followed by the command specified in shell-command, e.g. ,
when command = 'ls -la', the running command is \"sh -c 'ls -la'\".
It provides the ability to call the interpreters like perl/AWK easily.

The name/path of the interpreter must be separated by spaces from the options like '-c'.
Do not use complex things like --- LANG=C sh . The first word is always considered the pathname.
If you want to do complicated stuff, then do it inside the interpreter.

The process is forked, then the child process calls execvp with the name of
the interpreter. (This is defined in function `eazy-process:shell'.) The
actual pathname of the intepreter can be resolved using PATH environment
variable.
")

(define-symbol-macro *bourne-compatible-shell* *interpreter*)

(defmacro with-retry-open-file ((tag &key (max MOST-POSITIVE-FIXNUM)) args &body body)
  (with-gensyms (maxcnt failcnt condition blk)
    `(block ,blk
       (let ((,maxcnt ,max)
             (,failcnt 0))
         (tagbody
           ,tag
           (handler-case
               (return-from ,blk
                 (with-open-file ,args
                   ,@body))
             (file-error (,condition)
               (sleep 1/1000)
               (incf ,failcnt)
               (if (< ,failcnt ,maxcnt)
                   (go ,tag)
                   (signal ,condition)))))))))

(declaim (ftype (function ((or pathname string)
                           &key
                           (:input (or stream string null))
                           (:external-format symbol)
                           (:verbose t))
                          (values string string list))
                shell-command))

(defun shell-command (command &key
                                (input "")
                                (result-type :string)
                                (external-format :default)
                                verbose)
  "simple interface compatible to trivial-shell @
https://github.com/gwkkwg/trivial-shell.git.

returns (values output error-output exit-status).
The input is read from the :input key argument.

Additional functionality:

:external-format --- specifies the decoding of the output.
"
  (let* ((command (if (pathnamep command)
                      (namestring command)
                      command))
         (argv (append (ppcre:split "[ \t]+" *interpreter*)
                       (list command))))
    (when verbose
      (format *error-output* "~&; ~a '~a'" *interpreter* command))
    (with-process (p argv)
      (with-retry-open-file (:start)
        (s (fd-as-pathname p 0) :direction :output :if-exists :overwrite)
        (with-retry-open-file (:start1)
          (s1 (fd-as-pathname p 1) :direction :input :external-format external-format)
          (with-retry-open-file (:start2)
            (s2 (fd-as-pathname p 2) :direction :input :external-format external-format)
            (ecase result-type
              (:string
               (io-loop-string p
                               (etypecase input
                                 (stream input)
                                 (string (make-string-input-stream input)))
                               s
                               (if verbose (make-echo-stream s1 *standard-output*) s1)
                               (if verbose (make-echo-stream s2 *error-output*) s2))))))))))

;; TODO
;; :result-type --- if it is :stream, it returns the bare fd-stream.
;; Note that if you do not read the stream at an appropriate frequency,
;; the running command may be blocked.

(defun close-fd (fd)
  (handler-case
      ;; this is necessary since the lisp process may still open the fd
      ;; In such a case, it should be closed, or the process may
      ;; never finish
      (iolib.syscalls:close fd)
    ;; but it might not be the case also, and the fd is already closed
    ;; and the process has already died, returning ebadf
    (iolib.syscalls:ebadf ())
    ;; this is not handled
    #+nil
    (iolib.syscalls:eintr ())
    ;; this is not handled
    #+nil
    (iolib.syscalls:eio ())))

(defun io-loop-string (p in s0 s1 s2)
  (iter
    (with out = (make-string-output-stream))
    (with err = (make-string-output-stream))
    (with exitstatus = nil)
    (try-read-write-char in s0  (fd p 0))
    (try-read-write-char s1 out (fd p 1))
    (try-read-write-char s2 err (fd p 2))
    (unless exitstatus
      (setf exitstatus (check-alive p)))
    (while (or (null exitstatus)
               (open-stream-p s0)
               (open-stream-p s1)
               (open-stream-p s2)))
    (finally (return (values (get-output-stream-string out)
                             (get-output-stream-string err)
                             exitstatus)))))

(defun try-read-write-char (sin sout fd)
  (handler-case
      (when (and (open-stream-p sin)
                 (open-stream-p sout))
        (when-let ((c (read-char-no-hang sin)))
          (write-char c sout)
          t))
    (end-of-file ()
      (close sin)
      (close sout)
      (close-fd fd)
      (values nil t))
    #+sbcl
    (sb-int:stream-decoding-error ()
      (close sin)
      (close sout)
      (close-fd fd)
      (values nil t))))

(defun check-alive (process)
  (match (handler-case (wait process :nohang)
           (iolib.syscalls:echild () '(echild))
           (iolib.syscalls:eintr () '(eintr))
           #+nil
           (iolib.syscalls:einval () '(einval)))
    (nil nil)
    ((list* _ exitstatus1)
     exitstatus1)))


