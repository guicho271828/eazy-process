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

(defun read-all-chars (s)
  (iter (while (listen s)
          ;; bugfix --- fails to peek-char when the full data is not available
          ;; Unhandled sb-int:stream-decoding-error in thread #<sb-thread:thread
          ;;                                                    "main thread" running
          ;;                                                     {1002C4EC23}>:
          ;;   :utf-8 stream decoding error on
          ;;   #<sb-sys:fd-stream for "file /proc/2357/fd/10" {1002CFF593}>:
          ;;     the octet sequence #(181) cannot be decoded.
          #+nil
          (peek-char nil s nil nil))
        (collect (read-char s nil nil) result-type string)))

(defmacro with-retry-open-file ((max tag) args &body body)
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
               (sleep 0.01)
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
                                input
                                (external-format :default)
                                verbose)
  "simple interface compatible to trivial-shell @
https://github.com/gwkkwg/trivial-shell.git.

returns (values output error-output exit-status).
The input is read from the :input key argument.
"
  (let* ((command (if (pathnamep command)
                      (namestring command)
                      command))
         (argv (append (ppcre:split "[ \t]+" *interpreter*)
                       (list command))))
    (when verbose
      (format *trace-output* "~&; ~a '~a'" *interpreter* command))
    (with-process (p argv)
      ;; input
      (when input
        (with-retry-open-file (100 :start) 
          (s (fd-as-pathname p 0)
             :direction :output
             :if-exists :overwrite)
          (etypecase input
            (stream
             (handler-case
                 (loop (write-char (read-char input) s))
               (end-of-file (c)
                 (declare (ignore c)))))
            (sequence
             (write-sequence input s)))))
      (ensure-closed (fd p 0))
      ;; now, read the output
      (multiple-value-bind (out err status)
          (with-retry-open-file (100 :start1)
            (s1 (fd-as-pathname p 1) :external-format external-format)
            (with-retry-open-file (100 :start2)
              (s2 (fd-as-pathname p 2) :external-format external-format)
              (loop-impl4 p s1 s2)))
        (values (coerce out 'string)
                (coerce err 'string)
                status)))))

(defun ensure-closed (fd)
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

(defun loop-impl2 (p s1 s2)
  "busy-waiting. works but inefficient"
  (iter
    outer
    (iter (match (read-char-no-hang s1 nil)
            ((and c (type character))
             (in outer (collect c result-type string into out)))
            (_ (leave))))
    (iter (match (read-char-no-hang s2 nil)
            ((and c (type character))
             (in outer (collect c result-type string into err)))
            (_ (leave))))
    (match (wait p :nohang)
      ((list* _ exitstatus)
       ;; ensure everything is read
       (iter (match (read-char-no-hang s1 nil)
               ((and c (type character))
                (in outer (collect c result-type string into out)))
               (_ (leave))))
       (iter (match (read-char-no-hang s2 nil)
               ((and c (type character))
                (in outer (collect c result-type string into err)))
               (_ (leave))))
       (leave
        (values (coerce out 'string)
                (coerce err 'string)
                exitstatus))))))

(defun loop-impl3 (p s1 s2)
  "wait and read, however it may stop when it reaches the buffer limit (4 Kbyte or so)"
  (match (wait p)
    ((list* _ exitstatus)
     (values (iter (while (listen s1))
                   (collect (read-char-no-hang s1 nil) result-type string))
             (iter (while (listen s2))
                   (collect (read-char-no-hang s2 nil) result-type string))
             exitstatus))))

(defun loop-impl4 (p s1 s2)
  "busy-waiting + 100ms sleep"
  (iter
    outer
    (sleep 1/100)
    (iter (match (read-char-no-hang s1 nil)
            ((and c (type character))
             (in outer (collect c result-type string into out)))
            (_ (leave))))
    (iter (match (read-char-no-hang s2 nil)
            ((and c (type character))
             (in outer (collect c result-type string into err)))
            (_ (leave))))
    (match (handler-case (wait p :nohang)
             (iolib.syscalls:echild () nil)
             (iolib.syscalls:eintr () nil)
             #+nil
             (iolib.syscalls:einval () nil))
      ((list* _ exitstatus)
       ;; ensure everything is read
       (iter (match (read-char-no-hang s1 nil)
               ((and c (type character))
                (in outer (collect c result-type string into out)))
               (_ (leave))))
       (iter (match (read-char-no-hang s2 nil)
               ((and c (type character))
                (in outer (collect c result-type string into err)))
               (_ (leave))))
       (leave
        (values out err exitstatus))))))

