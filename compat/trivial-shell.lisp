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
  (iter (while (peek-char nil s nil nil))
        (collect (read-char s nil nil) result-type string)))

(defun shell-command (command &key (input ""))
  "simple interface compatible to trivial-shell @
https://github.com/gwkkwg/trivial-shell.git.

returns (values output error-output exit-status).
The input is read from the :input key argument.
"
  (let ((command (if (pathnamep command)
                     (namestring command)
                     command)))
    (format t "~&; ~a '~a'" *interpreter* command)
    (let (p)
      (unwind-protect-case ()
          (progn
            (setf p (shell (append (ppcre:split "[ \t]+" *interpreter*) (list command))))
            (with-open-file (s (fd-as-pathname p 0)
                               :direction :output
                               :if-exists :overwrite)
              (write-sequence input s))
            (iolib.syscalls:close (fd p 0))
            (return-from shell-command
              (values (with-open-file (s (fd-as-pathname p 1)) (read-all-chars s))
                      (with-open-file (s (fd-as-pathname p 2)) (read-all-chars s))
                      (nth-value 1 (wait p)))))
        (:abort (finalize-process p))))))

