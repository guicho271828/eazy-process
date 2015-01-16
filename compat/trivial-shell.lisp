(in-package :eazy-process)

#|

*  trivial-shell compatibility (almost)

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

(defun shell-command (command)
  "simple interface compatible to trivial-shell @
https://github.com/gwkkwg/trivial-shell.git."
  (format t "~&; ~a '~a'" *interpreter* command)
  (let (p)
    (unwind-protect
         (progn
           (setf p (shell (append (split "[ \t]+" *interpreter*) (list command))))
           (with-open-file (s (fd-as-pathname p 1))
             (iter (while (peek-char nil s nil nil))
                   (collect (read-char s nil nil) result-type string))))
      (when p
        (wait p)))))

