(in-package :eazy-process)

#|

*  trivial-shell compatibility (almost)

Goal: simple exec/fork interface compatible to trivial-shell except
timeout-related stuff

https://github.com/gwkkwg/trivial-shell.git

trivial shell has the following interace

trivial-shell:*BOURNE-COMPATIBLE-SHELL* 	trivial-shell:*SHELL-SEARCH-PATHS*
trivial-shell:EXIT 	trivial-shell:GET-ENV-VAR
trivial-shell:SHELL-COMMAND 	trivial-shell:TIMEOUT-ERROR
trivial-shell:TIMEOUT-ERROR-COMMAND 	trivial-shell:WITH-TIMEOUT

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
  (format t "; ~a '~a'" *interpreter* command)
  (apply #'shell (split "[ \t]+" *interpreter*) command))
