(in-package :eazy-process.impl)

#|

*  inferior-shell compatibility

Goal: sexp -> bash compiler in inferior-shell

trivial shell has the following interace

run
run/s
run/lines

|#

(defparameter *interpreter* "sh -c"
  "Command line string which is invoked in order to run the subshell. Default value is \"sh -c\".
The value is then followed by the command specified in shell-command, e.g. ,
when command = 'ls -la', the running command is \"sh -c 'ls -la'\".
It provides the ability to call the interpreters like perl/AWK easily.

The name/path of the interpreter must be separated by spaces from the options like '-c'.
The process is forked, then the child process calls execvp with the name of the interpreter.
Therefore, the actual pathname of the intepreter can be resolved using PATH environment variable.
")
