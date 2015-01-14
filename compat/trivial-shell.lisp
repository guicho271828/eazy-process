(in-package :eazy-process.impl)

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
