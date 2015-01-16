(in-package :eazy-process.impl)

#|

*  sbcl run-program compatibility

Goal: 


 (run-program PROGRAM ARGS
  &KEY
  (ENV NIL) -- specify the environment
  (ENVIRONMENT ...)  -- specify the environment, inherited by the lisp process.
  (WAIT T)
  SEARCH                   --- ignored. alsways search the path.
  PTY
  INPUT
  IF-INPUT-DOES-NOT-EXIST
  OUTPUT
  (IF-OUTPUT-EXISTS ERROR)
  (ERROR OUTPUT)
  (IF-ERROR-EXISTS ERROR)
  STATUS-HOOK
  (EXTERNAL-FORMAT DEFAULT)
  DIRECTORY)

The difference is that it assumes the input/output is a TTY.

|#

