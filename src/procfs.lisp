

(in-package :eazy-process.impl)

#|

procfs interfaces based on libproc.

In this file, we always treat directory pathnames without the trailing slash.

|#

(define-foreign-library libprocps
  (:unix "libprocps.so")
  (t (:default "libprocps")))

(use-foreign-library libprocps)


