#|
  This file is a part of eazy-process project.
  Copyright (c) 2014 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :eazy-process.test
  (:use :cl
        :eazy-process.impl
        :cl-ppcre
        :fiveam
        :iterate :alexandria :cffi :optima)
  (:shadow :fail))
(in-package :eazy-process.test)



(def-suite :eazy-process)
(in-suite :eazy-process)

;; run test with (run! test-name) 
;;   test as you like ...

(test shell-command
  (finishes
    (shell "ls" "-la")
    (shell "sh" "-c" "echo subshell! ; ls -la")))


