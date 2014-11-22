#|
  This file is a part of eazy-process project.
  Copyright (c) 2014 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :eazy-process.test
  (:use :cl
        :eazy-process
        :fiveam
        :iterate :alexandria :cffi :optima))
(in-package :eazy-process.test)



(def-suite :eazy-process)
(in-suite :eazy-process)

;; run test with (run! test-name) 
;;   test as you like ...

(test eazy-process

  )


