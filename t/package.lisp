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

(defun test-subfields (fn fields)
  (finishes (print (funcall fn :self)))
  (iter (for f in-sequence fields)
        (is-true (typep (funcall fn :self f) 'integer)
                 "~a not integer" f)
        (is-true (typep (funcall fn :self (princ-to-string f)) 'integer)
                 "~a not integer" f)
        (is-true (typep (funcall fn :self (let ((*print-case* :downcase))
                                            (princ-to-string f))) 'integer)
                 "~a not integer" f))
  (signals error (funcall fn :self :nosuchfield))
  (signals error (funcall fn :self "nosuchfield"))
  (signals error (funcall fn :self "NOSUCHFIELD")))

(test procfs
  (finishes
    (proc :self :fdinfo)
    (proc :self :fd)
    (fd :self 0)
    (fd :self 1)
    (fd :self 2))
  (multiple-value-bind (path exists?) (fd :self 5)
    (is-false exists?)))

(test io (test-subfields #'io +io-keywords+))
(test statm (test-subfields #'statm +statm-keywords+))
(test stat (test-subfields #'stat (remove :state (remove :comm +stat-keywords+))))
