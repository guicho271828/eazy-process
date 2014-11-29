#|
  This file is a part of eazy-process project.
  Copyright (c) 2014 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage eazy-process-asd
  (:use :cl :asdf))
(in-package :eazy-process-asd)


(defsystem eazy-process
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:iterate :alexandria :cffi :optima :trivial-shell
                        :cl-syntax-interpol
                        :cl-syntax-annot
                        :cl-ppcre
                        :cl-rlimit)
  :components ((:module :swig
                :serial t
                :components
                ((:file :package)
                 (:file :signal)
                 (:file :unistd)))
               (:module "src"
                :serial t
                :components
                ((:file "package")
                 (:cffi-grovel-file :stdio))))
  :description ""
  :in-order-to ((test-op (load-op eazy-process.test))))