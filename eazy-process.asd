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
  :licence "MIT"
  :depends-on (:iterate :alexandria
                        :cffi
                        :optima
                        :iolib/syscalls
                        :iolib/os
                        :trivial-garbage
                        ;; :cl-syntax-interpol
                        ;; :cl-syntax-annot
                        :cl-ppcre
                        :cl-rlimit)
  :components ((:module :src
                :serial t
                :components
                ((:file :package)
                 (:file :execv)
                 (:file :mktempfifo)
                 (:file :pipe)
                 (:file :environ)
                 (:file :specials)
                 (:file :shell)
                 (:file :process)
                 (:file :resources)
                 #+linux
                 (:module :linux
                          :serial t
                          :components
                          ((:file :specials)
                           (:file :process)
                           (:file :procfs)))))
               (:module :compat
                :serial t
                :components
                ((:file :trivial-shell)
                 ;; (:file :inferior-shell)
                 ;; (:file :sb-run-program)
                 )))
  :serial t
  :description ""
  :in-order-to ((test-op (load-op eazy-process.test))))
