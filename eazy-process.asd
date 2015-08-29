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
                        :optima.ppcre
                        :iolib/syscalls
                        :trivial-garbage
                        :cl-ppcre
                        :cl-rlimit
                        :exponential-backoff)
  :components ((:module :src
                :serial t
                :components
                ((:file :package)
                 (:file :mktempfifo)
                 (:file :pipe)
                 (:file :environ)
                 (:file :specials)
                 (:file :fdspec)
                 (:file :shell)
                 (:file :process)
                 (:file :macros)
                 (:file :resources)
                 #+nil
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
  :description "Yet Another Portable Library for Process Handling / Subshell Invokation"
  :long-description "In `run-program` interface in the popular
  implementations, piping between subprocesses are hard. It requires either
  reading the entire output stream and packing the contents as a new
  string-input-stream, or using some other implementation-specific
  functions. Also, compatibility libraries e.g.  trivial-shell or
  inferior-shell, often depend on these functions, implying the same
  problem. Iolib also has `run-program` that allows easy piping, but it is
  restricted to 3 fds: `input,output,error`.

  Eazy-process provides a clean, declarative and thin layer to the
  processes. It depends on the concept of 'everything is a file' and do not
  provide interfaces to streams."
  :in-order-to ((test-op (load-op eazy-process.test))))
