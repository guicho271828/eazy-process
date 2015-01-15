#|
  This file is a part of eazy-process project.
  Copyright (c) 2014 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :eazy-process.impl
  (:use :cl :iterate :alexandria :optima :cl-ppcre :cffi)
  (:import-from :iolib/syscalls
                :getpid
                :fork :execvp
                ;; :exit
                :kill :waitpid)
  (:export
   #:shell
   #:%exec
   #:*interpreter*
   #:process
   #:getpid
   #:pid
   #:wait
   #:io
   #:fd
   #:proc
   #:statm
   #:stat
   #:+stat-keywords+
   #:+statm-keywords+
   #:+io-keywords+))

  
