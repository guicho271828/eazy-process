#|
  This file is a part of eazy-process project.
  Copyright (c) 2014 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :eazy-process
  (:use :cl :iterate :alexandria :optima :cl-ppcre :cffi)
  (:import-from :trivial-garbage :gc :finalize)
  (:import-from :iolib/syscalls
                :getpid :getppid :getpgid
                :fork :execvp
                ;; :exit
                :pipe :dup2
                :kill :waitpid)
  (:export
   #:shell
   #:%exec
   #:process
   #:getpid
   #:pid
   #:getppid
   #:ppid
   #:getpgid
   #:pgid
   #:wait
   ;; procfs
   #:io
   #:fd
   #:proc
   #:statm
   #:stat
   #:+stat-keywords+
   #:+statm-keywords+
   #:+io-keywords+
   #:+fdspecs-default+
   #:fd-as-pathname
   #:fds
   #:subprocesses
   #:all-processes
   #:threads
   ;; trivial-shell
   #:shell-command
   #:*interpreter*))
