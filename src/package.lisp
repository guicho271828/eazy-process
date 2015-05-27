#|
  This file is a part of eazy-process project.
  Copyright (c) 2014 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :eazy-process
  (:use :cl :iterate :alexandria
        :optima :optima.ppcre
        :cffi
        :cl-rlimit)
  (:import-from :iolib.syscalls
                :getpid :getppid :getpgid
                :fork
                :execv ; :execve
                :execvp ; :execvpe
                ;; :exit
                :mkfifo :mkdtemp
                :dup2
                :kill :waitpid)
  (:export
   #:shell
   #:process
   #:getpid
   #:pid
   #:wait
   #:fd
   #:+fdspecs-default+
   #:fd-as-pathname
   #:fds
   #:pipe
   #:*rlimit-resources*
   #:with-rlimit
   #:close-pipe
   ;;;; proc file system
   ;; #:io
   ;; #:proc
   ;; #:statm
   ;; #:stat
   ;; #:+stat-keywords+
   ;; #:+statm-keywords+
   ;; #:+io-keywords+
   ;; #:subprocesses
   ;; #:all-processes
   ;; #:threads
   ;; #:getppid
   ;; #:ppid
   ;; #:getpgid
   ;; #:pgid
   ;;;; trivial-shell
   #:shell-command
   #:*interpreter*
   #:finalize-process
   #:environment
   ;;;; sbcl run-program
   ;; #:run-program
   ;; #:process-kill
   ;; #:process-wait
   ;; #:process-input
   ;; #:process-output
   ;; #:process-error
   #:with-process
   #:*bourne-compatible-shell*))
