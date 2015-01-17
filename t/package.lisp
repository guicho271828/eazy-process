#|
  This file is a part of eazy-process project.
  Copyright (c) 2014 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :eazy-process.test
  (:use :cl
        :eazy-process
        :cl-ppcre
        :fiveam
        :iterate :alexandria :cffi :optima)
  (:shadow :fail)
  (:export
   #:trivial-shell
   #:test-1
   #:test-input
   #:test-space
   #:procfs
   #:io
   #:statm
   #:stat
   #:shell
   #:threads
   #:subprocesses
   #:pgid
   #:ppid
   #:pids))
(in-package :eazy-process.test)



(def-suite :eazy-process)
(in-suite :eazy-process)

;;; pid utilities

(test pids
  (finishes
    (format t "~& my pid : ~a" (getpid))))
(test ppid
  (finishes
    (format t "~& ppid : ~a" (ppid (getpid)))))
(test pgid
  (finishes
    (format t "~& gpid : ~a" (pgid (getpid)))))
(test subprocesses
  (finishes
    (format t "~& children : ~a" (subprocesses (getpid)))))
(test threads
  (finishes
    (format t "~& threads : ~a" (threads (getpid)))))

;;; shell

(test shell
  (finishes (wait (print (shell '("ls" "-la")))))
  (finishes (wait (print (shell '("sh" "-c" "echo subshell! ; ls -la"))))))

(test sleep-wait
  (finishes
    (let ((p1 (shell '("sleep" "2"))))
      (wait p1))))

(test read
  (let ((p1 (shell `("hostname"))))
    (with-open-file (s (fd-as-pathname p1 1))
      (is (string= (machine-instance)
                   (read-line s))))))

(test manual-pipe-and-gc
  (let (pid1 pid2)
    (trivial-garbage:gc :full t :verbose t)
    (finishes
      (let* ((p1 (shell `("ps" "-ef")))
             (p2 (shell `("cat")  (list (fd p1 1) :out :out))))
        (format t "~&process 1 : ~a , fd ~a" p1 (fds p1))
        (format t "~&process 2 : ~a , fd ~a" p2 (fds p2))
        (with-open-file (s (fd-as-pathname p2 1))
          (print (read-line s))
          (print (read-line s)))
        (setf pid1 (pid p1) pid2 (pid p2))))
    
    ;; (sleep 10)
    ;; ;; calls wait(2) on GC
    ;; (format t "~&GC ing")
    ;; (trivial-garbage:gc :full t :verbose t)
    ;; (sleep 10)
    ;; ;; because the process should be killed and waitpid'ed
    ;; (signals error
    ;;   ;; iolib.syscalls:syscall-error
    ;;   (iolib.syscalls:waitpid pid1 iolib/syscalls:WNOHANG))
    ;; (signals error
    ;;   ;;iolib.syscalls:syscall-error
    ;;   (iolib.syscalls:waitpid pid2 iolib/syscalls:WNOHANG))
    ))

(test environment
  (finishes
    (let ((p1 (shell `("sh" "-c" "echo $MYVAL")
                     +fdspecs-default+
                     (cons "MYVAL=2015" (environment)))))
      (with-open-file (s (fd-as-pathname p1 1))
        (is (= 2015 (read s)))))))

(test file
  (let ((p (shell '("cat") `((,(asdf:system-relative-pathname
                                :eazy-process "t/test-input") :direction :input)
                             :out :out))))
    (with-open-file (s (fd-as-pathname p 1))
      (is (string= "guicho" (read-line s))))))

;;; posix procfs

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
    (proc :self :fd)))

(test io (test-subfields #'io +io-keywords+))
(test statm (test-subfields #'statm +statm-keywords+))
(test stat (test-subfields #'stat (remove :state (remove :comm +stat-keywords+))))

;;; trivial-shell

;; notice the newline!
(test trivial-shell
  (is (string= "10
" (shell-command "expr 1 + 2 + 3 + 4")))
  ;; bash-specific stuff
  (let ((*interpreter* "bash -c"))
    (is (string= "1 2 3
" (shell-command "echo {1..3}"))))
  (let ((*interpreter* "perl -e"))
    (is (string= "4" (shell-command "print(1+3)")))))

;;;; tests below are imported from trivial-shell and converted to fiveam

(test test-1
  (is (= 2 (parse-integer (shell-command "expr 1 + 1") :junk-allowed t))))
(test test-input
  (is (= 5 (parse-integer
            (shell-command "wc -c" :input "hello")
            :junk-allowed t))))

