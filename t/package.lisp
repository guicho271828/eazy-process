
;;; This file is a part of eazy-process project.
;;; Copyright (c) 2014 Masataro Asai (guicho2.71828@gmail.com)

(in-package :cl-user)
(defpackage :eazy-process.test
  (:use :cl
        :eazy-process
        :cl-ppcre
        :fiveam
        :iterate :alexandria :cffi :optima :cl-rlimit)
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

(defun localpath (str)
  (asdf:system-relative-pathname :eazy-process str))

(def-suite :eazy-process)
(in-suite :eazy-process)

;;; pid utilities

(test pids
  (finishes
    (format t "~& my pid : ~a" (getpid))))

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

(test devnull
  (shell `("hostname") '(nil nil)))


(test implicit-pipe
  (let (pid1 pid2)
    (finishes
      (let* ((p1 (shell `("ps" "-ef")))
             (p2 (shell `("cat")  (list (fd p1 1) :out :out))))
        (format t "~&process 1 : ~a , fd ~a" p1 (fds p1))
        (format t "~&process 2 : ~a , fd ~a" p2 (fds p2))
        (with-open-file (s (fd-as-pathname p2 1))
          (print (read-line s))
          (print (read-line s)))
        (setf pid1 (pid p1) pid2 (pid p2))))))
    
#+nil
(progn
  (defvar *pid*)
  (test zombie
    ;; not waited
    (setf *pid* (pid (shell `("uname")))))

  (test (gc :depends-on zombie)  
    (format t "~&GC ing")
    ;; GC should invoke the finalizer, which in tern calls kill(2) and wait(2)
    (trivial-garbage:gc :full t :verbose t)
    (sleep 10)
    ;; because the process should be already killed and waitpid'ed
    (signals iolib.syscalls:syscall-error
      ;; iolib.syscalls:syscall-error
      (iolib.syscalls:waitpid *pid* 0))))

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

(defun ensure-missing (file)
  (when (probe-file file) (delete-file file)))

(defmacro with-ensure-missing-file ((file) &body body)
  (once-only (file)
    `(unwind-protect
          (progn (ensure-missing ,file)
                 ,@body)
       (ensure-missing ,file))))

(defmacro with-ensure-missing-files (files &body body)
  (if files
      `(with-ensure-missing-file (,(car files))
         (with-ensure-missing-files ,(cdr files)
           ,@body))
      `(progn ,@body)))

(test explicit-pipe
  (let* ((pipe (pipe))
         (in (localpath "t/test-input"))
         (out (localpath "t/test-output"))
         (err (localpath "t/test-error")))
    (with-ensure-missing-files (err out)
      (let ((p1 (shell '("cat") `((,in :direction :input)
                                  (,pipe :direction :output)
                                  :out)))
            (p2 (shell '("cat") `((,pipe :direction :input)
                                  (,out :direction :output
                                        :if-does-not-exist :create)
                                  (,err :direction :output
                                        :if-does-not-exist :create)))))
        (close-pipe pipe)
        ;; since lisp process does not use this pipe,
        ;; it should close both fds, or p2 does not terminate
        (print :waiting-p1)
        (print (multiple-value-list (wait p1)))
        (print :waiting-p2)
        (print (multiple-value-list (wait p2)))
        (is (probe-file out))
        (is (probe-file err))
        (with-open-file (s out)
          (is (string= "guicho" (read-line s))))
        (with-open-file (s err)
          (signals error
            (read-char s))))))) ; because it should write nothing to the error output

(test explicit-pipe-nodir
  (let* ((pipe (pipe))
         (in (localpath "t/test-input"))
         (out (localpath "t/test-output"))
         (err (localpath "t/test-error")))
    (with-ensure-missing-files (err out)
      (let ((p1 (shell '("cat") `(,in ,pipe)))
            (p2 (shell '("cat") `(,pipe ,out ,err))))
        (close-pipe pipe)
        ;; since lisp process does not use this pipe,
        ;; it should close both fds, or p2 does not terminate
        (print :waiting-p1)
        (print (multiple-value-list (wait p1)))
        (print :waiting-p2)
        (print (multiple-value-list (wait p2)))
        (is (probe-file out))
        (is (probe-file err))
        (with-open-file (s out)
          (is (string= "guicho" (read-line s))))
        (with-open-file (s err)
          (signals error
            (read-char s)))))))

#+nil
(test tee
    (let* ((in (localpath "t/test-input"))
           (out (localpath "t/test-output"))
           (tee (localpath "t/test-tee")))
      (is (probe-file out))
      (with-open-file (s out)
        (string= "guicho" (read-line s)))
      (when (probe-file out)
        (delete-file out))

      (is (probe-file tee))
      (with-open-file (s tee)
        (string= "guicho" (read-line s)))
      (when (probe-file tee)
        (delete-file tee))))
;; seq 3 | cat 1>&2 | cat > out
;; the result will not be written into `out' ... but what does the second cat read from?
;; Probably 1>&2 is implemented with `dup', not `dup2'. So the second cat is reading from
;; an empty pipe.

;;; trivial-shell

;; notice the newline!
(test trivial-shell
  (is (= 10 (read-from-string
             (shell-command "expr 1 + 2 + 3 + 4"))))
  (is (= 2 (read-from-string
            (nth-value 1 (shell-command "echo 2 >&2")))))
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



;;; resource

(defvar *testdir* (localpath "t"))
(defvar *exit0* (localpath "t/exit0"))
(defvar *exit1* (localpath "t/exit1"))
(defvar *malloc* (localpath "t/malloc"))
(defvar *spendtime* (localpath "t/spendtime"))
(defun make ()
  (print (shell-command (format nil "make -C ~a" *testdir*)))
  (is-true (probe-file *malloc*))
  (is-true (probe-file *spendtime*)))

(test exit
  (with-ensure-missing-files (*exit0* *exit1*)
    (make)
    (finishes (print (multiple-value-list (shell-command *exit0*))))
    (finishes (print (multiple-value-list (shell-command *exit1*))))
    (let ((p (shell `(,(namestring *exit0*)))))
      (destructuring-bind (exited exitstatus &rest args) 
          (print (multiple-value-list (wait p)))
        (is-true exited)
        (is (= 0 exitstatus))))
    (let ((p (shell `(,(namestring *exit1*)))))
      (destructuring-bind (exited exitstatus &rest args) 
          (print (multiple-value-list (wait p)))
        (is-true exited)
        (is (= 1 exitstatus))))))

(test malloc
  (with-ensure-missing-files (*malloc*)
    (make)
    (finishes (print (multiple-value-list (shell-command *malloc*))))
    (let ((p (shell `(,(namestring *malloc*))))) ;; allocate 1GB
      (destructuring-bind (exited exitstatus &rest args) 
          (print (multiple-value-list (wait p)))
        (is-true exited)
        (is (= 0 exitstatus))))))

(test malloc-limit
  (with-ensure-missing-files (*malloc*)
    (make)
    (with-rlimit ((+rlimit-address-space+ 500000000)) ; 500MB
      ;; with-rlimit does not affect the lisp process itself;
      ;; The effect is applied to the child process only
      (finishes (print (multiple-value-list (shell-command *malloc*))))
      (let ((p (shell `(,(namestring *malloc*))))) ;; allocate 1GB
        (destructuring-bind (exited exitstatus &rest args) 
            (print (multiple-value-list (wait p)))
          (is-true exited)
          (is (= 1 exitstatus)))))))

(test spendtime
  ;; (sleep 15)
  (with-ensure-missing-files (*spendtime*)
    (make)
    (finishes (print (shell-command *spendtime*)))
    (let ((p (shell `(,(namestring *spendtime*))))) ; busy wait
      (destructuring-bind (exited exitstatus ifsignalled termsig &rest args) 
          (print (multiple-value-list (wait p)))
        (is-true exited)
        (is (= 0 exitstatus))
        (is-false ifsignalled)
        (is-false termsig)))))

(test spendtime-limit
  ;; (sleep 15)
  (with-ensure-missing-files (*spendtime*)
    (make)
    (with-rlimit ((+rlimit-cpu-time+ 1)) ; sec
      (finishes (print (shell-command *spendtime*)))
      (let ((p (shell `(,(namestring *spendtime*)))))
        (destructuring-bind (exited exitstatus ifsignalled termsig &rest args) 
            (print (multiple-value-list (wait p)))
          (is-false exited)
          (is-false exitstatus)
          (is-true ifsignalled)
          (is (= 24 termsig)))))))

