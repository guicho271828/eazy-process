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
  (:shadow :fail))
(in-package :eazy-process.test)



(def-suite :eazy-process)
(in-suite :eazy-process)

;; run test with (run! test-name) 
;;   test as you like ...

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
(test tasks
  (finishes
    (format t "~& threads : ~a" (tasks (getpid)))))

(test shell
  (finishes (wait (print (shell '("ls" "-la")))))
  (finishes (wait (print (shell '("sh" "-c" "echo subshell! ; ls -la")))))
  (finishes
    (let ((p1 (shell '("sleep" "2"))))
      (print :test-sleep)
      (print p1)
      (print (fd-as-pathname p1 1))
      (wait p1)))
  (finishes
    (let ((p1 (shell `("uname"))))
      (print :test-uname)
      (print p1)
      (print (fd-as-pathname p1 1))
      (with-open-file (s (fd-as-pathname p1 1))
        (print (read-line s)))
      (wait p1)))
  (finishes
    (let (p1 p2)
      (unwind-protect
           (progn
             (print :test-pipe)             

             (setf p1 (shell `("ps" "-ef")))
             (format t "~&process 1 : ~a , fd ~a" p1 (fds p1))

             (setf p2 (shell `("cat")  (list (fd p1 1) :out :out)))
             (format t "~&process 2 : ~a , fd ~a" p2 (fds p2))

             (with-open-file (s (fd-as-pathname p2 1))
               (print (read-line s))
               (print (read-line s))))
        (print :waiting-p1)
        (wait p1)
        (print :waiting-p2)
        (wait p2)))))

;; uname -a | cut -d' ' -f 1


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
