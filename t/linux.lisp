
#|

this file is not actually used.

|#


(in-package :eazy-process.test)
(in-suite :eazy-process)

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
