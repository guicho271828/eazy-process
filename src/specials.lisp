

(in-package :eazy-process)

#|

Define some special variables

|#

(defparameter +fdspecs-default+ '(:input :output :output)
  "fd-specifier is one of 6 symbols
:i, :in, :input, :o,:out,:output, or an integer fd.
The first 3 and the last 3 symbols are the synonyms.")

;; (defparameter *rlimit-resources* nil
;;   "An alist of (cl-rlimit:+rlimit-XXX+ . value).
;; This value does not affect the lisp process itself; The effect is applied to the child process only.
;; Rather than modifying this variable directly, use `with-rlimit' instead.")
;; 
;; ;;  <--- could be possible.

(defparameter +max-rlimit-constant+
  #.(max +rlimit-address-space+ 	+rlimit-as+
         +rlimit-core+ 	+rlimit-cpu+
         +rlimit-cpu-time+ 	+rlimit-data+
         +rlimit-file-size+ 	+rlimit-fsize+
         +rlimit-memlock+ 	+rlimit-msgqueue+
         +rlimit-nice+ 	+rlimit-nofile+
         +rlimit-nproc+ 	+rlimit-number-of-files+
         +rlimit-number-of-processes+ 	+rlimit-ofile+
         +rlimit-real-time-priority+ 	+rlimit-rss+
         +rlimit-rtprio+ 	+rlimit-sigpending+
         +rlimit-stack+))

(defparameter *rlimit-resources*
  (make-array (1+ +max-rlimit-constant+)
              :element-type 'fixnum
              :initial-element -1)
  "A vector of rlimit resource value, where the index suggests +rlimit-XXX+, which is an integer (usually below 16)
This value does not affect the lisp process itself; The effect is applied to the child process only.
Rather than modifying this variable directly, use `with-rlimit' instead.")

