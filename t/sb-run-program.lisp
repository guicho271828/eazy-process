(in-package :eazy-process.test)
(in-suite :eazy-process)

#|

Tests derived from run-program.impure.lisp in sbcl.
Half Abandoned...

|#

;;; sbcl run-program

(defmacro with-test ((&key name &allow-other-keys) &body body)
  `(test ,(cond
           ((symbolp name) name)
           ((listp name) (reduce #'symbolicate name)))
     ,@body))

(with-test (:name :run-program-cat-1 :skipped-on :win32)
  (let* ((process (run-program "/bin/cat" '() :wait nil
                                      :output :stream :input :stream))
         (out (process-input process))
         (in (process-output process)))
    (unwind-protect
         (loop for i from 0 to 255 do
              (write-byte i out)
              (force-output out)
              (assert (= (read-byte in) i)))
      (process-close process))))

;; skipped: ;; Tests that reading from a FIFO is interruptible.
;; skipped: ;; User-defined binary input and output streams.

(with-test (:name :run-program-cat-4 :skipped-on :win32)
  ;; Null broadcast stream as output
  (let* ((process (run-program "/bin/cat" '() :wait nil
                                      :output (make-broadcast-stream)
                                      :input :stream))
         (in (process-input process)))
    (unwind-protect
         (progn
           (write-string "foobar" in)
           (close in)
           (process-wait process))
      (process-close process))))

;; (progn
;;   (defun make-pipe ()
;;     (multiple-value-bind (in out) (iolib.syscalls:pipe)
;;       (make-two-way-stream
;;        (open (fd-as-pathname (getpid) in))
;;        (open (fd-as-pathname (getpid) out) :direction :output :if-exists :overwrite)))
;;     ;; (multiple-value-bind (in out) (sb-posix:pipe)
;;     ;;   (let ((input (sb-sys:make-fd-stream in
;;     ;;                                       :input t
;;     ;;                                       :external-format :ascii
;;     ;;                                       :buffering :none :name "in"))
;;     ;;         (output (sb-sys:make-fd-stream out
;;     ;;                                        :output t
;;     ;;                                        :external-format :ascii
;;     ;;                                        :buffering :none :name "out")))
;;     ;;     (make-two-way-stream input output)))
;;     )
;;   (defparameter *cat-in-pipe* (make-pipe))
;;   (defparameter *cat-in* (make-synonym-stream '*cat-in-pipe*))
;;   (defparameter *cat-out-pipe* (make-pipe))
;;   (defparameter *cat-out* (make-synonym-stream '*cat-out-pipe*)))
;; 
;; (with-test (:name :run-program-cat-5 :fails-on :win32)
;;   (let ((cat (run-program "/bin/cat" nil :input *cat-in* :output *cat-out*
;;                           :wait nil)))
;;     (dolist (test '("This is a test!"
;;                     "This is another test!"
;;                     "This is the last test...."))
;;       (write-line test *cat-in*)
;;       (assert (equal test (read-line *cat-out*))))
;;     (process-close cat)))

;; Around 1.0.12 there was a regression when :INPUT or :OUTPUT was a
;; pathname designator.  Since these use the same code, it should
;; suffice to test just :INPUT.
(test sbcl-regression
  (let ((file))
    (unwind-protect
         (progn (with-open-file (f "run-program-test.tmp" :direction :output)
                  (setf file (truename f))
                  (write-line "Foo" f))
                (assert (run-program "cat" ()
                                     :input file :output t
                                     :search t :wait t)))
      (when file
        (delete-file file)))))

;; This used to crash on Darwin and trigger recursive lock errors on
;; every platform.
(with-test (:name (:run-program :stress) :fails-on :win32)
  ;; Do it a hundred times in batches of 10 so that with a low limit
  ;; of the number of processes the test can have a chance to pass.
  (loop
   repeat 10 do
   (map nil
        #'process-wait
        (loop repeat 10
              collect
              (run-program "/bin/echo" '
                           ("It would be nice if this didn't crash.")
                           :wait nil :output nil)))))

;; Check whether RUN-PROGRAM puts its child process into the foreground
;; when stdin is inherited. If it fails to do so we will receive a SIGTTIN.
;;
;; We can't check for the signal itself since run-program.c resets the
;; forked process' signal mask to defaults. But the default is `stop'
;; of which we can be notified asynchronously by providing a status hook.
(with-test (:name (:run-program :inherit-stdin) :fails-on :win32)
  (let (stopped)
    (flet ((status-hook (proc)
             (case (process-status proc)
               (:stopped (setf stopped t)))))
      (let ((proc (run-program "/bin/ed" nil :search nil :wait nil
                               :input t :output t
                               :status-hook #'status-hook)))
        ;; Give the program a generous time to generate the SIGTTIN.
        ;; If it hasn't done so after that time we can consider it
        ;; to be working (i.e. waiting for input without generating SIGTTIN).
        (sleep 0.5)
        ;; either way we have to signal it to terminate
        (process-kill proc sb-posix:sigterm)
        (process-close proc)
        (assert (not stopped))))))

;; skipped:
;; Check that in when you do run-program with :wait t that causes
;; encoding error, it does not affect the following run-program

(with-test (:name (:run-program :no-such-thing))
  (assert (search "Couldn't execute"
                  (handler-case
                      (progn (run-program "no-such-program-we-hope" '()) nil)
                    (error (e)
                      (princ-to-string e))))))

(with-test (:name (:run-program :set-directory))
  (let* ((directory #-win32 "/"
                    #+win32 "c:\\")
         (out (sb-ext:process-output
               (sb-ext:run-program #-win32 "/bin/sh"
                                   #-win32 '("-c" "pwd")
                                   #+win32 "cmd.exe"
                                   #+win32 '("/c" "cd")
                                   :output :stream
                                   :directory directory
                                   :search t))))
    (assert
     (equal directory
            (string-right-trim '(#\Return) (read-line out))))))

(with-test (:name (:run-program :directory-nil))
  (sb-ext:run-program #-win32 "/bin/sh"
                      #-win32 '("-c" "pwd")
                      #+win32 "cmd.exe"
                      #+win32 '("/c" "cd")
                      :directory nil
                      :search t))
