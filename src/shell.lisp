;;; shell.lisp

(in-package :eazy-process)

#|

subshell implemented with fork-execvp

|#
;;; preprocessing

(defun dclose (old new)
  (dup2 old new)
  (unless (= old new) (isys:close old)))

(defun canonicalize-fdspec (fdspec)
  "Take an fd-specifier and return a cons of (parent-fn . child-fn) where
both parent-fn and child-fn are closures of 1 arg, the fd on the child end.
Each function runs a job that should be done in the context of
parent/child process.

Parent-fn should return the fd of the parent-end."
  (ematch fdspec
    ;; implicit pipes
    ((or :i :in :input)
     (multiple-value-bind (read write) (isys:pipe)
       (cons (lambda (i) (isys:close read) write)
             (lambda (i)
               ;; note that if read==i then duplication does not occur
               (dclose read i)
               (isys:close write)))))
    ((or :o :out :output)
     (multiple-value-bind (read write) (isys:pipe)
       (cons (lambda (i) (isys:close write) read)
             (lambda (i)
               (dclose write i)
               (isys:close read)))))
    ((type fixnum)
     (warn "this feature is deprecated!")
     (cons (constantly nil)
           (lambda (i)
             (dclose fdspec i))))
    ((list* (and pipe (pipe)) options)
     (apply #'%pipe pipe options))
    ((list* (and path (type pathname)) options)
     (apply #'%open path options))))

(defun %pipe (pipe &key (direction :input))
  (ematch pipe
    ((pipe read write)
     (ecase direction
       (:input
        (cons (lambda (i) write)
              (lambda (i)
                (dclose read i)
                (isys:close write))))
       (:output 
        (cons (lambda (i) read)
              (lambda (i)
                (dclose write i)
                (isys:close read))))))))

(defun %open (path
              &key
                (direction :input)
                (if-exists :error)
                (if-does-not-exist
                 (multiple-value-match (values direction if-exists)
                   ((:input           _ )                                    :error)
                   ((_                (or :overwrite :append))               :error)
                   (((or :output :io) (and (not :overwrite) (not :append))) :create)
                   ((:probe           _ )                                       nil))))
  ;; if-does-not-exist---one of :error, :create, or nil.
  ;; The default is
  ;; :error if direction is :input or if-exists is :overwrite or :append
  ;; :create if direction is :output or :io, and if-exists is neither :overwrite nor :append;
  ;; or nil when direction is :probe. 
  (multiple-value-bind (input output mask) ;; partly copied from sbcl
      (ecase direction
        (:input  (values   t nil isys:o-rdonly))
        (:output (values nil   t isys:o-wronly))
        (:io     (values   t   t isys:o-rdwr))
        (:probe  (values   t nil isys:o-rdonly)))
    (declare (ignorable input output))
    (ecase if-exists
      ((:new-version :error nil)
       (setf mask (logior mask isys:o-excl)))
      ;; ((:rename :rename-and-delete)
      ;;  (setf mask (logior mask isys:o-creat)))
      ((:supersede)
       (setf mask (logior mask isys:o-trunc)))
      (:append
       (setf mask (logior mask isys:o-append)))
      ((:overwrite nil) nil))
    ;; returns a file descriptor
    (ecase if-does-not-exist
      (:create (setf mask (logior mask isys:o-creat)))
      (:error)
      (nil))
    (cons (constantly nil)
          (lambda (i)
            (dclose (isys:open (namestring path) mask) i)))))

;;; shell

(defun shell (argv &optional
                     (fdspecs '#.+fdspecs-default+)
                     (environments nil env-p)
                     (search t))
  (let ((fdspecs (mapcar #'canonicalize-fdspec fdspecs)))
    (let ((pid (fork)))
      (cond
        ;; ((= -1 pid) ;; this is already handled by iolib, so don't care
        ;;  (%failure command))
        ((zerop pid)
         (%in-child fdspecs argv environments env-p search))
        (t
         (%in-parent fdspecs pid))))))

(defun %in-parent (fdspecs pid)
  (%make-process
   pid
   (iter (for i from 0)
         (for (parent . child) in fdspecs)
         (collect (funcall parent i) result-type vector))))

(defun %in-child (fdspecs argv environments env-p search)
         (handler-case
             (progn
               (iter (for kind from 0)
                     (for value in-vector *rlimit-resources*)
                     (when (plusp value)
                       (setf (rlimit kind) value)))
               (iter (for i from 0)
                     (for (parent . child) in fdspecs)
                     (funcall child i))
               (%exec argv environments env-p search))
           (isys:syscall-error (c)
             (declare (ignorable c))
             (foreign-funcall "_exit" :int 203))
           (error (c)
             (declare (ignorable c))
             (foreign-funcall "_exit" :int 202))))

(defun %exec (argv env env-p search)
  (if search
      (if env-p
          (%execvpe argv env)
          (%execvp argv))
      (if env-p
          (%execve argv env)
          (%execv argv))))

;;; 4 versions of exec

(defun make-c-char* (list-of-string)
  (foreign-alloc :string
                 :initial-contents list-of-string
                 :null-terminated-p t))

(defun make-c-env-char* (list)
  (make-c-char*
   (mapcar (lambda (pair)
             (etypecase pair
               (cons (format nil "~a=~a" (car pair) (cdr pair)))
               (string pair)))
           list)))

(defun %execvpe (argv env)
  (let (_argv _env)
    (unwind-protect
         (progn
           (setf _argv (make-c-char* argv))
           (setf _env (make-c-env-char* env))
           (execvpe (first argv) _argv _env)) ; does not return on success
      (when _argv (foreign-free _argv))
      (when _env (foreign-free _env)))))

(defun %execve (argv env)
  (let (_argv _env)
    (unwind-protect
         (progn
           (setf _argv (make-c-char* argv))
           (setf _env (make-c-env-char* env))
           (execve (first argv) _argv _env)) ; does not return on success
      (when _argv (foreign-free _argv))
      (when _env (foreign-free _env)))))

(defun %execvp (argv)
  (let (_argv)
    (unwind-protect
         (progn
           (setf _argv (make-c-char* argv))
           (execvp (first argv) _argv)) ; does not return on success
      (when _argv (foreign-free _argv)))))

(defun %execv (argv)
  (let (_argv)
    (unwind-protect
         (progn
           (setf _argv (make-c-char* argv))
           (execv (first argv) _argv)) ; does not return on success
      (when _argv (foreign-free _argv)))))

;; Note:
;; IMPL> (cffi::canonicalize-foreign-type :string)
;; :POINTER

;; Note: allocated memory is automatically freed and get reclaimed by the
;; OS when exec is called successfully, because the data segment = heap is replaced.


;;; documentation

(setf (documentation 'shell 'function)
      "Asynchronously execute `argv' using fork(2) and `execve'
 family of system calls, returns a process structure object.

ARGV is a sequence of strings. Each string is converted to null-terminated
C strings char* ARGV and passed to execvp.  The first element is also
passed to execvp as the pathname of the executable.

FDSPECS is a sequence specifying how the child process should handle its output.
For the documentation of fd-specifier see `+fdspecs-default+'.
Example: (:in :out :out) , (5 :out 8)

Each element in FDSPECS corresponds to each file descriptor of the child process.

When an i-th element of FDSPECS is a symbol, then a new pipe(2) is created for each i.
The i-th fd of the child process p1 is associated with the correct end of the new pipe,
the other end of the pipe is readable/writable from lisp from the process interface.
 (see the documentation of class PROCESS.)

When an i-th element of FDSPECS is an integer j,
then it should be the lisp end of a pipe connected to some other child process p2.
 (not the normal fd of the lisp process, like 0,1,2)
The i-th fd of p1 is connected to this pipe,
then p1 and p2 can exchange the data each other.
When this happens, lisp process should not read from this pipe
 (or the data will not be sent to the destination of the pipe).
For that purpose, add a layer like `tee'.

When ENVIRONMENT is specified, it is passed to execve/execvpe.
It should be a list of strings (\"NAME1=VALUE1\" \"NAME2=VALUE2\")
or an alist ((\"NAME1\" . \"VALUE1\") ...).
When SEARCH is t, it uses execvp/execvpe. (==executable is searched through PATH)

On error during system call in the parent process, iolib/syscalls:syscall-error is signalled.
In the child process, the system call failure result in error status 203.
FIXME: this might not be good.
")
