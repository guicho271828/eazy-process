(in-package :eazy-process)

#|

subshell implemented with fork-execvp

|#

(defun canonicalize-fdspec (fdspec)
  "Take an fd-specifier and return a cons.
When fd-specifier is a symbol, the return value is (<int parent-fd> . <int child-fd>),
where parent-fd and child-fd are the file descripters newly created by pipe(2).
This is different from what pipe(2) directly provides,
because for pipe(&fd,0) fd[0] is always the read end and fd[1] the write end.

If the FDSPEC is an integer <int fd>, it returns (nil . <int fd>)."
  (ematch fdspec
    ((or :i :in :input)
     (multiple-value-bind (read write) (pipe)
       (cons write read)))
    ((or :o :out :output)
     (multiple-value-bind (read write) (pipe)
       (cons read write)))
    ((type fixnum)
     (cons nil fdspec))
    ((list* (and path (type pathname)) options)
     (apply #'%open path options))))


;; if-does-not-exist---one of :error, :create, or nil.
;; The default is
;; :error if direction is :input or if-exists is :overwrite or :append
;; :create if direction is :output or :io, and if-exists is neither :overwrite nor :append;
;; or nil when direction is :probe. 

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
    (cons nil (isys:open (namestring path) mask))))

;; O_APPEND O_ASYNC O_CLOEXEC O_CREAT O_DIRECT O_DIRECTORY O_DSYNC O_EXCL
;; O_LARGEFILE O_NOATIME O_NOCTTY O_NOFOLLOW O_NONBLOCK O_NDELAY O_PATH O_SYNC
;; O_TMPFILE O_TRUNC

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
         ;; child
         (handler-case
             (progn
               (iter (for kind from 0)
                     (for value in-vector *rlimit-resources*)
                     (when (plusp value)
                       (setf (rlimit kind) value)))
               (iter (for i from 0)
                     (for (parent . child) in fdspecs)
                     (dup2 child i)
                     (when parent
                       (iolib/syscalls:close parent)))
               (%exec argv environments env-p search))
           (isys:syscall-error (c)
             (declare (ignorable c))
             (foreign-funcall "_exit" :int 203))
           (error (c)
             (declare (ignorable c))
             (foreign-funcall "_exit" :int 202))))
        (t
         ;; parent
         (%make-process
          pid
          (iter (for i from 0)
                (for (parent . child) in fdspecs)
                (iolib/syscalls:close child)
                (collect parent result-type vector))))))))

(defun %exec (argv env env-p search)
  (if search
      (if env-p
          (%execvpe argv env)
          (%execvp argv))
      (if env-p
          (%execve argv env)
          (%execv argv))))

;;;; 4 versions

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
