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
    ((and fd (type fixnum))
     (cons nil fd))))

(defun make-pipe (x)
  (declare (ignore x))
  (multiple-value-list (pipe)))

(defun shell (argv &optional (fdspecs '#.+fdspecs-default+))
  "Asynchronously execute `argv' using fork(2) and `execvp', returns a process structure object.

ARGV is a sequence of strings. Each string is converted to
null-terminated C strings char* ARGV and passed to execvp.
The first element is also passed to execvp as the pathname of the executable.

FDSPECS is a sequence specifying how the child process should handle its output.
For the documentation of fd-specifier see `+fdspecs-default+'.

Ex.1 (:in :out :out)
Ex.2 (5 :out 8)

Each element in FDSPECS corresponds to each file descriptor of the child process.

When an i-th element of FDSPECS is a symbol, then a new pipe(2) is created for each i.
The i-th fd of the child process p1 is associated with the correct end of the new pipe,
the other end of the pipe is readable/writable from lisp from the process interface.
 (see the documentation of class PROCESS.)

When an i-th element of FDSPECS is an integer j,
then it should be the lisp end of a pipe connected to some other child process p2.
 (not the normal fd of the lisp process, like 0,1,2!)
The i-th fd of p1 is connected to this pipe,
then p1 and p2 can exchange the data each other.

When this happens, lisp process is no longer able to see the contents of the transmission
between p1 and p2, unless you specifically add a layer like `tee'.

On error during system call, iolib/syscalls:syscall-error is signalled.
"
  (let ((fdspecs (mapcar #'canonicalize-fdspec fdspecs)))
    (let ((pid (fork)))
      ;; On success, the PID of the child process is returned in the parent,
      ;; and 0 is returned in the child.  On failure, -1 is returned in the
      ;; parent, no child process is created, and errno is set appropriately.
      (cond
        ;; ((= -1 pid) ;; this is already handled by iolib, so don't care
        ;;  (%failure command))
        ((zerop pid)
         ;; child
         ;; close the old stdin, overwrite stdin with in[0]
         (iter (for i from 0)
               (for (parent . child) in fdspecs)
               (dup2 child i)
               (when parent
                 (iolib/syscalls:close parent)))
         (%exec argv))
        (t
         ;; parent
         (%make-process
          pid
          (iter (for i from 0)
                (for (parent . child) in fdspecs)
                (iolib/syscalls:close child)
                (collect parent result-type vector))))))))

(defun %exec (argv)
  (let (_strings)
    (unwind-protect
         (progn
           (setf _strings (foreign-alloc :string
                                         :initial-contents argv
                                         :null-terminated-p t))
           (execvp (first argv) _strings)) ; does not return on success
      (foreign-free _strings)
      (foreign-funcall "_exit" :int -1))))

;; Note:
;; IMPL> (cffi::canonicalize-foreign-type :string)
;; :POINTER

;; Note: allocated memory is automatically freed and get reclaimed by the
;; OS when exec is called successfully, because the data segment = heap is replaced.

