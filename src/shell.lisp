(in-package :eazy-process.impl)

#|

subshell implemented with fork-execvp

|#

(defun shell (path &rest argv)
  "Asynchronously execute `command` using an interpreter, returns a process structure object.
The `command' is a valid script in the interpreter specified in `*interpreter*'.

On error during system call, iolib/syscalls:syscall-error is signalled."
  (let ((pid (fork)))
    ;; On success, the PID of the child process is returned in the parent,
    ;; and 0 is returned in the child.  On failure, -1 is returned in the
    ;; parent, no child process is created, and errno is set appropriately.
    (cond
      ((zerop pid)
       ;; child
       (%exec (cons path argv)))
      ;; ((= -1 pid) ;; this is 
      ;;  ;; failure
      ;;  (%failure command))
      (t
       ;; parent
       (%make-process pid)))))

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

