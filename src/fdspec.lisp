
(in-package :eazy-process)

;;; preprocessing

(defun dclose (old new)
  "Duplicate the old fd, set it to the new fd, then close the old fd"
  (dup2 old new)
  ;; note that if read==i then duplication does not occur
  (unless (= old new) (isys:close old)))

(defun canonicalize-fdspec (fdspec &optional (fd 0))
  "Take an fd-specifier and return a cons of (parent-fn . child-fn) where
both parent-fn and child-fn are closures of 1 arg, the fd on the child end.
Each function runs a job that should be done in the context of
parent/child process.

if FD is specified and the direction is not specified, use the default
direction of that fd by default. For FD > 2, there is no default direction
and it signals an error.

Parent-fn should return the fd of the parent-end."
  (ematch fdspec
    ;; implicit pipes
    ((or :i :in :input)
     (multiple-value-bind (read write) (isys:pipe)
       (cons (lambda (i) (isys:close read) write)
             (lambda (i)
               (dclose read i)
               (isys:close write)))))
    ((or :o :out :output)
     (multiple-value-bind (read write) (isys:pipe)
       (cons (lambda (i) (isys:close write) read)
             (lambda (i)
               (dclose write i)
               (isys:close read)))))
    ((type fixnum)
     (cons (lambda (i)
             ;; ensure that the only process opening this fd is the child
             ;; process
             (isys:close fdspec) nil)
           (lambda (i)
             (dclose fdspec i))))
    ;; without options
    ((pipe)
     (%pipe fdspec :direction (elt +fdspecs-default+ fd)))
    ((type pathname)
     (%open fdspec :direction (elt +fdspecs-default+ fd)))
    ;; with options
    ((list* (and pipe (pipe)) options)
     (apply #'%pipe pipe options))
    ((list* (and path (type pathname)) options)
     (apply #'%open path options))
    ;; dev/null (idea in iolib)
    (nil (canonicalize-fdspec
          (match (elt +fdspecs-default+ fd)
            (:output `(#p"/dev/null" :direction :output :if-exists :overwrite)) ;; always exists
            (:input `(#p"/dev/null" :direction :input))
            (nil (error "There is no default direction for fd ~a, it should be specified!" fd)))
          fd))))

(defun %pipe (pipe &key (direction :input))
  ;; On the child side, close the old fd and the parent side of fd.  On the
  ;; parent side, just return the fd (since the other process might use the
  ;; other side of the pipe)
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
  "ANSI CL standard:

   if-does-not-exist---one of :error, :create, or nil.
   The default is
   :error if direction is :input or if-exists is :overwrite or :append
   :create if direction is :output or :io, and if-exists is neither :overwrite nor :append;
   or nil when direction is :probe."
  (multiple-value-bind (input output mask) ;; partly copied from sbcl
      (ecase direction
        (:input  (values   t nil isys:o-rdonly))
        (:output (values nil   t isys:o-wronly))
        (:io     (values   t   t isys:o-rdwr))
        (:probe  (values   t nil isys:o-rdonly)))
    (declare (ignorable input output))
    (when (eq direction :output)
      (ecase if-exists
        ((:error nil)
         (assert (not (probe-file path)) nil ":if-exists flag was :error, but the file ~a exists"
                 path)
         (setf mask (logior mask isys:o-excl)))
        ;; ((:rename :rename-and-delete)
        ;;  (setf mask (logior mask isys:o-creat)))
        ((:supersede)
         (setf mask (logior mask isys:o-trunc)))
        (:append
         (setf mask (logior mask isys:o-append)))
        ((:overwrite nil) nil)))
    ;; returns a file descriptor
    (ecase if-does-not-exist
      (:create
       (setf mask (logior mask isys:o-creat)))
      (:error
       (assert (probe-file path)
               nil ":if-does-not-exist flag was :error, but the file ~a doesnTt exist"
               path))
      (nil (assert (eq :probe direction) nil
                   ":if-does-not-exist flag was NIL, the direction should be :probe, actually ~a"
                   direction)))
    ;; ;; always, for the sake of fifo
    ;; (setf mask (logior mask isys:o-nonblock))
    (cons (constantly nil)
          (lambda (i)
            (dclose (isys:open (namestring path) mask) i)))))
