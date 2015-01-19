
(in-package :eazy-process)

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
    ;; ;; always, for the sake of fifo
    ;; (setf mask (logior mask isys:o-nonblock))
    (cons (constantly nil)
          (lambda (i)
            (dclose (isys:open (namestring path) mask) i)))))
