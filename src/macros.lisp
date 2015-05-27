
(in-package :eazy-process)


(defmacro with-process ((&whole whole process argv &optional fdspecs environment search)
                             &body body)
  "Spawn a process, bind the process to PROCESS,
execute the body and finalize (= kill and wait w/ nohang) the process.
BODY works as an implicit progn, and the return value is that of the progn.

It does not synchronously wait the process. Therefore, it is the user's
responsibility to inhibit the early termination of the process.
The aim of finalization is to release the resources of the process e.g. file descriptor and pipes.

 Example:

 (with-open-process (p (\"sleep\" \"10\"))
    (print :waiting)
    (wait p))

"
  (declare (ignorable argv fdspecs environment search))
  `(let (,process)
     (unwind-protect
         (progn
           (setf ,process (shell ,@(cdr whole)))
           ,@body)
       ;; there is a timing where process is not set.
       ;; (let ((process (shell...)))) is also a problem
       ;; since the atomicity of let and unwin-protect is not guaranteed
       ;; (the stack may be unwound before the unwind-protect takes effect)
       (when ,process
         (finalize-process ,process)))))
