
(in-package :eazy-process)

(defmacro with-rlimit (pairs &body body)
  "Append/overwrite the current rlimit resouce limitation.
As noted in *rlimit-resources*, this does not affect the lisp process itself.
Only the spawned child process will be affected."
  ;; binary search? not! :)
  `(let ((*rlimit-resources* (copy-array *rlimit-resources*)))
     ,@(iter (for (resource value) in pairs)
             (collecting
              `(setf (aref *rlimit-resources* ,resource) ,value)))
     ,@body))

