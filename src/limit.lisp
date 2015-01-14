

(in-package :eazy-process.impl)

#|

limit the process hierarchically

Trying to make the library compatible among unix oses and bsd,
it requires libbsd library

|#

;;; proc

(defvar *procfs-root* "/proc")

(defun proc (process &rest subdir)
  "Compose a pathname based on the given arguments. Return (values pathname existsp)
where existsp shows if the file or the directory exists.
`process' can be either a process object, :self, or a fixnum of a pid."
  (let ((path (format nil "~a/~(~a~{/~a~}~)"
                      *procfs-root*
                      (ematch process
                        (:self :self)
                        ((type fixnum) process)
                        ((process) (pid process)))
                      subdir)))
    (values path (probe-file path))))

;;; result of ls /proc/self/ in linux 3.13.0-43-generic #72-Ubuntu
;;; attr        cmdline          environ  io         maps        net        oom_score_adj  sched      stat     timers
;;; autogroup   comm             exe      latency    mem         ns         pagemap        schedstat  statm    uid_map
;;; auxv        coredump_filter  fd       limits     mountinfo   numa_maps  personality    sessionid  status   wchan
;;; cgroup      cpuset           fdinfo   loginuid   mounts      oom_adj    projid_map     smaps      syscall
;;; clear_refs  cwd              gid_map  map_files  mountstats  oom_score  root           stack      task

;;; 

(defun fd (process &optional (num 0)) (proc process :fd num))

(defun awk (path regex per-line-fn)
  (with-open-file (stream path)
    (iter (for line = (read-line stream))
          (multiple-value-bind (mstart mend rstarts rends)
              (scan regex line)
            (declare (ignore mend))
            (when mstart
              (apply per-line-fn
                     (iter (for rs in-vector rstarts)
                           (for re in-vector rends)
                           (collect
                               (subseq line rs re)))))))))
  
(defun io (process field)
  "
# cat /proc/3828/io
rchar: 323934931
wchar: 323929600
syscr: 632687
syscw: 632675
read_bytes: 0
write_bytes: 323932160
cancelled_write_bytes: 0
"
  (awk (proc process :io)
       "(.*): +(.*)"
       (lambda (name var)
         (when (string-equal field name)
           (return-from io (parse-integer var))))))

#+nil
(defun limits (process field)
  "Not included because it is slow...
cat /proc/self/limits
Limit                     Soft Limit           Hard Limit           Units     
Max cpu time              unlimited            unlimited            seconds   
Max file size             unlimited            unlimited            bytes     
Max data size             unlimited            unlimited            bytes     
Max stack size            8720000              unlimited            bytes     
Max core file size        0                    unlimited            bytes     
Max resident set          unlimited            unlimited            bytes     
Max processes             30940                30940                processes 
Max open files            1024                 4096                 files     
Max locked memory         65536                65536                bytes     
Max address space         unlimited            unlimited            bytes     
Max file locks            unlimited            unlimited            locks     
Max pending signals       30940                30940                signals   
Max msgqueue size         819200               819200               bytes     
Max nice priority         0                    0                    
Max realtime priority     0                    0                    
Max realtime timeout      unlimited            unlimited            us        
"
  (awk (proc process :io)
       "(.*): +(.*)"
       (lambda (name var)
         (when (string-equal field name)
           (return-from io (parse-integer var))))))


