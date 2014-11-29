(in-package :eazy-process)

(cl-syntax:use-syntax :cl-interpol)
(cl-syntax:use-syntax :annot)

;; accessors

;; fork

;; exec

@export
(defun kill (pid sigspec)
  (etypecase sigspec
    ((or symbol string) (shell-command #?"kill -s $U(sigspec) $(pid)"))
    (fixnum  (shell-command #?"kill -n $(sigspec) $(pid)"))))

@export
(defun fd (pid num)
  "return the pathname of file descriptor in procfs. pid can be a process id or :self"
  (etypecase pid
    ((or symbol string number) (pathname #?"/proc/self/fd/$(num)"))))

;; (with-open-file (s (fd 14299 1) :direction :output :if-exists :overwrite)
;;   (format s "hi!"))


@export
(defun pgid (&optional (pid (pid)))
  (%getpgid pid))
(defun setpgid (pgid &optional (pid (pid)))
  (%setpgid pid pgid))
(defsetf pgid setpgid)

;;; cgroups

(defun map-line (stream)
  (iter (for line = (read-line stream nil nil))
        (while line)
        (collect line)))

@export
(defun cgroup (&optional (pid (pid)))
  (with-open-file (s #?"/proc/$(pid)/cgroup")
    (mapcar (lambda (str)
              (register-groups-bind
                  (id subsystems groupname)
                  ("([0-9]+):([^:]*):(.*)" str)
                (list (when id (read-from-string (the string id)))
                      (split #\, subsystems)
                      groupname)))
            (map-line s))))

@export
(defvar *default-cgroup*
  (lastcar (first (cgroup))))

@export
(defun whoami ()
  (string-trim #?"\n" (shell-command "whoami")))
