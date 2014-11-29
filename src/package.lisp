#|
  This file is a part of eazy-process project.
  Copyright (c) 2014 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage eazy-process
  (:use :cl :iterate :alexandria :cffi :optima
        :ppcre
        :trivial-shell)
  (:export :pid :ppid :pgid))
(in-package :eazy-process)

(cl-syntax:use-syntax :cl-interpol)
(cl-syntax:use-syntax :annot)

;; blah blah blah.

@export
(defun subprocesses (ppid)
  (let ((str (shell-command #?"pgrep -P $(ppid)")))
    (with-input-from-string (s str)
      (iter (for pid = (read s nil nil))
            (while pid)
            (collect pid)))))

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

(cffi:defcfun (pid "getpid") :int)
(cffi:defcfun (ppid "getppid") :int)

;; int setpgid(pid_t pid, pid_t pgid);
;; pid_t getpgid(pid_t pid);
;; pid_t getpgrp(void); /* POSIX.1 version */

(cffi:defcfun (%getpgid "getpgid") :int (pid :int))
(cffi:defcfun (%setpgid "setpgid") :int (pid :int) (pgid :int))

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

@export
(defun cgcreate (controllers &key
                               (as (whoami))
                               (group as)
                               (name "lisp"))
  (let* ((interpol:*list-delimiter* #\,)
         (*print-case* :downcase)
         (command #?"cgcreate -a $(as):$(group) -t $(as):$(group) -g @(controllers):$(*default-cgroup*)/$(name)"))
    (princ command)
    (shell-command command)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct cgroup
    (system "" :type string)
    (hierarchy "" :type string)))

@export
(defun lscgroup (&key system hierarchy)
  "Example return value:

 ((\"cpuset\" \"/\") (\"cpuset\" \"/user\") ... ) "
  (let ((*print-case* :downcase))
    (with-input-from-string (s (shell-command
                                (format nil "lscgroup")))
      (iter (for line in (map-line s))
            (register-groups-bind
                (system2 hierarchy2)
                ("([^:]*):(.*)" line)
              (when (and
                     (or (not system) (string-equal system system2))
                     (< (length hierarchy) (length hierarchy2))
                     (every #'char-equal hierarchy hierarchy2))
                (collect (make-cgroup :system system2 :hierarchy hierarchy2))))))))

(defun cgroup-system-mount-point (name)
  (eazy-process.cgroup:cgroup-init)
  (with-foreign-pointer-as-string (mountpoint* +filename-max+)
    (with-foreign-string (name* name)
      (eazy-process.cgroup:CGROUP-GET-SUBSYS-MOUNT-POINT name* mountpoint*))))

;; (defun cgroup-resource (cg)
;;   (match cg
;;     ((cgroup system hierarchy)
     


(defun cgroup-difference (a b))
(defun cgroup-union (a b))
(defun cgroup-intersection (a b))

(defun cgroup-min (a b))
(defun cgroup-max (a b))

(defun cgexec ())
