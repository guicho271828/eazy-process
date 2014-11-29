;; this file is currently not used

(in-package :eazy-process)

(cl-syntax:use-syntax :cl-interpol)
(cl-syntax:use-syntax :annot)


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
