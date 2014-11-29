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
