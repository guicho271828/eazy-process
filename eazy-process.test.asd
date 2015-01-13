#|
  This file is a part of eazy-process project.
  Copyright (c) 2014 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage eazy-process.test-asd
  (:use :cl :asdf))
(in-package :eazy-process.test-asd)


(defsystem eazy-process.test
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:eazy-process
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (load-op :after (op c)
                    (eval (read-from-string "(fiveam:run! :eazy-process)"))))
