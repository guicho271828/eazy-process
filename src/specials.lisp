

(in-package :eazy-process)

#|

Define some special variables

|#

(defparameter +fdspecs-default+ '(:in :out :out)
  "fd-specifier is one of 6 symbols
:i, :in, :input, :o,:out,:output, or an integer fd.
The first 3 and the last 3 symbols are the synonyms.")
