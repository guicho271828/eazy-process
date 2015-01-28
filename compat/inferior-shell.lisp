(in-package :eazy-process)

#|

*  inferior-shell compatibility

Goal: sexp -> bash compiler in inferior-shell
inferior-shell has the following interace

run
run/s
run/lines

there could be several possible ways to implement this feature.

1. implement it with direct shell.

the pros in this is that we can fuly handle the underlying processes,
and also that the code would be fast.

the cons is the rather bit of ugliness and complexity; it could not be a
direct 1-to-1 mapping between the shell script and the s-expression.

2. compile the sexp into bash script

the pros in this is simplicity, and its bug-ridden aspect.
 (or at least, we can rely on everythin bash supports, including exporting,
 aliasing, etc.)

the cons is that we have no access to the underlying processes.

|#

;; (defnamespace shell-expander)
;; (define-shell-expander progn (&body body)
;;   ...)
;; (define-shell-expander pipe (&body body)
;;   ...)
;; (define-shell-expander and (&body body)
;;   ...)
;; (define-shell-expander or (&body body)
;;   ...)
;; (define-shell-expander if (&body body)
;;   ...)

;; (defun compile-shell-sexp (sexp)
;;   
;; 
;;   )
