(define-module (python eval)
  #:use-module (python core type)
  #:use-module (python core libpython)
  #:export (python-eval python-exec))

(define (python-eval code)
  (python->scm (python-run-string code)))

(define (python-exec code)
  (python->scm
    (scm->python (python-run-simple-string code))))
