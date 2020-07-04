(define-module (python eval)
  #:use-module (python core util)
  #:use-module (python core type)
  #:use-module (python core libpython)
  #:export (python-eval python-exec))

(define (python-eval code)
  (ensure-python)
  (python->scm (python-run-string code)))

(define (python-exec code)
  (ensure-python)
  (python->scm
    (scm->python (python-run-simple-string code))))
