(define-module (python core util)
  #:use-module (python core libpython)
  #:export (ensure-python))

(define (ensure-python)
  (unless (python-initialized?) (python-initialize)) #t)
