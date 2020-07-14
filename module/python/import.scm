(define-module (python import)
  #:use-module (python core type)
  #:use-module (python core libpython)
  #:use-module (ice-9 string-fun)
  #:replace (import))

(define (scm-name->py-name name)
  (string-replace-substring name "-" "_"))

(defmacro import (module)
  `(define-public
     ,(string->symbol (car (string-split (symbol->string module) #\.)))
       ((@ (python eval) python-eval) 
         (format #f "__import__(\"~a\")"
           ,((@@ (python import) scm-name->py-name) (symbol->string module))))))

(define (getattr obj . attrs)
  (for-each
    (lambda (attr)
      (set! obj (python->scm (python-getattr-string (scm->python obj) attr))))
    (map scm-name->py-name attrs))
  (if (list? obj) `(,@obj) obj))

(define (read-python-syntax _ p)
  (let ((names (string-split (symbol->string (read p)) #\.)))
    `((@@ (python import) getattr) ,(string->symbol (car names)) ,@(cdr names))))

(read-hash-extend #\. read-python-syntax)
