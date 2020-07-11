(define-module (python import)
  #:use-module (python core type)
  #:use-module (python core libpython)
  #:replace (import))

(defmacro import (module)
  `(define-public
     ,(string->symbol (car (string-split (symbol->string module) #\.)))
       ((@ (python eval) python-eval) 
         (format #f "__import__(\"~a\")" (quote ,module)))))

(define (getattr obj . attrs)
  (for-each
    (lambda (attr)
      (set! obj (python->scm (python-getattr-string (scm->python obj) attr))))
    attrs)
  (if (list? obj) `(list ,@obj) obj))

(define (read-python-syntax _ p)
  (let ((names (string-split (symbol->string (read p)) #\.)))
    `((@@ (python import) getattr) ,(string->symbol (car names)) ,@(cdr names))))

(read-hash-extend #\. read-python-syntax)
