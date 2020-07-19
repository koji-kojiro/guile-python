(define-module (python import)
  #:use-module (python core type)
  #:use-module (python core libpython)
  #:use-module (oop goops)
  #:use-module (ice-9 string-fun)
  #:replace (import set!))

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
      (let ((value (python->scm (python-getattr-string (scm->python obj) attr))))
        (set-object-property! value #:parent obj)
        (set-object-property! value #:name attr)
        ((@ (guile) set!) obj value)))
    (map scm-name->py-name attrs))
  (if (list? obj) `(,@obj) obj))

(define (read-python-syntax _ p)
  (let ((names (string-split (symbol->string (read p)) #\.)))
    `((@@ (python import) getattr) ,(string->symbol (car names)) ,@(cdr names))))

(define (setattr obj value)
  (python-setattr-string
    (scm->python (object-property obj #:parent))
    (object-property obj #:name)
    (scm->python value)))

(define-syntax-rule (set! obj value)
  (let ((parent (object-property obj #:parent))
        (name (object-property obj #:name)))
    (if (and parent name)
        ((@@ (python import) setattr) obj value)
        ((@ (guile) set!) obj value))))
 
(read-hash-extend #\. read-python-syntax)
