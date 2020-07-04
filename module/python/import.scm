(define-module (python import)
  #:use-module (ice-9 eval-string)
  #:use-module (python eval)
  #:use-module (python core type)
  #:use-module (python core libpython)
  #:replace (import))

(define-syntax import
  (syntax-rules ()
    ((import module)
     (define-public
       module
       (python-eval
         (format #f "__import__(\"~a\")" (quote module)))))
    ((import module #:as alias)
     (define-public
       alias
       (python-eval
         (format #f "__import__(\"~a\")" (quote module)))))
    ((import object #:from module)
     (define-public
       object
       (python-eval
         (format #f "__import__(\"~a\", fromlist=[\"~a\"]).~a"
                (quote module) (quote object) (quote object)))))
    ((import object #:from module #:as alias)
     (define-public
       alias
       (python-eval
         (format #f "__import__(\"~a\", fromlist=[\"~a\"]).~a"
                (quote module) (quote object) (quote object)))))))

(define (read-python-syntax _ p)
  (let* ((names (string-split (symbol->string (read p)) #\.))
         (obj (eval-string (car names)))
         (attributes (cdr names)))
    (for-each
      (lambda (name)
        (set! obj
              (python->scm
                (python-getattr-string (scm->python obj) name))))
      attributes)
    (if (list? obj) `(list ,@obj) obj)))

(read-hash-extend #\. read-python-syntax)
