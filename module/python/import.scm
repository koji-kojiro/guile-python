(define-module (python import)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
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
       (begin
         (python-exec (format #f "import ~a as _" (quote module)))
         (python-eval "_"))))
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

(define-method (getattr obj attr)
  (format #t "~s . ~s\n" obj attr)
  (python->scm (python-getattr-string (scm->python obj) attr)))

(define-method (getattr (name <string>) attr)
  (format #t "~s . ~s\n" name attr)
  (with-exception-handler
    (lambda (_)
      (eval-string (format #f "~a.~a" name attr)))
    (lambda () (getattr (eval-string name) attr))
    #:unwind? #t))

(define (test obj attr)
  (format #t "~s . ~s\n" attr obj))

(define (read-python-syntax _ p)
  (let* ((names (string-split (symbol->string (read p)) #\.))
         (obj (car names)))
    (for-each
      (lambda (attr)
        (set! obj (getattr obj attr)))
      (cdr names))
    (if (list? obj) `(list ,@obj) obj)))

(read-hash-extend #\. read-python-syntax)
