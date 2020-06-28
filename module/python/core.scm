(define-module (python core)
  #:use-module (system foreign)
  #:export (libpyptr
            libpyproc
            python-initialize
            python-initialized?
            python-finalize
            python-run-simple-string
            python-import-module
            python-getattr-string
            python-call
            python-call-object))

(define libpython)

(let find-python ((version 3.9))
  (with-exception-handler
    (lambda (exception)
      (if (> version 3.3)
          (find-python (- version 0.1))
          (error "cannot find libpython (>3.6)")))
    (lambda ()
      (set! libpython
            (dynamic-link (format #f "libpython~s" version))))
    #:unwind? #t))

(define (libpyproc type name args)
  (pointer->procedure type (dynamic-func name libpython) args))

(define (libpyptr name)
  (dynamic-pointer name libpython))

(define (python-initialize)
  (dynamic-call "Py_Initialize" libpython))

(define (python-initialized?)
  (not (zero? ((libpyproc int "Py_IsInitialized" '())))))

(define (python-finalize)
  (when (python-initialized?)
    (dynamic-call "Py_FinalizeEx" libpython)))

(define (python-run-simple-string code)
  (zero? ((libpyproc int "PyRun_SimpleString" '(*))
          (string->pointer code))))

(define (python-import-module name)
  ((libpyproc '* "PyImport_ImportModule" '(*))
    (string->pointer name)))

(define (python-getattr-string pyobj attr)
  ((libpyproc '* "PyObject_GetAttrString" '(* *))
    pyobj (string->pointer attr)))

(define (python-gettype pyobj)
  (cadr (parse-c-struct pyobj '(int *)))) 

(define (python-call pyobj args kwargs)
  ((libpyproc '* "PyObject_Call" '(* * *)) pyobj args kwargs))

(define (python-call-object pyobj args)
  ((libpyproc '* "PyObject_CallObject" '(* *)) pyobj args))
