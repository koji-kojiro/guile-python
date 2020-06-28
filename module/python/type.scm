(define-module (python type)
  #:use-module (python core)
  #:use-module (system foreign)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-69)
  #:export (python->scm scm->python))

(define (python-isinstance? pyobj type)
  (positive? 
    ((libpyproc int "PyObject_IsInstance" '(* *))
      pyobj (libpyptr (format #f "Py~a_Type" type)))))

(define (python-callable? pyobj)
  (positive?
    ((libpyproc int "PyCallable_Check" '(*)) pyobj)))

(define (python->string pyobj)
  (pointer->string
    ((libpyproc '* "PyUnicode_AsUTF8" '(*))
      ((libpyproc '* "PyObject_Repr" '(*)) pyobj))))

(define-wrapped-pointer-type <python>
  python?
  wrap-python
  unwrap-python
  (lambda (obj port)
    (let ((pyobj (unwrap-python obj)))
      (format port "#<python object of [~a] ~x>"
              (python->string pyobj)
              (pointer-address pyobj)))))

(define (pycallable->scm pyobj)
  (lambda* (#:key (key 0) #:allow-other-keys #:rest rest)
    (let ((args '())
          (kwargs '()))
      (let loop ((index 0))
        (when (< index (length rest))
        (if (keyword? (list-ref rest index))
            (begin
              (append! kwargs
                       (list (symbol->string
                               (keyword->symbol (list-ref rest index)))
                             (list-ref rest (1+ index))))
              (loop (+ index 2)))
            (begin
              (append! args (list-ref rest index))
              (loop (1+ index))))))
      (python->scm
        (python-call pyobj
                     (scm->python (list->vector args))
                     (scm->python (alist->hash-table kwargs)))))))

(define (pytuple->scm pyobj)
  (let* ((n ((libpyproc int "PyTuple_Size" '(*)) pyobj))
         (obj (make-vector n)))
    (let loop ((index 0))
      (if (< index n)
        (begin
          (vector-set!
            obj index
            (python->scm
              ((libpyproc '* "PyTuple_GetItem" `(* ,int)) pyobj index)))
          (loop (1+ index)))
        obj))))

(define (pylist->scm pyobj)
  (vector->list
    (pytuple->scm
      ((libpyproc '* "PyList_AsTuple" '(*)) pyobj))))

(define (pydict->scm pyobj)
  (alist->hash-table
    (map vector->list
      (pylist->scm
        ((libpyproc '* "PyDict_Items" '(*)) pyobj)))))

(define (python->scm pyobj)
  (cond
    ((python-callable? pyobj) (pycallable->scm pyobj))
    ((python-isinstance? pyobj "Long")
     ((libpyproc long "PyLong_AsLong" '(*)) pyobj))
    ((python-isinstance? pyobj "Float")
     ((libpyproc double "PyFloat_AsDouble" '(*)) pyobj))
    ((python-isinstance? pyobj "Unicode")
     (pointer->string
       ((libpyproc '* "PyUnicode_AsUTF8" '(*)) pyobj)))
    ((python-isinstance? pyobj "Bytes")
     (string->char-set
      (pointer->string
        ((libpyproc '* "PyBytes_AsString" '(*)) pyobj))))
    ((python-isinstance? pyobj "List") (pylist->scm pyobj))
    ((python-isinstance? pyobj "Tuple") (pytuple->scm pyobj))
    ((python-isinstance? pyobj "Dict") (pydict->scm pyobj))
    (#t (wrap-python pyobj))))

(define-method (scm->python (obj <string>))
 ((libpyproc '* "PyUnicode_FromString" '(*))
  (string->pointer obj)))

(define-method (scm->python (obj <character-set>))
 ((libpyproc '* "PyBytes_FromString" '(*))
  (string->pointer obj)))

(define-method (scm->python (obj <integer>))
 ((libpyproc '* "PyLong_FromLong" `(,long)) obj))

(define-method (scm->python (obj <real>))
 ((libpyproc '* "PyFloat_FromDouble" `(,double)) obj))

(define-method (scm->python (obj <foreign>)) obj)

(define-method (scm->python (obj <list>))
  (let* ((n (length obj))
         (pyobj ((libpyproc '* "PyList_New" `(,int)) n)))
    (let loop ((index 0))
      (if (< index n)
          (begin
            ((libpyproc int "PyList_SetItem" `(* ,int *))
              pyobj index (scm->python (list-ref obj index)))
            (loop (1+ index)))
          pyobj))))

(define-method (scm->python (obj <vector>))
  ((libpyproc '* "PyList_AsTuple" '(*))
    (scm->python (vector->list obj))))

(define <hash-table> (class-of (make-hash-table)))

(define-method (scm->python (obj <hash-table>))
  (let ((pyobj ((libpyproc '* "PyDict_New" '()))))
    (for-each
      (lambda (pair)
        ((libpyproc int "PyDict_SetItemString" '(* * *))
          pyobj (string->pointer (car pair)) (scm->python (cdr pair))))
        (hash-table->alist obj))
    pyobj))
