(define-module (python core type)
  #:use-module (python core libpython)
  #:use-module (system foreign)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-69)
  #:export (python->scm scm->python))

(define (python-instance? pyobj type)
  (positive? 
    ((libpyproc int "PyObject_IsInstance" '(* *))
      pyobj (libpyptr (format #f "Py~a_Type" type)))))

(define (python-callable? pyobj)
  (positive?
    ((libpyproc int "PyCallable_Check" '(*)) pyobj)))

(define (python-none? pyobj)
  (= (pointer-address pyobj)
     (pointer-address (libpyptr "_Py_NoneStruct"))))

(define (python->repr pyobj)
  (pointer->string
    ((libpyproc '* "PyUnicode_AsUTF8" '(*))
      ((libpyproc '* "PyObject_Repr" '(*)) pyobj))))

(define (python->string pyobj)
  (pointer->string
    ((libpyproc '* "PyUnicode_AsUTF8" '(*))
      ((libpyproc '* "PyObject_Str" '(*)) pyobj))))

(define-wrapped-pointer-type <python>
  python?
  wrap-python
  unwrap-python
  (lambda (obj port)
    (let ((pyobj (unwrap-python obj)))
      (format port "#<python object of [~a] ~x>"
              (python->repr pyobj)
              (pointer-address pyobj)))))

(define (pycallable->scm pyobj)
  (lambda* (#:key (____key 0) #:allow-other-keys #:rest rest)
    (let* ((kwargs-list (take-right rest (* 2 (count keyword? rest))))
           (kwargs (make-hash-table))
           (args (list->vector
                   (take rest (- (length rest) (length kwargs-list))))))
      (let loop ((n 0))
        (when (< n (count keyword? rest))
          (hash-table-set!
            kwargs
            (symbol->string
              (keyword->symbol
                (list-ref kwargs-list (* n 2))))
            (list-ref kwargs-list (1+ (* n 2))))
          (loop (1+ n))))
      (python->scm
        (python-call pyobj (scm->python args) (scm->python kwargs))))))

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
    ((null-pointer? pyobj)
     (error
      (format #f "An exception of ~a reported by python"
        (python->repr ((libpyproc '* "PyErr_Occurred" '()))))))
    ((python-callable? pyobj) (pycallable->scm pyobj))
    ((python-none? pyobj) #nil)
    ((python-instance? pyobj "Bool")
     (not (zero? ((libpyproc long "PyLong_AsLong" '(*)) pyobj))))
    ((python-instance? pyobj "Long")
     ((libpyproc long "PyLong_AsLong" '(*)) pyobj))
    ((python-instance? pyobj "Float")
     ((libpyproc double "PyFloat_AsDouble" '(*)) pyobj))
    ((python-instance? pyobj "Unicode")
     (pointer->string
       ((libpyproc '* "PyUnicode_AsUTF8" '(*)) pyobj)))
    ((python-instance? pyobj "Bytes")
     (string->char-set
      (pointer->string
        ((libpyproc '* "PyBytes_AsString" '(*)) pyobj))))
    ((python-instance? pyobj "List") (pylist->scm pyobj))
    ((python-instance? pyobj "Tuple") (pytuple->scm pyobj))
    ((python-instance? pyobj "Dict") (pydict->scm pyobj))
    (#t (wrap-python pyobj))))

(define-method (scm->python (obj <string>))
 ((libpyproc '* "PyUnicode_FromString" '(*))
  (string->pointer obj)))

(define-method (scm->python (obj <character-set>))
 ((libpyproc '* "PyBytes_FromString" '(*))
  (char-set->string (string->pointer obj))))

(define-method (scm->python (obj <integer>))
 ((libpyproc '* "PyLong_FromLong" `(,long)) obj))

(define-method (scm->python (obj <real>))
 ((libpyproc '* "PyFloat_FromDouble" `(,double)) obj))

(define-method (scm->python (obj <boolean>))
 (if (nil? obj)
     (libpyptr "Py_None")
     ((libpyproc '* "PyBool_FromLong" `(,long)) (if obj 1 0))))

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
