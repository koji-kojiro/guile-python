#!/usr/local/bin/guile -s
!#
(use-modules (ice-9 readline)
             (ice-9 exceptions)
             (python eval)
             (python import))

(import sys)

(format #t "Python ~a on GNU Guile ~a\n" #.sys.version (version))

(let loop ()
  (let ((line (readline ">>> ")))
    (unless (eof-object? line)
      (with-exception-handler
        (lambda (_) (python-exec line))
        (lambda () (display (python-eval line)))
      #:unwind? #t)
    (newline)
    (loop))))
