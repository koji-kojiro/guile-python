#!/usr/local/bin/guile -s
!#
(use-modules (python import))

(import urllib.request)

(let* ((url "http://www.gnu.org/licenses/gpl-3.0.txt")
       (res (#.urllib.request.urlopen url))
       (content (#.res.read)))
  (display (#.content.decode))
  (newline)
  (#.res.close))
