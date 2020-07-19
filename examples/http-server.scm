#!/usr/local/bin/guile -s
!#
(use-modules (python import))

(import http.server)
(import socketserver)

(let* ((handler #.http.server.SimpleHTTPRequestHandler)
       (httpd (#.socketserver.TCPServer #("" 8080) handler)))
  (#.httpd.serve-forever))

