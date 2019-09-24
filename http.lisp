;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :cl)

(defpackage :http-client
  (:use :common-lisp
        :usocket)
  (:export :request-http))

(in-package :http-client)

(defun make-http-headers (lst)
  (reduce #'(lambda (a b) (concatenate 'string a b))
          (mapcar #'(lambda (x) (concatenate 'string (car x) ": " (cadr x) "~%"))
                  lst)))

(defun http-get (&optional (path "/") (headers '()))
  (concatenate 'string "GET " path " HTTP/1.1~%" (make-http-headers headers) "~%"))

(defun read-all (stream)
  (loop for char = (read-char-no-hang stream nil :eof)
     until (or (null char) (eq char :eof)) collect char into msg
     finally (return (values msg char))))

(defun request-http (server port path headers)
  (let ((socket (usocket:socket-connect server port :element-type 'character)))
    (unwind-protect
         (progn
           (format (usocket:socket-stream socket)
                   (http-get path headers))
           (force-output (usocket:socket-stream socket))
           (usocket:wait-for-input socket)
           (read-all (usocket:socket-stream socket)))
      (usocket:socket-close socket))))

(defun charlist-to-string (charlist)
  (reduce #'(lambda (a b) (concatenate 'string a b))
          (mapcar #'string charlist)))
