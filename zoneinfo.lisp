;;;; zoneinfo.lisp

(defpackage #:zoneinfo
  (:use #:cl)
  (:export *zoneinfo*
           get-zoneinfo))

(in-package #:zoneinfo)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *zoneinfo-dist-dir*
    (asdf:system-relative-pathname (asdf:find-system 'zoneinfo t)
                                   "zoneinfo-dist/")))

(defmacro read-resource (name)
  (with-open-file (stream (uiop:subpathname *zoneinfo-dist-dir* name))
    (let ((*read-eval* nil))
      `(quote ,(read stream)))))

(defmacro def-zoneinfo ()
  `(defparameter *zoneinfo*
     (list
      ,@(loop :for name :in (list "africa"
                                  "antarctica"
                                  "asia"
                                  "australasia"
                                  "backward"
                                  "backzone"
                                  "etcetera"
                                  "europe"
                                  "factory"
                                  "northamerica"
                                  "southamerica")
              :for sym := (intern (string-upcase name) :keyword)
              :collect `(cons (quote ,sym)
                              (read-resource ,(format nil "~a.lisp" name)))))))

(def-zoneinfo)

(defun get-zoneinfo (source)
  (let ((result (cdr (assoc source *zoneinfo*))))
    (if result
        result
        (error "Unknown source ~a" source))))
