;;;; zoneinfo.lisp

(defpackage #:zoneinfo
  (:use #:cl)
  (:export *info*
           get-info

           ;; Symbols for types of time of day
           universal-time
           local-time
           standard-time
           daylight-saving-time

           ;; Symbols for weekdays
           monday
           tuesday
           wednesday
           thursday
           friday
           saturday
           sunday

           ;; Misc symbols for zones & rules
           zone
           rule
           only
           min
           max
           last
           >=
           <=))

(in-package #:zoneinfo)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *zoneinfo-dist-dir*
    (asdf:system-relative-pathname (asdf:find-system 'zoneinfo t)
                                   "zoneinfo-dist/")))

(defmacro read-resource (name)
  (with-open-file (stream (uiop:subpathname *zoneinfo-dist-dir* name))
    (let ((*read-eval* nil))
      `(quote ,(read stream)))))

(defmacro def-info ()
  `(defparameter *info*
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

(def-info)

(defun get-info (source)
  (let ((result (cdr (assoc source *info*))))
    (if result
        result
        (error "Unknown source ~a" source))))
