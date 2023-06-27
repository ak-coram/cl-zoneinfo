;;;; zoneinfo-test.lisp

(defpackage #:zoneinfo-test
  (:use #:cl #:fiveam))

(in-package #:zoneinfo-test)

(def-suite :zoneinfo)
(in-suite :zoneinfo)

;;; The following cases just test if the parser doesn't crash

(defun parse (name)
  (let* ((system (asdf:find-system 'zoneinfo t))
         (path (asdf:system-relative-pathname system (format nil "tz/~a" name))))
    (zoneinfo-parser:parse-zoneinfo (uiop:read-file-string path))))

(test parse-africa
  (finishes (parse "africa")))

(test parse-antarctica
  (finishes (parse "antarctica")))

(test parse-asia
  (finishes (parse "asia")))

(test parse-australasia
  (finishes (parse "australasia")))

(test parse-backward
  (finishes (parse "backward")))

(test parse-backzone
  (finishes (parse "backzone")))

(test parse-etcetera
  (finishes (parse "etcetera")))

(test parse-europe
  (finishes (parse "europe")))

(test parse-factory
  (finishes (parse "factory")))

(test parse-northamerica
  (finishes (parse "northamerica")))

(test parse-southamerica
  (finishes (parse "southamerica")))
