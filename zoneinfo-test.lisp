;;;; zoneinfo-test.lisp

(defpackage #:zoneinfo-test
  (:use #:cl #:fiveam))

(in-package #:zoneinfo-test)

(def-suite :zoneinfo)
(in-suite :zoneinfo)

;;; The following cases just test if the parser doesn't crash

(test parse-africa
  (finishes (zoneinfo-parser:parse-zoneinfo "tz/africa")))

(test parse-antarctica
  (finishes (zoneinfo-parser:parse-zoneinfo "tz/antarctica")))

(test parse-asia
  (finishes (zoneinfo-parser:parse-zoneinfo "tz/asia")))

(test parse-australasia
  (finishes (zoneinfo-parser:parse-zoneinfo "tz/australasia")))

(test parse-backward
  (finishes (zoneinfo-parser:parse-zoneinfo "tz/backward")))

(test parse-backzone
  (finishes (zoneinfo-parser:parse-zoneinfo "tz/backzone")))

(test parse-etcetera
  (finishes (zoneinfo-parser:parse-zoneinfo "tz/etcetera")))

(test parse-europe
  (finishes (zoneinfo-parser:parse-zoneinfo "tz/europe")))

(test parse-factory
  (finishes (zoneinfo-parser:parse-zoneinfo "tz/factory")))

(test parse-northamerica
  (finishes (zoneinfo-parser:parse-zoneinfo "tz/northamerica")))

(test parse-southamerica
  (finishes (zoneinfo-parser:parse-zoneinfo "tz/southamerica")))
