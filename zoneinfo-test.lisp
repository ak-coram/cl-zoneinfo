;;;; zoneinfo-test.lisp

(defpackage #:zoneinfo-test
  (:use #:cl #:fiveam))

(in-package #:zoneinfo-test)

(def-suite :zoneinfo)
(in-suite :zoneinfo)

(test nop
  (is-true t))
