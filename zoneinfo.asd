;;;; zoneinfo.asd

(asdf:defsystem #:zoneinfo
  :description "Time zone information for Common Lisp projects based on the IANA time zone database"
  :author "Ákos Kiss <ak@coram.pub>"
  :license  "MIT License"
  :serial t
  :components ((:file "package")
               (:file "zoneinfo"))
  :in-order-to ((test-op (test-op "zoneinfo/test"))))

(asdf:defsystem #:zoneinfo/parser
  :depends-on (#:alexandria
               #:esrap)
  :components ((:file "zoneinfo-parser")))

(asdf:defsystem #:zoneinfo/test
  :depends-on (#:zoneinfo
               #:zoneinfo/parser
               #:fiveam)
  :components ((:file "zoneinfo-test"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :zoneinfo)))

(asdf:defsystem #:zoneinfo/*
  :depends-on (#:zoneinfo
               #:zoneinfo/parser
               #:zoneinfo/test))

(asdf:defsystem #:zoneinfo/make-dist
  :depends-on (#:zoneinfo/parser)
  :components ((:file "zoneinfo-dist"))
  :build-operation "program-op"
  :build-pathname "make-zoneinfo-dist"
  :entry-point "zoneinfo-dist:make-dist")
