;;;; zoneinfo.asd

(asdf:defsystem #:zoneinfo
  :description "Time zone information for Common Lisp projects based on the IANA time zone database"
  :author "Ákos Kiss <ak@coram.pub>"
  :license  "MIT License"
  :serial t
  :depends-on ()
  :components ((:file "package")
               (:file "zoneinfo"))
  :in-order-to ((test-op (test-op "zoneinfo/test"))))

(asdf:defsystem #:zoneinfo/test
  :depends-on (#:zoneinfo
               #:fiveam)
  :components ((:file "zoneinfo-test"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :zoneinfo)))

(asdf:defsystem #:zoneinfo/*
  :depends-on (#:zoneinfo
               #:zoneinfo/test))
