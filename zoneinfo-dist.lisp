;;;; zoneinfo-dist.lisp

(defpackage #:zoneinfo-dist
  (:use #:cl)
  (:export #:make-dist))

(in-package #:zoneinfo-dist)

(defun make-zoneinfo (tz-dir dist-dir name)
  (ensure-directories-exist dist-dir)
  (with-open-file (stream (format nil "~a~a.lisp" dist-dir name)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (let ((*package* (find-package 'zoneinfo-parser)))
      (format stream "~s~%" (zoneinfo-parser:parse-zoneinfo
                             (format nil "~a~a" tz-dir name))))))

(defun make-dist (&optional custom-tz-dir)
  (loop :with system := (asdf:find-system 'zoneinfo t)
        :with tz-dir := (or custom-tz-dir
                            (asdf:system-relative-pathname system "tz/"))
        :with dist-dir
          := (asdf:system-relative-pathname system "zoneinfo-dist/")
        :for name :in '("africa"
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
        :do (format t "Writing ~a~a.lisp... " dist-dir name)
        :do (make-zoneinfo tz-dir dist-dir name)
        :do (format t "DONE~%")))
