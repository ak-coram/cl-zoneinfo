;;;; zoneinfo-dist.lisp

(defpackage #:zoneinfo-dist
  (:use #:cl)
  (:export #:make-dist))

(in-package #:zoneinfo-dist)

(defun get-tz-release-tag (system)
  (string-trim '(#\linefeed #\return #\space)
               (uiop:read-file-string
                (asdf:system-relative-pathname system "TZ_RELEASE"))))

(defun get-archive-url (tag)
  (format nil "https://github.com/eggert/tz/archive/refs/tags/~a.zip" tag))

(defun make-zoneinfo (contents dist-dir name)
  (with-open-file (stream (format nil "~a~a.lisp" dist-dir name)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (let ((*package* (find-package 'zoneinfo-parser)))
      (format stream "~s~%" (zoneinfo-parser:parse-zoneinfo contents)))))

(defun make-dist (&optional force-download)
  (let* ((system (asdf:find-system 'zoneinfo t))
         (dist-dir (asdf:system-relative-pathname system "zoneinfo-dist/"))
         (tz-submodule-available
           (uiop:file-exists-p
            (asdf:system-relative-pathname system "tz/europe")))
         (tag (when (or force-download (not tz-submodule-available))
                (get-tz-release-tag system)))
         (names '("africa"
                  "antarctica"
                  "asia"
                  "australasia"
                  "backward"
                  "backzone"
                  "etcetera"
                  "europe"
                  "factory"
                  "northamerica"
                  "southamerica")))
    (ensure-directories-exist dist-dir)
    (if tag
        (uiop:with-temporary-file (:stream s)
          (format t "Downloading tz release ~a... " tag)
          (force-output)
          (let ((bytes (dex:get (get-archive-url tag))))
            (write-sequence bytes s))
          (format t "DONE~%")
          (zip:with-zipfile (f s)
            (loop :for name :in names
                  :for entry := (zip:get-zipfile-entry
                                 (format nil "tz-~a/~a" tag name) f)
                  :do (format t "Writing ~a~a.lisp... " dist-dir name)
                  :do (force-output)
                  :do (make-zoneinfo
                       (babel:octets-to-string (zip:zipfile-entry-contents entry)
                                               :encoding :utf-8)
                       dist-dir
                       name)
                  :do (format t "DONE~%"))))
        (loop :with tz-dir := (asdf:system-relative-pathname system "tz/")
              :for name :in names
              :do (format t "Writing ~a~a.lisp... " dist-dir name)
              :do (force-output)
              :do (make-zoneinfo (uiop:read-file-string (format nil "~a~a"
                                                                tz-dir
                                                                name))
                                 dist-dir
                                 name)
              :do (format t "DONE~%")))))
