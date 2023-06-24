;;;; zoneinfo-parser.lisp

(defpackage #:zoneinfo-parser
  (:use #:cl #:esrap)
  (:export #:parse-zoneinfo))

(in-package #:zoneinfo-parser)

(defrule whitespace (or #\space #\page #\tab #\vt)
  (:constant nil))

(defrule end-of-line (or (and #\linefeed #\return)
                         (and #\return #\linefeed)
                         #\linefeed
                         #\return)
  (:constant nil))

(defrule empty-lines (+ (and (* whitespace) end-of-line))
  (:constant nil))

(defrule digit (digit-char-p character))

(defrule integer (and (? (or #\+ #\-)) (+ digit))
  (:destructure (sign digits)
    (let ((i (parse-integer (text digits))))
      (if (string= sign "-")
          (- i)
          i))))

(defrule hours (or integer #\-)
  (:lambda (hours)
    (list (cond
            ((numberp hours) hours)
            ((string= hours "-") 0)))))

(defrule hours-and-minutes (and integer #\: integer)
  (:destructure (hours colon minutes)
    (declare (ignore colon))
    (if (minusp hours)
        (list hours (- minutes))
        (list hours minutes))))

(defrule hours-minutes-and-seconds (and integer #\: integer #\: integer)
  (:destructure (hours c1 minutes c2 seconds)
    (declare (ignore c1 c2))
    (if (minusp hours)
        (list hours (- minutes) (- seconds))
        (list hours minutes seconds))))

(defrule hours-minutes-and-fractional-seconds
    (and integer #\: integer #\: integer #\. integer)
  (:destructure (hours c1 minutes c2 seconds dot fraction)
    (declare (ignore c1 c2 dot))
    (let ((total-seconds (+ seconds (/ fraction
                                       (expt 10 (ceiling (log fraction 10)))))))
      (if (minusp hours)
          (list hours (- minutes) (- total-seconds))
          (list hours minutes total-seconds)))))

(defrule time-of-day (and (or hours-minutes-and-fractional-seconds
                              hours-minutes-and-seconds
                              hours-and-minutes
                              hours)
                          (? (or #\w #\s #\u #\g #\z)))
  (:destructure (time type)
    (cons (alexandria:switch (type :test #'string=)
            (nil 'local-time)
            ("w" 'local-time)
            ("s" 'standard-time)
            ("u" 'universal-time)
            ("g" 'universal-time)
            ("z" 'universal-time))
          time)))

(defrule save (and (or hours-minutes-and-fractional-seconds
                       hours-minutes-and-seconds
                       hours-and-minutes
                       hours)
                   (? (or #\s #\d)))
  (:destructure (time type)
    (let ((is-zero (every #'zerop time)))
      (cons (alexandria:switch (type :test #'string=)
              (nil (if is-zero 'standard-time 'daylight-saving-time))
              ("s" 'standard-time)
              ("d" 'daylight-saving-time))
            time))))

(defrule month (or (and "Jan" (? #\u) (? #\a) (? #\r) (? #\y))
                   (and "Feb" (? #\r) (? #\u) (? #\a) (? #\r) (? #\y))
                   (and "Mar" (? #\c) (? #\h))
                   (and "Apr" (? #\i) (? #\l))
                   (and "May")
                   (and "Jun" (? #\e))
                   (and "Jul" (? #\y))
                   (and "Aug" (? #\u) (? #\s) (? #\t))
                   (and "Sep" (? #\t) (? #\e) (? #\m) (? #\b) (? #\e) (? #\r))
                   (and "Oct" (? #\o) (? #\b) (? #\e) (? #\r))
                   (and "Nov" (? #\e) (? #\m) (? #\b) (? #\e) (? #\r))
                   (and "Dec" (? #\e) (? #\m) (? #\b) (? #\e) (? #\r)))
  (:lambda (result)
    (alexandria:switch ((car result) :test #'string=)
      ("Jan" 1)
      ("Feb" 2)
      ("Mar" 3)
      ("Apr" 4)
      ("May" 5)
      ("Jun" 6)
      ("Jul" 7)
      ("Aug" 8)
      ("Sep" 9)
      ("Oct" 10)
      ("Nov" 11)
      ("Dec" 12))))

(defrule day (or (and "Mon" (? #\d) (? #\a) (? #\y))
                 (and "Tue" (? #\s) (? #\d) (? #\a) (? #\y))
                 (and "Wed" (? #\n) (? #\e) (? #\s) (? #\d) (? #\a) (? #\y))
                 (and "Thu" (? #\r) (? #\s) (? #\d) (? #\a) (? #\y))
                 (and "Fri" (? #\d) (? #\a) (? #\y))
                 (and "Sat" (? #\u) (? #\r) (? #\d) (? #\a) (? #\y))
                 (and "Sun" (? #\d) (? #\a) (? #\y)))
  (:lambda (result)
    (alexandria:switch ((car result) :test #'string=)
      ("Mon" 'monday)
      ("Tue" 'tuesday)
      ("Wed" 'wednesday)
      ("Thu" 'thursday)
      ("Fri" 'friday)
      ("Sat" 'saturday)
      ("Sun" 'sunday))))

(defrule on (or integer
                (and "last" day)
                (and day ">=" integer)
                (and day "<=" integer))
  (:lambda (result)
    (cond
      ((numberp result) result)
      ((string= (car result) "last") `(last ,(cadr result)))
      (t (alexandria:switch ((cadr result) :test #'string=)
           (">=" `(>= ,(car result) ,(caddr result)))
           ("<=" `(<= ,(car result) ,(caddr result))))))))

(defrule comment (and (* whitespace) #\# (* (not end-of-line)))
  (:constant nil))

(defrule quoted-token (and #\" (* (not #\")) #\")
  (:destructure (q1 token q2)
    (declare (ignore q1 q2))
    (text token)))

(defrule unquoted-token (+ (not (or whitespace
                                    comment
                                    end-of-line)))
  (:lambda (token) (text token)))

(defrule token (or quoted-token unquoted-token))

(defrule year (or integer
                  token)
  (:lambda (result)
    (cond
      ((numberp result) result)
      (t (intern (string-upcase result))))))

(defrule rule-line (and (* whitespace)
                        "Rule"
                        (+ whitespace) token       ; NAME
                        (+ whitespace) year        ; FROM
                        (+ whitespace) year        ; TO
                        (+ whitespace) #\-         ; TYPE (reserved)
                        (+ whitespace) month       ; IN
                        (+ whitespace) on          ; ON
                        (+ whitespace) time-of-day ; AT
                        (+ whitespace) save        ; SAVE
                        (+ whitespace) token       ; LETTER/S
                        (* whitespace) (or comment end-of-line))
  (:destructure (w1 rule-token w2 name w3 from w4 to w5 type
                    w6 in w7 on w8 at w9 save w10 letters w11 eol)
    (declare (ignore w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11
                     rule-token type eol))
    (list 'rule name from to in on at save letters)))

(defrule until (and (or #\- year)
                    (? (or comment
                           (and (+ whitespace) month)))
                    (? (or comment
                           (and (+ whitespace)
                                on)))
                    (? (or comment
                           (and (+ whitespace) time-of-day))))
  (:destructure (year (&optional w1 month) (&optional w2 day) (&optional w3 time))
    (declare (ignore w1 w2 w3))
    (unless (eql year #\-)
      (cons year
            (when month
              (cons month
                    (when day
                      (cons day
                            (when time (list time))))))))))

(defrule zone-continuation-line (and (* whitespace)
                                     time-of-day          ; STDOFF
                                     (+ whitespace) token ; RULES
                                     (+ whitespace) token ; FORMAT
                                     (? (and (+ whitespace) until)) ; UNTIL
                                     (* whitespace) (or comment end-of-line))
  (:destructure (w1 stdoff w2 rules w3 format until w4 eol)
    (declare (ignore w1 w2 w3 w4 eol))
    (list stdoff rules format (second until))))

(defrule zone-line (and (* whitespace)
                        "Zone"
                        (+ whitespace) token ; NAME
                        (+ (or empty-lines comment zone-continuation-line)))
  (:destructure (w1 zone-token w2 name continuations)
    (declare (ignore w1 w2 zone-token))
    (list 'zone name (remove nil continuations))))

(defrule link-line (and (* whitespace)
                        "Link"
                        (+ whitespace)
                        token ; TARGET
                        (+ whitespace)
                        token ; LINK-NAME
                        (* whitespace) (or comment end-of-line))
  (:destructure (w1 link-token w2 target w3 link-name w4 eol)
    (declare (ignore w1 w2 w3 w4 link-token eol))
    (list 'link target link-name)))

(defrule line (or empty-lines
                  comment
                  link-line
                  rule-line
                  zone-line))

(defrule file (* line))

(defun parse-zoneinfo (path)
  (remove nil (parse 'file (uiop:read-file-string path))))


