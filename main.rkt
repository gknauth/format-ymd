#lang racket/base
;; format-ymd/main.rkt
;; Copyright Geoffrey S. Knauth. See file "info.rkt".

(require (prefix-in s19: srfi/19)
         racket/date
         format-numbers)

(provide (all-defined-out))

(define (date->ymd10 d)
  (s19:date->string d "~Y-~m-~d"))

(define (ymd8->ymd10 ymd8)
  (let*-values ([(year mmdd) (quotient/remainder ymd8 10000)]
                [(month day) (quotient/remainder mmdd 100)])
    (string-append (number->string year) "-" (fmt-i-02d month) "-" (fmt-i-02d day))))

(define (ymd10->ymd8 ymd10)
  (let ([year (string->number (substring ymd10 0 4))]
        [month (string->number (substring ymd10 5 7))]
        [day (string->number (substring ymd10 8 10))])
    (+ (* 10000 year) (* 100 month) day)))
         
(define (date->ymd8 d)
  (string->number (s19:date->string d "~Y~m~d")))

(define (today->ymd8)
  (date->ymd8 (s19:current-date)))

;2011-02-03
(define (ymd8->date ymd8)
  (s19:string->date (ymd8->ymd10 ymd8) "~Y-~m-~d"))

; EXAMPLE
; (ymd8->date 20140912)
;= (date* 0 0 0 12 9 2014 5 254 #f -14400 0 "")

; "2011-02-03"
;  0123456789
(define (ymd10->date ymd10)
  (s19:string->date ymd10 "~Y-~m-~d"))

; EXAMPLE
; (ymd10->date "2014-09-12")
;= (date* 0 0 0 12 9 2014 5 254 #f -14400 0 "")

(define (incr-date start-day days)
  (let ((one-day (* 24 3600)))
    (s19:time-utc->date (s19:add-duration (s19:date->time-utc start-day)
                                          (s19:make-time s19:time-duration 0 (* days one-day))))))

(define (days-forward start-day days)
  (reverse (for/fold ((answer null))
                     ((i (in-range days)))
             (cons (incr-date start-day i) answer))))

(define (jan01-ymd8-ymd8 ymd8)
  (let-values ([(year mmdd) (quotient/remainder ymd8 10000)])
    (jan01-y4-ymd8 year)))

(define (jan01-y4-ymd8 year)
  (+ (* year 10000) 101))

(define (leap-year? year)
  (and (zero? (modulo year 4)) (or (positive? (modulo year 100)) (zero? (modulo year 400)))))

(define (days-in-year year)
  (if (leap-year? year) 366 365))

(define DAY-SECONDS 86400)

; I don't remember what this struct was supposed to be for.
; (define-struct a (noon-secs net net-seen tx) #:transparent)

(define v-month-day-ones
  #( 1 32 60 91 121 152 182 213 244 274 305 335)) ; non-leap

(define v-month-days
  #(31 28 31 30  31  30  31  31  30  31  30  31)) ; non-leap

(define (year-day ymd8) +nan.0)

(define (year-vector year)
  (let* ((ndays (days-in-year year))
         (v (make-vector ndays))
         (jan01 (ymd8->date (jan01-y4-ymd8 year))))
    (for ([i (in-range (vector-length v))]
          [day (in-naturals)])
         (vector-set! v i (incr-date jan01 day)))
    v))

;  0123456789
; "2015-11-10"
(define (ymd10-days-since ymd10-beg ymd10-end)
  (define s1 (ymd10->seconds ymd10-end))
  (define s0 (ymd10->seconds ymd10-beg))
  (/ (- s1 s0) DAY-SECONDS))

(define (ymd10->seconds ymd10)
  (find-seconds 0 0 0
                (string->number (substring ymd10 8 10))
                (string->number (substring ymd10 5 7))
                (string->number (substring ymd10 0 4))
                #f))

(define (ymd10-d1-within-days-following-d0? d0 num-days d1)
  (<= (ymd10-days-since d0 d1) num-days))

(module+ test
  ;; Tests to be run with raco test
  (require rackunit)
  (check-equal? (ymd8->ymd10 20170922) "2017-09-22")
  ;(check-equal? (ft))
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

