#lang racket/base
;; Copyright Geoffrey S. Knauth. See file "info.rkt".

(require (prefix-in s19: (lib "19.ss" "srfi"))
         ; (prefix-in s48: (lib "48.ss" "srfi"))
         racket/date
         format-numbers)

(provide (all-defined-out))

(define (date->ymd10 d)
  (s19:date->string d "~Y-~m-~d"))

(define (fmt-ymd8-s10 yyyymmdd)
  (let*-values ([(year mmdd) (quotient/remainder yyyymmdd 10000)]
                [(month day) (quotient/remainder mmdd 100)])
    (string-append (number->string year) "-" (fmt-i-02d month) "-" (fmt-i-02d day))))
         
(define (date->ymd8 d)
  (string->number (s19:date->string d "~Y~m~d")))

(define (today->ymd8)
  (date->ymd8 (s19:current-date)))

;2011-02-03
(define (ymd8->date ymd8)
  (s19:string->date (fmt-ymd8-s10 ymd8) "~Y-~m-~d"))

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

(define (ymd8->10 ymd8)
  (let ([s (number->string ymd8)])
    (string-append (substring s 0 4) "-" (substring s 4 6) "-" (substring s 6))))

(define (leap-year? year)
  (and (zero? (modulo year 4)) (or (positive? (modulo year 100)) (zero? (modulo year 400)))))

(define (days-in-year year)
  (if (leap-year? year) 366 365))

(define DAY-SECONDS 86400)
(define-struct a (noon-secs net net-seen tx) #:transparent)

(define v-month-day-ones
  #( 1 32 60 91 121 152 182 213 244 274 305 335)) ; non-leap

(define v-month-days
  #(31 28 31 30  31  30  31  31  30  31  30  31)) ; non-leap

(define (year-day ymd8) +nan.0)

(define (year-vector year)
  (let ((v (make-vector (if (leap-year? year) 366 365)))
        (sec (find-seconds 0 0 12 1 1 year)))
    (for ([i (in-range (vector-length v))])
      (vector-set! v i ))
    (printf "~a~n" sec)))

;  0123456789
; "2015-11-10"
(define (ymd10-days-since d1 d0)
  (define s1 (ymd10->seconds d1))
  (define s0 (ymd10->seconds d0))
  (/ (- s1 s0) (* 24 3600)))

(define (ymd10->seconds ymd)
  (find-seconds 0 0 0
                (string->number (substring ymd 8 10))
                (string->number (substring ymd 5 7))
                (string->number (substring ymd 0 4))
                #f))

(define (ymd10-d1-within-days-of-d0? d1 num-days d0)
  (<= (ymd10-days-since d1 d0) num-days))

(module+ test
  (require rackunit))

(module+ test
  ;; Tests to be run with raco test
  (require rackunit)
  (check-equal? (fmt-ymd8-s10 20170922) "2017-09-22")
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

