#lang scribble/manual
@(require scribble/core scribble/example scriblib/footnote
          format-ymd
          (prefix-in s19: srfi/19))

@require[@for-label[format-ymd
                    (prefix-in s19: srfi/19)
                    racket/base]]

@title{format-ymd: Simple year/month/day formatting}
@author{Geoff Knauth}

@defmodule[format-ymd]

The @racketmodname[format-ymd] module provides simple formatting for year, months and days.

Where you see @racket[s19:], as in @racket[s19:date] or @racket[s19:make-date], it refers to a function from @link["https://docs.racket-lang.org/srfi/srfi-19.html"]{srfi-19} @cite["SRFI-19"], which was included via @racket[(prefix-in s19: srfi/19)].

@(define my-eval (make-base-eval `(require format-ymd (prefix-in s19: srfi/19))))

@section{One}

@defproc[(date->ymd10 [d s19:date?]) string?]{Turns a @racket[srfi/19] date into a @racket[yyyy-mm-dd] string.}

@examples[#:eval my-eval
          (date->ymd10 (s19:make-date 0 0 0 0 1 10 2017 0))
          ]

@defproc[(date->ymd8 [d s19:date?]) integer?]{Turns a SRFI-19 date into a @racket[yyyymmdd] integer.}

@examples[#:eval my-eval
          (date->ymd8 (s19:make-date 0 0 0 0 1 10 2017 0))]

@defproc[(fmt-ymd8-s10 [yyyymmdd integer?]) string?]{Turns a @racket[yyyymmdd] integer into a @racket{yyyy-mm-dd} string.}

@examples[#:eval my-eval
          (fmt-ymd8-s10 20171001)]

@defproc[(today->ymd8) integer?]{Expresses today's date as a @racket[yyyymmdd] integer.}

@examples[#:eval my-eval
          (today->ymd8)]

@define-footnote[lcl-note make-lcl-note]

@defproc[(ymd8->date [ymd8 integer?]) s19:date?]{Turns a @racket[yyyymmdd] integer into a @racket[srfi/19] date (at @racket[00L]@lcl-note{@racket[00L] means time @racket[00:00] in your computer's local time zone}).}

@examples[#:eval my-eval
          (ymd8->date 20171001)]

@defproc[(ymd10->date [ymd10 string?]) s19:date?]{Turns a @racket{yyyy-mm-dd} string into a @racket[srfi/19] date (at @racket[00L]).}

@examples[#:eval my-eval
          (ymd10->date "2017-10-01")]

@defproc[(incr-date [start-day s19:date?] [days integer?]) s19:date?]{Take @racket[00L] on the starting day and produce a date @racket[days] forward.}

@examples[#:eval my-eval
          (let ([start-day (ymd10->date "2017-10-01")])
            (incr-date start-day 16))]

@defproc[(days-forward [start-day s19:date?] [days integer?]) (listof s19:date?)]{Take @racket[00L] on the starting day and produce a list of @racket[days] days going forward.}

@examples[#:eval my-eval
          (days-forward (ymd10->date "2017-09-29") 4)]

@defproc[(jan01-ymd8-ymd8 [ymd8 integer?]) integer?]{Produce the January 1st @racket[yyyy0101] integer that is the first day of the year @racket[yyyymmdd] is in.}

@examples[#:eval my-eval
          (jan01-ymd8-ymd8 20171001)]

@defproc[(jan01-y4-ymd8 [year integer?]) integer?]{Produce the January 1st @racket[yyyy0101] integer that is the first day of the year @racket[yyyy].}

@examples[#:eval my-eval
          (jan01-y4-ymd8 2017)]

@defproc[(ymd8->10 [ymd8 integer?]) string?]{Turn a @racket{yyyymmdd} integer into a @racket{yyyy-mm-dd} string.}

@examples[#:eval my-eval
          (ymd8->10 20171001)]

@defproc[(leap-year? [year integer?]) boolean?]{Is @racket[year] a leap year?}

@examples[#:eval my-eval
          (leap-year? 2017)
          (leap-year? 2016)
          (leap-year? 2000)
          (leap-year? 1900)]

@make-lcl-note[]

@(bibliography
  (bib-entry #:key "SRFI-19"
             #:title "SRFI-19: Time Data Types and Procedures"
             #:author "Will Fitzgerald"
             #:location "SRFI"
             #:url "http://srfi.schemers.org/srfi-19/"
             #:date "2000"))