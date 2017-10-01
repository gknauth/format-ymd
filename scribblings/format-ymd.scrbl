#lang scribble/manual
@(require scribble/core scribble/example scriblib/footnote
          format-ymd
          (prefix-in s19: srfi/19)
          ;racket/vector
          )

@require[@for-label[format-ymd
                    (prefix-in s19: srfi/19)
                   ; racket/vector
                    racket/base]]

@title{format-ymd: Simple year/month/day formatting}
@author{Geoff Knauth}

@defmodule[format-ymd]

The @racketmodname[format-ymd] module provides simple formatting for year, months and days.

Where you see @racket[s19:], as in @racket[s19:date] or @racket[s19:make-date], it refers to a function from @link["https://docs.racket-lang.org/srfi/srfi-19.html"]{srfi-19} @cite["SRFI-19"], which was included via @racket[(prefix-in s19: srfi/19)].

@(define my-eval (make-base-eval `(require format-ymd (prefix-in s19: srfi/19) racket/vector)))

@section{srfi/19 dates}

@defproc[(date->ymd10 [d s19:date?]) string?]{Turns a @racket[srfi/19] date into a @racket[yyyy-mm-dd] string.}

@examples[#:eval my-eval
          (date->ymd10 (s19:make-date 0 0 0 0 1 10 2017 0))
          ]

@defproc[(date->ymd8 [d s19:date?]) integer?]{Turns a SRFI-19 date into a @racket[yyyymmdd] integer.}

@examples[#:eval my-eval
          (date->ymd8 (s19:make-date 0 0 0 0 1 10 2017 0))]

@defproc[(day-of-week [d s19:date?]) symbol?]{Query the day of week and return it as a lowercase 3-letter English symbol.}

@examples[#:eval my-eval
          (day-of-week (s19:make-date 0 0 0 0 1 10 2017 0))]

@defproc[(incr-date [start-day s19:date?] [days integer?]) s19:date?]{Take @racket[00L] on the starting day and produce a date @racket[days] forward.}

@examples[#:eval my-eval
          (let ([start-day (ymd10->date "2017-10-01")])
            (incr-date start-day 16))]

@defproc[(days-forward [start-day s19:date?] [days integer?]) (listof s19:date?)]{Take @racket[00L] on the starting day and produce a list of @racket[days] days going forward.}

@examples[#:eval my-eval
          (days-forward (ymd10->date "2017-09-29") 4)]

@defproc[(year-vector [year integer?]) (vectorof s19:date?)]{Produce a vector containing a @racket[s19:date] for each of the year's days.}

@examples[#:eval my-eval
          (let ([v (year-vector 2017)])
            (list (vector-take v 3) (vector-take-right v 3)))]

@section{ymd8 integers}

@defproc[(ymd8->ymd10 [ymd8 integer?]) string?]{Turns a @racket[yyyymmdd] integer into a @racket{yyyy-mm-dd} string.}

@examples[#:eval my-eval
          (ymd8->ymd10 20171001)]

@define-footnote[lcl-note make-lcl-note]

@defproc[(ymd8->date [ymd8 integer?]) s19:date?]{Turns a @racket[yyyymmdd] integer into a @racket[srfi/19] date (at @racket[00L]@lcl-note{@racket[00L] means time @racket[00:00] in your computer's local time zone}).}

@examples[#:eval my-eval
          (ymd8->date 20171001)]

@defproc[(today->ymd8) integer?]{Expresses today's date as a @racket[yyyymmdd] integer.}

@examples[#:eval my-eval
          (today->ymd8)]

@defproc[(jan01-ymd8-ymd8 [ymd8 integer?]) integer?]{Produce the January 1st @racket[yyyy0101] integer that is the first day of the year @racket[yyyymmdd] is in.}

@examples[#:eval my-eval
          (jan01-ymd8-ymd8 20171001)]

@defproc[(jan01-y4-ymd8 [year integer?]) integer?]{Produce the January 1st @racket[yyyy0101] integer that is the first day of the year @racket[yyyy].}

@examples[#:eval my-eval
          (jan01-y4-ymd8 2017)]

@defproc[(ymd8-day-of-week [ymd8 integer?]) symbol?]{Given @racket[yyyyddd], return a lowercase 3-letter English symbol representing the day of the week.}

@examples[#:eval my-eval
          (ymd8-day-of-week 20171001)]

@make-lcl-note[]

@section{ymd10 strings}

@defproc[(ymd10->ymd8 [ymd10 string?]) integer?]{Turns a @racket{yyyy-mm-dd} string into a @racket[yyyymmdd] integer.}

@examples[#:eval my-eval
          (ymd10->ymd8 "2017-10-01")]

@defproc[(ymd10->date [ymd10 string?]) s19:date?]{Turns a @racket{yyyy-mm-dd} string into a @racket[srfi/19] date (at @racket[00L]).}

@examples[#:eval my-eval
          (ymd10->date "2017-10-01")]

@defproc[(today->ymd10) string?]{Expresses today's date as a @racket{yyyy-mm-dd} string.}

@examples[#:eval my-eval
          (today->ymd10)]

@defproc[(ymd10-day-of-week [ymd10 string?]) symbol?]{Given @racket{yyyy-dd-mm}, return a lowercase 3-letter English symbol representing the day of the week.}

@examples[#:eval my-eval
          (ymd10-day-of-week "2017-10-01")]

@section{Year}

@defproc[(leap-year? [year integer?]) boolean?]{Is @racket[year] a leap year?}

@examples[#:eval my-eval
          (leap-year? 2017)
          (leap-year? 2016)
          (leap-year? 2000)
          (leap-year? 1900)]

@defproc[(days-in-year [year integer?]) integer?]{How many days are in an Earth year?}

@examples[#:eval my-eval
          (days-in-year 2017)
          (days-in-year 2016)]

@section{Arithmetic}

@defproc[(ymd8-days-since [ymd8-beg integer?] [ymd8-end integer?]) integer?]{Produces the number of days from the @racket[yyyymmdd] integer representing the first day @racket[ymd8-beg] up to the last day @racket[ymd8-end].}

@examples[#:eval my-eval
          (ymd8-days-since 20170515 20170715)]

@defproc[(ymd10-days-since [ymd10-beg string?] [ymd10-end string?]) integer?]{Produces the number of days from the @racket{yyyy-mm-dd} string representing the first day @racket[ymd10-beg] up to the last day @racket[ymd10-end].}

@examples[#:eval my-eval
          (ymd10-days-since "2017-05-15" "2017-07-15")]

@defproc[(ymd8->seconds [ymd8 integer?]) integer?]{Answer the number of seconds since the platform-specific epoch of the @racket[yyyy-mm-dd] integer representing @racket[0Z] (@racket[00:00 UTC]) on that date.}

@examples[#:eval my-eval
          (let ([a (ymd8->seconds 20161231)]
                [b (ymd8->seconds 20170101)])
            (list a b (- b a)))]

@defproc[(ymd10->seconds [ymd10 string?]) integer?]{Answer the number of seconds since the platform-specific epoch of the @racket{yyyy-mm-dd} string representing @racket[0Z] (@racket[00:00 UTC]) on that date.}

@examples[#:eval my-eval
          (let ([a (ymd10->seconds "2016-12-31")]
                [b (ymd10->seconds "2017-01-01")])
            (list a b (- b a)))]

@defproc[(ymd10-d1-within-days-following-d0? [d0 string?] [num-days integer?] [d1 string?]) boolean?]{Is @racket[d1] within @racket[num-days] following @racket[d0]?  Both @racket[d1] and @racket[d0] are strings of the form @racket{yyyy-mm-dd}.}

@examples[#:eval my-eval
          (ymd10-d1-within-days-following-d0? "2017-04-15" 30 "2017-05-15")
          (ymd10-d1-within-days-following-d0? "2017-05-15" 30 "2017-06-15")]

@defproc[(ymd8-d1-within-days-following-d0? [d0 integer?] [num-days integer?] [d1 integer?]) boolean?]{Is @racket[d1] within @racket[num-days] following @racket[d0]?  Both @racket[d1] and @racket[d0] are integers of the form @racket[yyyymmdd].}

@examples[#:eval my-eval
          (ymd8-d1-within-days-following-d0? 20170415 30 20170515)
          (ymd8-d1-within-days-following-d0? 20170515 30 20170615)]

@(bibliography
  (bib-entry #:key "SRFI-19"
             #:title "SRFI-19: Time Data Types and Procedures"
             #:author "Will Fitzgerald"
             #:location "SRFI"
             #:url "http://srfi.schemers.org/srfi-19/"
             #:date "2000"))