#lang scribble/manual
@(require scribble/core scribble/example
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

@defproc[(date->ymd10 [d s19:date?]) string?]{Turns a SRFI-19 date into a @racket[yyyy-mm-dd] string.}

@examples[#:eval my-eval
          (date->ymd10 (s19:make-date 0 0 0 0 1 10 2017 0))
          ]

@(bibliography
  (bib-entry #:key "SRFI-19"
             #:title "SRFI-19: Time Data Types and Procedures"
             #:author "Will Fitzgerald"
             #:location "SRFI"
             #:url "http://srfi.schemers.org/srfi-19/"
             #:date "2000"))