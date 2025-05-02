#lang scribble/manual
@require[@for-label[cadnr
                    cadnr/defaults
                    racket/list
                    racket/sequence
                    (rename-in
                     racket/base
                     ;; with zws
                     [#%top #%​top])]
         racket/runtime-path
         scribble/example]

@(define evaltor ((make-eval-factory '(racket/base cadnr racket/sequence))))

@title{c(a|d)ⁿr}

@author{Eutro}

@racket[car], @racket[cdr], @racket[caaaaddddr], and everything in between.

@defmodule[cadnr]

@define-runtime-path[logo-path]{cadnr.svg}
@image[logo-path #:scale 3]{c(a|d)ⁿr}

This module extends a number of built-in Racket functions that have obvious
arbitrary extensions.

For instance, the @racket[caaaar]–@racket[cddddr] family
of functions are defined for all combinations of @tt{a}-s and @tt{d}-s, but only
up to four! The obvious extension is to allow for an arbitrary number of each,
and to simply generate them on the fly when they are referred to.

With this module @racket[require]d, it's possible to use these functions
just by naming them:

@examples[#:eval evaltor
          (caddddddddddr '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
          (succcccc 1)
          (sub1234 5678)
          twenty-million-six-hundred-and-forty-one-thousand-six-hundred-and-ninety
          ]

@defform[(#%top . id)]{

Overrides @racket[#%​top] from @racketmodname[racket/base].
This is introduced automatically by the expander whenever you use an otherwise
unbound variable, see @secref["expand-steps" #:doc '(lib "scribblings/reference/reference.scrbl")].

Pattern matches @racket[id] to see if it matches one of the
c(a|d)ⁿr patterns below. If one of them matches, the expression expands
to an expression as described, otherwise it expands to the
@racket[#%​top] from @racketmodname[racket/base].

The patterns are as follows:

@(itemlist
  @item{@racket[#px"c([ad]+)r"]
               (e.g. @racket[caaaaaaddddr], @racket[cdaddadar])

        Extensions of the built-in @racket[caaaar]–@racket[cddddr] family,
        to an arbitrary number of @tt{a}s and @tt{d}s.

        Each new
        @tt{a} or @tt{d} corresponds to another @racket[car]
        or @racket[cdr] respectively, in composition order
        (so the last @tt{a} or @tt{d} is applied first, then the others
        from right to left).

        @examples[#:eval evaltor
                  (cadaaaaar '(((((((1) 2) 3) 4) 5) 6) 7))
                  (cadaddr '((a b) (c d) (e f) (g h)))]}

  @item{@racket[#px"([frn]*)(first|rest|next)"]
               (e.g. @racket[fffirst], @racket[frest],
                     @racket[nfnext])

        Analogous to the previous pattern,
        but uses @racket[first] and @racket[rest] instead
        of @racket[car] and @racket[cdr],
        so that these can only be used with proper lists.

        The naming of these is inspired by the similar
        family of functions in
        @hyperlink["https://clojuredocs.org/clojure.core/ffirst"]{Clojure}.

        @examples[#:eval evaltor
                  (fnfffffirst '(((((((1) 2) 3) 4) 5) 6) 7))
                  (fnfnnext '((a b) (c d) (e f) (g h)))]}

  @item{@racket[#px"suc(c*)"]
               (e.g. @racket[suc], @racket[succ],
                     @racket[succcccccc])

        Extensions of the successor function (called @racket[add1] in Racket).

        Each @tt{c} adds one more to the number, starting at 0 with just one
        (so @racket[suc] is the identity on numbers, and
            @racket[succ] is equivalent to @racket[add1]).

        @examples[#:eval evaltor
                  (suc 10)
                  (succccccccc 1)]}

  @item{@racket[#px"pre(d+)"]
               (e.g. @racket[pred], @racket[predddd])

        Extensions of the predecessor function (called @racket[sub1] in Racket).

        Each @tt{d} subtracts one more from the number, starting at 1 with just one
        (so @racket[pred] is equivalent to @racket[sub1]).

        @examples[#:eval evaltor
                  (pred 10)
                  (predddddddd 100)]}

  @item{@racket[#px"add(\\d+)"]
               (e.g. @racket[add123], @racket[add37949])

        Extensions of @racket[add1], but for any natural number.

        @examples[#:eval evaltor
                  (add256 10)
                  (map add64 '(1 2 5 10 32))]}

  @item{@racket[#px"sub(\\d+)"]
               (e.g. @racket[sub123], @racket[sub37949])

        Extensions of @racket[sub1], but for any natural number.

        @examples[#:eval evaltor
                  (sub256 500)
                  (map sub64 '(1 2 5 10 32))]}

  @item{@racket[#px"$NUMBER"]
               (e.g. @racket[five], @racket[five-hundred-and-twelve])

        An English-language number.

        The syntax includes English numbers from zero
        to one thousand, as well as
        @hyperlink["https://en.wikipedia.org/wiki/Names_of_large_numbers"]{large numbers}
        in @hyperlink["https://en.wikipedia.org/wiki/Long_and_short_scales"]{short-scale}
        up to the centillions. Furthermore, you can
        @hyperlink["https://dictionaryblog.cambridge.org/2017/10/04/1066-and-all-that-how-to-say-years/"]{
        write the names of years out in words too}, in either
        British or American English.

        @examples[#:eval evaltor
                  four
                  one-hundred-and-eight
                  one-million-billion

                  nineteen-oh-five
                  ten-sixty-six
                  two-thousand-eight

                  (define (pengő->forint p)
                    (/ p four-hundred-octillion))

                  (pengő->forint seventy-six-septillion)]}

  @item{@racket[#px"($NUMBER)\\?"]
               (e.g. @racket[five?], @racket[nine-hundred-and-ninety-nine?])

        Extensions of @racket[zero?].

        @examples[#:eval evaltor
                  (two? 2)
                  (nineteen-eighty-four? 2025)

                  (struct http-response (status))
                  (define not-found-response (http-response 404))
                  (four-oh-four? (http-response-status not-found-response))]}

  @item{@racket[#px"($NUMBER)th"]
               (e.g. @racket[twentieth], @racket[fifty-fifth])

        Extensions of @racket[first]–@racket[fifteenth], finally
        future-proofing @hyperlink["https://con.racket-lang.org/"]{RacketCon}.

        @examples[#:eval evaltor
                  #:hidden
                  (define RacketCon
                    (for/list ([i (in-range 2011 10000)])
                      (list 'RacketCon i)))]
        @examples[#:eval evaltor
                  (twentieth RacketCon)
                  (nine-hundred-and-ninety-ninth RacketCon)

                  (define letters (string->list "abcdefghijklmnopqrstuvwxyz"))
                  (twenty-third letters)]}

  @item{@racket[#px"sequence-($NUMBER)th"]
               (e.g. @racket[sequence-twentieth], @racket[sequence-fifty-fifth])

        Extensions of @racket[first]–@racket[fifteenth], but using @racket[sequence-ref].

        @examples[#:eval evaltor
                  (sequence-twenty-second (in-naturals 1))
                  (sequence-sixty-fifth (sequence-map integer->char (in-naturals 1)))]})}

@section{Composable c(a|d)ⁿr}

@defmodule[cadnr/defaults]

@defform[(define-default-cadnr-top new-top old-top)]{

Define @racket[new-top] the same way as @racket[#%top] above, but such that
it falls back to @racket[old-top] instead of the @racketmodname[racket/base]'s @racket[#%​top] .

This is included for composability reasons, so as not to introduce conflicts
with other modules that override the binding of @racketid[#%top] the
way @racket[cadnr] does.

}
