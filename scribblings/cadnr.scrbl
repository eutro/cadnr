#lang scribble/manual
@require[@for-label[cadnr
                    cadnr/defaults
                    racket/list
                    racket/sequence
                    racket/string
                    (rename-in
                     racket/base
                     ;; with zws
                     [#%top #%​top])]
         racket/runtime-path

         scribble/racket
         scribble/core
         scribble/decode
         scribble/example]

@(define evaltor ((make-eval-factory '(racket/base cadnr racket/sequence))))
@(define vv racketvarfont)

@(define symdef-style (make-style "RktSymDef" (list)))

@(define (defcadnr key . str)
   (define e0 (make-element #f (decode-content str)))
   (define tag (list 'cadnr key))
   (define e (make-link-element symdef-style (list e0) tag))
   (define t (make-target-element #f (list e) tag))
   (make-index-element
    #;style #f
    #;content (list t)
    #;tag tag
    #;plain-seq
    (list (datum-intern-literal
           (clean-up-index-string (content->string e))))
    #;entry-seq
    (list e)
    #;desc
    (hash 'kind "cadnr-binding")))

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
  @item{@defcadnr["cadnr"]{@racket[#px"c([ad]+)r"]}
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

  @item{@defcadnr["frnnext"]{@racket[#px"([frn]*)(first|rest|next)"]}
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

  @item{@defcadnr["succc"]{@racket[#px"suc(c*)"]}
               (e.g. @racket[suc], @racket[succ],
                     @racket[succcccccc])

        Extensions of the successor function (called @racket[add1] in Racket).

        Each @tt{c} adds one more to the number, starting at 0 with just one
        (so @racket[suc] is the identity on numbers, and
            @racket[succ] is equivalent to @racket[add1]).

        @examples[#:eval evaltor
                  (suc 10)
                  (succccccccc 1)]}

  @item{@defcadnr["preddd"]{@racket[#px"pre(d+)"]}
               (e.g. @racket[pred], @racket[predddd])

        Extensions of the predecessor function (called @racket[sub1] in Racket).

        Each @tt{d} subtracts one more from the number, starting at 1 with just one
        (so @racket[pred] is equivalent to @racket[sub1]).

        @examples[#:eval evaltor
                  (pred 10)
                  (predddddddd 100)]}

  @item{@defcadnr["addN"]{@racket[#px"add(\\d+)"]}
               (e.g. @racket[add123], @racket[add37949])

        Extensions of @racket[add1], but for any natural number.

        @examples[#:eval evaltor
                  (add256 10)
                  (map add64 '(1 2 5 10 32))]}

  @item{@defcadnr["subN"]{@racket[#px"sub(\\d+)"]}
               (e.g. @racket[sub123], @racket[sub37949])

        Extensions of @racket[sub1], but for any natural number.

        @examples[#:eval evaltor
                  (sub256 500)
                  (map sub64 '(1 2 5 10 32))]}

  @item{@defcadnr["number"]{@tt{@vv{number}}}
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

  @item{@defcadnr["number?"]{@tt{@vv{number}?}}
           (e.g. @racket[five?], @racket[nine-hundred-and-ninety-nine?])

        Extensions of @racket[zero?].

        @examples[#:eval evaltor
                  (two? 2)
                  (nineteen-eighty-four? 2025)

                  (struct http-response (status))
                  (define not-found-response (http-response 404))
                  (four-oh-four? (http-response-status not-found-response))]}

  @item{@defcadnr["numberth"]{@tt{@vv{number}th}}
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

  @item{@defcadnr["数字"]{@tt{@vv{数字}}}
           (e.g. @racket[五], @racket[五百二十])
        
        A Chinese-language number.
        
        The syntax includes Chinese number, positive or non-positive.
        Currently names of years (e.g. 一九八四) and large number from
        亿 upwards (兆, 京, etc.) are not supported.
        
        @examples[#:eval evaltor
                  四
                  一万零八十六
                  十一万四千五百一十四
                  一百九十一万九千八百一十
                  负一
                  零
                  (eval:error 负零)]}

  @item{@defcadnr["数字?"]{@tt{@vv{数字}?}}
           (e.g. @racket[五?], @racket[九百九十九?])

        Extensions of @racket[zero?].

        @examples[#:eval evaltor
                  (二? 2)
                  (九百八十四? 2026)]}

  @item{@defcadnr["第数字"]{@tt{第@vv{数字}}}
           (e.g. @racket[第二十], @racket[第五十五])

        Extensions of @racket[first]–@racket[fifteenth].

        @examples[#:eval evaltor
                  (第二十 RacketCon)
                  (第九百九十九 RacketCon)]}

  @item{@defcadnr["type-numberth"]{@tt{@vv{type}-@vv{number}th}},
        @defcadnr["type-第数字"]{@tt{@vv{type}-第@vv{数字}}},
        @defcadnr["type-last"]{@tt{@vv{type}-last}},
        @defcadnr["type-empty?"]{@tt{@vv{type}-empty?}}
        @defcadnr["non-empty-type?"]{@tt{non-empty-@vv{type}?}}
        (e.g. @racket[flvector-fifteenth])

        Extensions of @racketmodname[racket/list] and @racket[non-empty-string?]
        for arbitrary container types.

        These work by referencing bindings derived from @vv{type}. As such,
        a binding for @tt{@vv{type}?} must be in scope, in addition to:

        @(itemlist
          @item{For @tt{@vv{type}-@vv{number}th}: @tt{@racketvarfont{type}-ref}}
          @item{For @tt{@vv{type}-last}: @tt{@racketvarfont{type}-ref} and @tt{@racketvarfont{type}-length}}
          @item{For @tt{@vv{type}-empty?} and @tt{non-empty-@vv{type}?}: @tt{@racketvarfont{type}-length}})

        @examples[#:eval evaltor
                  (sequence-twenty-second (in-naturals 1))
                  (sequence-sixty-fifth (sequence-map integer->char (in-naturals 1)))
                  (string-tenth "abcdefghijklmnopqrstuvwxyz")
                  (string-第十 "abcdefghijklmnopqrstuvwxyz")
                  (vector-last (vector 1 2 3))
                  (non-empty-list? null)
                  (eval:error stream-fifth)
                  (require racket/stream)
                  (stream-fifth (stream 1 2 3 4 5 6))
                  (require racket/flonum)
                  (flvector-empty? (flvector))]})}

@section{Composable c(a|d)ⁿr}

@defmodule[cadnr/defaults]

@defform[(define-default-cadnr-top new-top old-top)]{

Define @racket[new-top] the same way as @racket[#%top] above, but such that
it falls back to @racket[old-top] instead of the @racketmodname[racket/base]'s @racket[#%​top] .

This is included for composability reasons, so as not to introduce conflicts
with other modules that override the binding of @racketid[#%top] the
way @racket[cadnr] does.

}
