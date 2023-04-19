#lang scribble/manual
@require[@for-label[cadnr
                    cadnr/defaults
                    (except-in racket/base #%top)]
         racket/runtime-path
         scribble/example]

@(define evaltor ((make-eval-factory '(racket/base cadnr))))

@title{c(a|d)ⁿr}

@author{Eutro}

@racket[car], @racket[cdr], @racket[caaaaddddr], and everything in between.

@defmodule[cadnr]

@define-runtime-path[logo-path]{cadnr.svg}
@image[logo-path #:scale 3]{c(a|d)ⁿr}

This module extends a number of built-in Racket functions that have obvious
arbitrary extensions.

For instance, the @racket[caaaar]-@racket[cddddr] family
of functions are defined for all combinations of @tt{a}s and @tt{d}s, but only
up to four! The obvious extension is to allow for an arbitrary number of each,
and to simply generate them on the fly when they are referred to.

With this module @racket[require]d, it's possible to use these functions
just by naming them:

@examples[#:eval evaltor
          (caddddddddddr '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
          (succcccc 1)
          (sub1234 5678)]

@defform[(#%top . id)]{

Overrides the @racket[#%top] from @racket[racket/base], see
@secref["expand-steps" #:doc '(lib "scribblings/reference/reference.scrbl")]
for how this is introduced implicitly.

Pattern matches @racket[id] to see if it matches one of the default
c(a|d)ⁿr patterns. If one of them matches, this form expands to a
suitable @racket[lambda] expression, otherwise it expands to the
@racket[#%top] from @racket[racket/base].

The patterns are as follows:

@(itemlist
  @item{@racket[#px"c([ad]+)r"]
               (e.g. @racket[caaaaaaddddr], @racket[cdaddadar])

        Extensions of the built-in @racket[caaaar]-@racket[cddddr] family,
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
                  (map sub64 '(1 2 5 10 32))]})

}

@section{Composable c(a|d)ⁿr}

@defmodule[cadnr/defaults]

@defform[(define-default-cadnr-top new-top old-top)]{

Define @racket[new-top] the same way as @racket[#%top] above, but such that
it falls back to @racket[old-top] instead of the one from @racket[racket/base].

This is included for composability reasons, so as not to introduce conflicts
with other modules that override the binding of @racket['#%top] the
way as @racket[cadnr] does.

}
