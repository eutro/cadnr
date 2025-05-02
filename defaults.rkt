#lang racket/base

(require "support.rkt"
         (for-syntax racket/base
                     racket/match
                     "numerics.rkt"))

(provide define-default-cadnr-top)

(define-new-top-definer define-default-cadnr-top
  [#px"c([ad]+)r" (_ mids)
   #`(lambda (x)
       #,(for/foldr ([expr #'x])
                    ([c (in-string mids)])
           #`(#,(if (eqv? c #\a)
                    #'car
                    #'cdr)
              #,expr)))]
  [#px"([frn]*)(first|rest|next)" (this-str starts end)
   #`(lambda (x)
       (local-require racket/list)
       #,(for/foldr ([expr #`(#,(if (equal? end "first")
                                    #'first
                                    #'rest)
                              x)])
                    ([c (in-string starts)])
           #`(#,(if (eqv? c #\f)
                    #'first
                    #'rest)
              #,expr)))]
  [#px"suc(c*)" (_ cs)
   #`(lambda (x) (+ x #,(string-length cs)))]
  [#px"add(\\d+)" (_ n)
   #`(lambda (x) (+ x #,(string->number n)))]
  [#px"pre(d+)" (_ ds)
   #`(lambda (x) (- x #,(string-length ds)))]
  [#px"sub(\\d+)" (_ n)
   #`(lambda (x) (- x #,(string->number n)))]

  [(app (λ (it) (words->number it)) (list pfx n sfx))
   (=> fail)
   (match* {pfx sfx}
     [{"" ""} #`#,n]
     [{"" "?"} #`(lambda (n) (= n #,n))]
     [{_ _} (fail)])]

  [(and name (app (λ (it) (words->number it #:ordinal? #t)) (list pfx n sfx)))
   (=> fail)
   (with-syntax ([n* (sub1 n)]
                 [name-sym (string->symbol name)])
     (match* {pfx sfx}
       [{"" ""}
        #`(lambda (ls)
            (unless (list? ls) (raise-argument-error 'name-sym "list?" ls))
            (list-ref ls n*))]
       [{"sequence-" ""}
        #`(lambda (s)
            (local-require racket/sequence)
            (unless (sequence? s) (raise-argument-error 'name-sym "sequence?" s))
            (sequence-ref s n*))]
       [{_ _} (fail)]))])
