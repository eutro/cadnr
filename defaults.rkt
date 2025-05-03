#lang racket/base

(require "support.rkt"
         syntax/parse/define
         (for-syntax syntax/parse/define
                     (for-syntax racket/base)
                     racket/base
                     racket/match
                     racket/syntax
                     racket/string
                     "numerics.rkt"))

(provide define-default-cadnr-top)

(begin-for-syntax
  (define-logger cadnr)

  (define-syntax-parser log-unbound-variables
    [(_ who:str ids:id ...)
     #'(log-cadnr-warning
        "~a: not proceeding due to unbound variables: ~a"
        who
        (string-join
         (for/list ([x (in-list (list #'ids ...))]
                    #:unless (identifier-binding x))
           (symbol->string (syntax-e x)))
         ", "))]))

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
                 [name-sym this-id])
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
       [{(regexp "(.+)-" (list _ type-name)) ""}
        (with-syntax ([type-ref (format-id this-id "~a-ref" type-name #:source this-id)]
                      [type? (format-id this-id "~a?" type-name #:source this-id)]
                      [type?-str (format "~a?" type-name)])
          (unless (and (identifier-binding #'type?)
                       (identifier-binding #'type-ref))
            (log-unbound-variables "type-last" type? type-ref)
            (fail))
          #`(lambda (s)
              (unless (type? s) (raise-argument-error 'name-sym type?-str s))
              (type-ref s n*)))]
       [{_ _} (fail)]))]

  [#px"(.+)-last" (_ type-name)
   (=> fail)
   (with-syntax ([name-sym this-id]
                 [type-length (format-id this-id "~a-length" type-name #:source this-id)]
                 [type? (format-id this-id "~a?" type-name #:source this-id)]
                 [type?-str (format "~a?" type-name)]
                 [type-ref (format-id this-id "~a-ref" type-name #:source this-id)])
     (unless (and (identifier-binding #'type-length)
                  (identifier-binding #'type?)
                  (identifier-binding #'type-ref))
       (log-unbound-variables "type-last" type-length type? type-ref)
       (fail))
     #`(lambda (s)
         (unless (type? s) (raise-argument-error 'name-sym type?-str s))
         (type-ref s (sub1 (type-length s)))))]

  [#px"(.+)-empty\\?" (_ type-name)
   (=> fail)
   (with-syntax ([name-sym this-id]
                 [type-length (format-id this-id "~a-length" type-name #:source this-id)]
                 [type? (format-id this-id "~a?" type-name #:source this-id)]
                 [type?-str (format "~a?" type-name)])
     (unless (and (identifier-binding #'type-length)
                  (identifier-binding #'type?))
       (log-unbound-variables "type-empty?" type-length type?)
       (fail))
     #`(lambda (s)
         (unless (type? s) (raise-argument-error 'name-sym type?-str s))
         (zero? (type-length s))))]

  [#px"non-empty-(.+)\\?" (_ type-name)
   (=> fail)
   (with-syntax* ([name-sym this-id]
                  [type? (format-id this-id "~a?" type-name #:source this-id)]
                  [type-length (if (free-identifier=? #'list? #'type?)
                                   #'length
                                   (format-id this-id "~a-length" type-name #:source this-id))]
                  [type?-str (format "~a?" type-name)])
     (unless (and (identifier-binding #'type-length)
                  (identifier-binding #'type?))
       (log-unbound-variables "non-empty-type?" type-length type?)
       (fail))
     #`(lambda (s)
         (and (type? s)
              (not (zero? (type-length s))))))])
