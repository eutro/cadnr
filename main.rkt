#lang racket/base

(require "defaults.rkt")

(define-default-cadnr-top new-top #%top)
(provide (rename-out [new-top #%top]))

(module* test racket/base
  (require rackunit
           (submod "..")
           racket/function
           racket/list)

  (define tree
    '(((((a . b) . (c . d))
        .
        (e . f))
       .
       ((((g . h) . (i . j))
         . ())))
      . ()))

  (check-equal? (caaaaar tree) 'a)
  (check-equal? (cdaaaar tree) 'b)
  (check-equal? (cadaaar tree) 'c)
  (check-equal? (cddaaar tree) 'd)
  (check-equal? (cadaar tree) 'e) ;; built-in
  (check-equal? (cddaar tree) 'f) ;; built-in
  (check-equal? (caaaadar tree) 'g)
  (check-equal? (cdaaadar tree) 'h)
  (check-equal? (cadaadar tree) 'i)
  (check-equal? (cddaadar tree) 'j)

  (define tree2
    '(((((a b) (c d))
        (e f))
       ())
      (a b)
      c))

  (check-equal? (fffffirst tree2) 'a)
  (check-equal? (rffffirst tree2) '(b))
  (check-equal? (ffrfffirst tree2) 'c)
  (check-equal? (rfrfffirst tree2) '(d))
  (check-equal? (rest tree2) '((a b) c)) ;; built-in
  (check-equal? (frest tree2) '(a b))
  (check-equal? (frrest tree2) 'c)
  (check-equal? (frnext tree2) 'c)

  (check-equal? (succc 2) 4)
  (check-equal? (suc 0) 0)
  (check-exn exn:fail:contract? (thunk (suc 'a)))

  (check-equal? (predd 4) 2)
  (check-equal? (pred 1) 0)

  (check-equal? (add2 2) 4)
  (check-equal? (add348765 17263) (+ 348765 17263))
  (check-equal? (map add100 (range 100))
                (range 100 200))

  (check-equal? (sub2 4) 2)
  (check-equal? (sub126837 29384756) (- 29384756 126837))
  (check-equal? (map sub100 (range 100 200))
                (range 100))

  (define racketcon (range 1 10000))

  (check-equal? (sixteenth racketcon) 16)
  (check-equal? (sixtieth racketcon) 60)
  (check-equal? (nine-hundred-and-ninety-ninth racketcon) 999)

  (check-true (nineteen-eighty-four? 1984))

  (check-equal? (nine-hundred-and-ninety-ninth racketcon) 999)
  (check-equal? (nine-hundred-and-ninth racketcon) 909)

  (check-equal? (sequence-nine-thousand-nine-hundred-and-ninety-ninth (in-naturals 1)) 9999)

  (check-equal? nine-hundred-and-ninety-nine-decillion-nine-hundred-and-ninety-nine-nonillion-nine-hundred-and-ninety-nine-octillion-nine-hundred-and-ninety-nine-septillion-nine-hundred-and-ninety-nine-sextillion-nine-hundred-and-ninety-nine-quintillion-nine-hundred-and-ninety-nine-quadrillion-nine-hundred-and-ninety-nine-trillion-nine-hundred-and-ninety-nine-billion-nine-hundred-and-ninety-nine-million-nine-hundred-and-ninety-nine-thousand-nine-hundred-and-ninety-nine
                (sub1 (expt 10 36)))

  (check-equal? nine-hundred-thousand-million-billion
                (* 9 100 1000 one-million one-billion))

  (check-equal? nine-hundred-and-ninety-nine-decillion-decillion-nine-hundred-and-ninety-nine-nonillion-decillion
                (* 999999 (expt 10 63)))

  (check-equal? one-duodecillion (expt 10 39))
  (check-equal? two-duodecillion (* 2 one-duodecillion))
  (check-equal? ten-undecillion-vigintillion (expt 10 100))

  (check-equal? seventy-six-septillion
                seventy-six-million-billion-billion)

  (define (adópengő->forint ap)
    (/ ap two-hundred-million))

  (define (pengő->forint p)
    (/ p four-hundred-octillion))

  (check-equal? (pengő->forint seventy-six-septillion)
                (/ 19 one-hundred-thousand))

  (check-equal? (adópengő->forint five-hundred-million)
                5/2)

  (check-equal? one-oh-five 105)
  (check-equal? four-ninety-seven 497)
  (check-equal? ten-fifty-nine 1059)
  (check-equal? nineteen-oh-five 1905)
  (check-equal? twenty-one-oh-eight 2108)

  (check-equal? (vector-last (vector 1 2 3 4)) 4)
  (check-equal? (vector-second (vector 1 2)) 2)
  (check-equal? (bytes-last #"abcd") 100)

  (check-equal? (string-last "abcdef") #\f)
  (check-true (string-empty? ""))

  (check-false (non-empty-list? null))
  (check-false (non-empty-list? #f))
  (check-true (non-empty-list? (list 1)))

  )
