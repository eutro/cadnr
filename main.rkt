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
                (range 100)))
