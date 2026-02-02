#lang racket/base

(require racket/string
         racket/list
         racket/match
         syntax/parse/define
         (for-syntax racket/base))

(provide words->number)

(struct word (cardinal ordinal value))
(struct leaf (value ordinal?) #:transparent)

(define (build-table . words)
  (define leaves
    (apply
     append
     (for/list ([w (in-list words)])
       (define val (word-value w))
       (list
        (cons (word-cardinal w) (leaf val #f))
        (cons (word-ordinal w) (leaf val #t))))))

  (for/hash ([lf (in-list leaves)])
    (values (car lf) (cdr lf))))

(define-syntax (build-word-table stx)
  (define-syntax-class word-decl
    #:attributes {w}
    (pattern [cardinal:str value]
             #:with ordinal (datum->syntax #'cardinal (format "~ath" (syntax-e #'cardinal)))
             #:attr w #'(word cardinal ordinal value))
    (pattern [cardinal:str ordinal:str value]
             #:attr w #'(word cardinal ordinal value)))
  (syntax-parse stx [(_ w:word-decl ...) #'(build-table w.w ...)]))

(define word-table
  (build-word-table
   ["zero" 0] ["and" 'and] ["oh" 'oh]

   ["one" "first" 1] ["two" "second" 2] ["three" "third" 3] ["four" 4]
   ["five" "fifth" 5] ["six" 6] ["seven" 7] ["eight" "eighth" 8] ["nine" "ninth" 9]
   ["ten" 10] ["eleven" 11] ["twelve" "twelfth" 12] ["thirteen" 13] ["fourteen" 14]
   ["fifteen" 15] ["sixteen" 16] ["seventeen" 17] ["eighteen" 18] ["nineteen" 19]
   ["twenty" "twentieth" 20] ["thirty" "thirtieth" 30] ["forty" "fortieth" 40] ["fifty" "fiftieth" 50]
   ["sixty" "sixtieth" 60] ["seventy" "seventieth" 70] ["eighty" "eightieth" 80] ["ninety" "ninetieth" 90]

   ;; short-scale
   ["hundred" 100]
   ["thousand"      (expt 10  3)] ["million"        (expt 10  6)] ["billion"           (expt 10  9)]
   ["trillion"      (expt 10 12)] ["quadrillion"    (expt 10 15)] ["quintillion"       (expt 10 18)]
   ["sextillion"    (expt 10 21)] ["septillion"     (expt 10 24)] ["octillion"         (expt 10 27)]
   ["nonillion"     (expt 10 30)] ["decillion"      (expt 10 33)] ["undecillion"       (expt 10 36)]
   ["duodecillion"  (expt 10 39)] ["tredecillion"   (expt 10 42)] ["quattuordecillion" (expt 10 45)]
   ["quindecillion" (expt 10 48)] ["sexdecillion"   (expt 10 51)] ["septendecillion"   (expt 10 54)]
   ["octodecillion" (expt 10 57)] ["novemdecillion" (expt 10 60)] ["vigintillion"      (expt 10 63)]
   ;; v uses about 128 bytes
   ["centillion"    (expt 10 303)]))

(define (token->leaf tok)
  (hash-ref word-table tok #f))

(define (tokenize s)
  (match (regexp-match #px"^(.*?[^a-z-]|)([a-z]+(?:-[a-z]+)*)([^a-z-].*|)$" s)
    [(list _ untok-pfx tokenizable untok-sfx)
     (define (join-or-empty fmt toks)
       (if (null? toks)
           ""
           (format fmt (string-join toks "-"))))

     (match (string-split tokenizable "-")
       [(list (and (app token->leaf #f) pfx) ...
              (app token->leaf (? leaf? toks)) ...
              (and (app token->leaf #f) sfx) ...)
        (values toks
                (string-append untok-pfx (join-or-empty "~a-" pfx))
                (string-append (join-or-empty "-~a" sfx) untok-sfx))])]
    [_ (values null s "")]))

(define-match-expander <=?
  (syntax-parser
    [(_ left:number pat:expr right:number)
     #'(? (λ (it) (and (number? it) (<= left it right))) pat)]
    [(_ left:number pat:expr)
     #'(? (λ (it) (and (number? it) (<= left it))) pat)]
    [(_ pat:expr right:number)
     #'(? (λ (it) (and (number? it) (<= it right))) pat)]))

(define (tokens->number tokens ordinal?)
  (let/ec return
    (define (mills toks acc)
      (match toks
        ;; |ninety-nine-...
        [(list* (<=? 20 tens 90) (<=? 1 ones 9) tail)
         (mills-or-hundreds-prefix (+ tens ones) tail acc)]
        ;; |thirty-...
        [(list* (<=? 1 num 90) tail)
         (mills-or-hundreds-prefix num tail acc)]
        ;; |million-...
        [_ (return #f)]))

    (define (mills-or-empty toks acc)
      (match toks
        [(list) acc]
        [_ (mills toks acc)]))

    (define (mills-or-hundreds-prefix pfx toks acc)
      ;; 1 <= pfx < 100
      (match toks
        ;; ninety-nine|
        ;; ten|
        [(list) (+ acc pfx)]
        ;; five-|hundred-...
        [(list* 100 tail)
         (hundreds-prefix (* 100 pfx) tail acc)]
        [_ (mills-prefix pfx toks acc)]))

    (define (mills-prefix pfx toks acc)
      ;; 1 <= pfx < 1000
      (match toks
        ;; one-hundred-and-two|
        [(list) (+ acc pfx)]
        ;; one-|million-and-...
        [(list* (<=? 1000 mill) 'and tail)
         #:when (or (zero? acc) (< mill acc))
         (mills tail (+ acc (* pfx mill)))]
        ;; one-|million-million-...
        [(list* (<=? 1000 mill) (<=? 1000 mill2) tail)
         #:when (and (<= mill mill2) (or (zero? acc) (< mill2 acc)))
         (mills-prefix (* pfx mill) (cons mill2 tail) acc)]
        ;; ten-|thousand-...
        [(list* (<=? 1000 mill) tail)
         #:when (or (zero? acc) (< mill acc))
         (mills-or-empty tail (+ acc (* pfx mill)))]
        ;; one-hundred-and-two-|one
        [_ (return #f)]))

    (define (hundreds-prefix pfx toks acc)
      ;; 100 <= pfx
      (match toks
        ;; five-hundred|
        [(list) (+ acc pfx)]
        ;; one-hundred-|and-thirty-two-...
        [(list* 'and (<=? 20 tens 90) (<=? 1 ones 9) tail)
         (mills-prefix (+ pfx tens ones) tail acc)]
        ;; one-hundred-|and-five-...
        [(list* 'and (<=? 1 num 90) tail)
         (mills-prefix (+ pfx num) tail acc)]
        ;; one-hundred-|thirty-two-...
        [(list* (<=? 20 tens 90) (<=? 1 ones 9) tail)
         (mills-prefix (+ pfx tens ones) tail acc)]
        ;; one-hundred-|five-...
        [(list* (<=? 1 num 90) tail)
         (mills-prefix (+ pfx num) tail acc)]
        ;; one-hundred-|thousand-...
        [_ (mills-prefix pfx toks acc)]))

    (when (or (null? tokens)
              ;; ordinal? means the last token must be in ordinal form
              ;; like "first" or "ninetieth"
              (not (eq? ordinal? (leaf-ordinal? (last tokens))))
              (ormap leaf-ordinal? (drop-right tokens 1)))
      (return #f))

    (define nums (map leaf-value tokens))
    (match nums
      ;; zero is only ok on its own
      [(list 0) 0]
      [(list* _ ... 0 _) #f]
      ;; handle years like twenty-twenty-five
      [(or
        ;; |twenty-two-...
        (list* (<=? 20 century-tens 90) (<=? 1 century-units 9) tail)
        ;; |four-...
        (list* (and (<=? 1 century-units 90) (app (λ (_) 0) century-tens)) tail))
       (=> fail)
       (define century (* (+ century-tens century-units) 100))
       (match tail
         ;; nineteen-|eighty-four
         [(list (<=? 20 decade 90) (<=? 1 year 9))
          (+ century decade year)]
         ;; nineteen-|oh-nine
         [(list 'oh (<=? 1 year 9))
          (+ century year)]
         ;; nineteen-|thirty
         [(list (<=? 10 year 90))
          (+ century year)]
         ;; twenty-|five -> go to normal parsing
         [_ (fail)])]
      ;; start parsing X-hundred-Yty-Z-million-...
      [_ (mills nums 0)])))

;; Split s into (list prefix number suffix)
(define (words->number s #:ordinal? [ordinal? #f])
  (define-values (tokens pfx sfx) (tokenize s))
  (define num (tokens->number tokens ordinal?))
  (if num (list pfx num sfx) #f))
