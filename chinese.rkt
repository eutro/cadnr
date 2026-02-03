#lang racket/base

(provide chinese-numeral->number)

(require racket/match)

(define chinese-digit-table
  #hash((#\零 . 0)
        (#\〇 . 0)
        (#\一 . 1)
        (#\壹 . 1)
        (#\二 . 2)
        (#\贰 . 2)
        (#\三 . 3)
        (#\叁 . 3)
        (#\四 . 4)
        (#\肆 . 4)
        (#\五 . 5)
        (#\伍 . 5)
        (#\六 . 6)
        (#\陆 . 6)
        (#\七 . 7)
        (#\柒 . 7)
        (#\八 . 8)
        (#\捌 . 8)
        (#\九 . 9)
        (#\玖 . 9)
        (#\十 . 10)
        (#\拾 . 10)
        (#\百 . 100)
        (#\佰 . 100)
        (#\千 . 1000)
        (#\万 . 10000)
        (#\亿 . 100000000)))

(define (nxor a b)
  (or (and a b) (and (not a) (not b))))

(define (parse-positive-chinese-number-under-10000 lst last-位权)
  (let-values ([(lst has-preceding-zero?)
                (if (zero? (car lst)) (values (cdr lst) #t) (values lst #f))])
    (match lst
      [(list _) #:when (nxor has-preceding-zero? (< last-位权 100)) #f]
      [(list n) (and (< 0 n 10) n)]
      [(list* _ 位权 _) #:when (nxor has-preceding-zero? (< (/ last-位权 位权) 100)) #f]
      [(list n 位权) (* n 位权)]
      [(list* n 位权 rest)
       (define foo (parse-positive-chinese-number-under-10000 rest 位权))
       (and foo (+ (* n 位权) foo))])))

(define (sorted? lst cmp)
  (let loop ([lst lst])
    (match lst
      [(list* x1 x2 xs) (and (cmp x1 x2) (loop (cons x2 xs)))]
      [(or (list) (list _)) #t])))

(define (build-chinese-number lst)
  (match lst
    [(cons (cons (list* 1 10 _) _) _) #f]
    [_
     (define 补一-lst
       (match lst
         [(cons (cons (cons 10 foo) baz) bar) (cons (cons (list* 1 10 foo) baz) bar)]
         [_ lst]))
     (let loop ([lst 补一-lst] [acc 0] [last-位权 1])
       (match lst
         [(list) acc]
         [(cons (cons number-under-10000 位权) lst)
          (define base (parse-positive-chinese-number-under-10000 number-under-10000 last-位权))
          (and base (loop lst (+ acc (* base 位权)) 10000))]))]))

(define (parse-positive-chinese-number s)
  (define len (string-length s))
  (let loop ([i 0] [number-under-10000 null] [acc null])
    (cond
      [(= i len)
       (define lst
         (if (null? number-under-10000)
             (reverse acc)
             (reverse (cons (cons (reverse number-under-10000) 1) acc))))
       (and (sorted? (map cdr lst) >) (build-chinese-number lst))]
      [else
       (define c (string-ref s i))
       (case c
         [(#\万 #\亿)
          (define-values (new-i 位权)
            (let loop ([i (add1 i)] [acc (hash-ref chinese-digit-table c)])
              (if (and (< i len) (equal? (string-ref s i) #\亿))
                  (loop (add1 i) (* acc 100000000))
                  (values i acc))))
          (define new-acc
            (cons (cons (reverse number-under-10000) 位权) acc))
          (loop new-i null new-acc)]
         [else
          (define digit-value (hash-ref chinese-digit-table c #f))
          (and
           digit-value
           (loop (add1 i) (cons digit-value number-under-10000) acc))])])))

(define (parse-chinese-number s)
  (and
   (> (string-length s) 0)
   (let ([first-char-is-负? (equal? (string-ref s 0) #\负)])
     (cond
       [(member s '("〇" "零")) 0]
       [first-char-is-负?
        (define substring1 (substring s 1))
        (and (not (equal? substring1 "零")) (- (parse-positive-chinese-number substring1)))]
       [else (parse-positive-chinese-number s)]))))

(define (parse-chinese-number-maybe-ordinal s ordinal?)
  (and 
   (> (string-length s) 0)
   (let ([first-char-is-第? (equal? (string-ref s 0) #\第)])
     (cond
     [(and first-char-is-第? ordinal?) (parse-chinese-number (substring s 1))]
     [(and first-char-is-第? (not ordinal?)) #f]
     [else (parse-chinese-number s)]))))

(define (string-split-on-first-hyphen s)
  (define len (string-length s))
  (let loop ([i 0])
    (cond
      [(= i len) (values s "")]
      [else
       (define c (string-ref s i))
       (if (equal? c #\-)
           (values (substring s 0 (add1 i)) (substring s (add1 i)))
           (loop (add1 i)))])))

(define (string-maybe-trim-question-mark s)
  (define len (string-length s))
  (cond
    [(zero? len) (values s "")]
    [(equal? (string-ref s (sub1 len)) #\?) (values (substring s 0 (sub1 len)) "?")]
    [else (values s "")]))

(define (chinese-numeral->number s #:ordinal? [ordinal? #t])
  (let*-values ([(s pfx) (string-split-on-first-hyphen s)]
                [(s sfx) (string-maybe-trim-question-mark s)]
                [(n) (parse-chinese-number-maybe-ordinal s ordinal?)])
    (and n (list pfx n sfx))))
