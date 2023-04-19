#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         syntax/parse/define)

(provide define-new-top-definer
         define-new-top)

(begin-for-syntax
  (define-syntax-class name-clause
    #:description "#%top name pattern clause"
    #:transparent
    (pattern [regex:regexp (group:id ...+) result:expr ...+])))

(define-syntax-parse-rule
  (define-new-top-definer define-new-top:id
    clauses:name-clause ...+)

  (define-syntax-parse-rule
    (define-new-top new-top:id old-top:id)

    (define-syntax (new-top stx)
      (syntax-parse stx
        #:track-literals
        [(_ . id:id)
         (define id-sym (syntax-e #'id))
         (define id-str (symbol->string id-sym))
         (cond
           [(regexp-match clauses.regex id-str)
            =>
            (lambda (cgs)
              (apply
               (lambda (clauses.group ...)
                 (syntax-property
                  (let () clauses.result ...)
                  'inferred-name id-sym))
               cgs))]
           ...
           [else (syntax/loc stx (old-top . id))])]
        [(_ . etc) (syntax/loc stx (old-top . etc))]))))

(define-syntax-parse-rule
  (define-new-top new-top:id old-top:id
    clauses:name-clause ...+)
  (begin
    (define-new-top-definer definer clauses ...)
    (definer new-top old-top)))
