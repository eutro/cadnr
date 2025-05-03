#lang racket/base

(require (for-syntax racket/base
                     racket/stxparam
                     racket/match
                     (for-syntax racket/base))
         syntax/parse/define)

(provide define-new-top-definer
         define-new-top
         (for-syntax this-syntax
                     this-id))

(begin-for-syntax
  (define-syntax-parameter this-id
    (lambda (stx)
      (raise-syntax-error #f "used out of context: not within a #%top definer" stx)))

  (define-logger cadnr)

  (define-splicing-syntax-class name-clause
    #:description "#%top name pattern clause"
    #:attributes {as-match-pattern}
    #:transparent
    (pattern {~seq regex:regexp (group:expr ...+)}
             #:with regex*
             (datum->syntax
              #'regex
              (if (pregexp? (syntax-e #'regex))
                  (pregexp (format "^(?:~a)$" (object-name (syntax-e #'regex))))
                  (regexp (format "^(?:~a)$" (object-name (syntax-e #'regex)))))
              #'regex)
             #:attr as-match-pattern
             #'(app (λ (it) (regexp-match 'regex* it)) (list group ...)))
    (pattern (form ...)
             #:attr as-match-pattern #'(form ...))))

(define-syntax-parse-rule
  (define-new-top-definer define-new-top:id
    [clause:name-clause . clause-body] ...+)

  (define-syntax-parse-rule
    (define-new-top new-top:id old-top:id)

    (define-syntax (new-top stx)
      (syntax-parse stx
        #:track-literals
        [(_ . id:id)
         (define id-val #'id)
         (define id-sym (syntax-e #'id))
         (define id-str (symbol->string id-sym))
         (define value
           (syntax-parameterize ([this-id (make-rename-transformer #'id-val)])
             (match id-str
               [clause.as-match-pattern . clause-body] ...
               [_ #f])))
         (cond
           [value
            (log-cadnr-debug "transformed: ~a" id-str)
            (syntax-property value 'inferred-name id-sym)]
           [else (syntax/loc stx (old-top . id))])]
        [(_ . etc) (syntax/loc stx (old-top . etc))]))))

(define-syntax-parse-rule
  (define-new-top new-top:id old-top:id
    clauses:name-clause ...+)
  (begin
    (define-new-top-definer definer clauses ...)
    (definer new-top old-top)))
