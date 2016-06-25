#lang racket/base

(require racket/function
         rackunit)

(provide (all-defined-out))

(define-syntax-rule (check-values-equal? . (a (b c ...)))
  (check-equal? (call-with-values (thunk a) list) (list b c ...)))

(define-syntax-rule (check-values-with-predicates . (a (b c ...)))
  (for ([value (call-with-values (thunk a) list)]
        [predicate (list b c ...)])
    (check-true (predicate value))))
