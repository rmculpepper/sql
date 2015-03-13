#lang racket/base
(require (for-syntax racket/base
                     (rename-in syntax/parse [attribute $])
                     "parse.rkt")
         "emit.rkt")
(provide (all-defined-out))

(define-syntax (select stx)
  (syntax-parse stx
    [:SelectInner
     #`(statement->string (quote #,($ ast)))]))

(define-syntax (insert stx)
  (syntax-parse stx
    [:InsertInner
     #`(statement->string (quote #,($ ast)))]))

(define-syntax (update stx)
  (syntax-parse stx
    [:UpdateInner
     #`(statement->string (quote #,($ ast)))]))

(define-syntax (delete stx)
  (syntax-parse stx
    [:DeleteInner
     #`(statement->string (quote #,($ ast)))]))
