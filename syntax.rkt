#lang racket/base
(require (for-syntax racket/base
                     (rename-in syntax/parse [attribute $])
                     "parse.rkt")
         "emit.rkt")
(provide (all-defined-out))

(begin-for-syntax
  (define (make-stmt-expr ast)
    ;; Use #'here lexical context for embedded unquote
    #`(statement->string (quasiquote #,(datum->syntax #'here ast)))))

(define-syntax (select stx)
  (syntax-parse stx
    [:SelectInner (make-stmt-expr ($ ast))]))

(define-syntax (insert stx)
  (syntax-parse stx
    [:InsertInner (make-stmt-expr ($ ast))]))

(define-syntax (update stx)
  (syntax-parse stx
    [:UpdateInner (make-stmt-expr ($ ast))]))

(define-syntax (delete stx)
  (syntax-parse stx
    [:DeleteInner (make-stmt-expr ($ ast))]))
