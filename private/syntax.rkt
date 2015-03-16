#lang racket/base
(require (for-syntax racket/base
                     (rename-in syntax/parse [attribute $])
                     "parse.rkt")
         "emit.rkt")
(provide (except-out (all-defined-out)
                     define-ast-macros))

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

;; ============================================================
;; ASTs

(define-syntax-rule (define-ast-macros [name nt] ...)
  (begin
    (define-syntax (name stx)
      (syntax-parse stx
        [(_ x) #:declare x nt #`(quasiquote #,(datum->syntax #'here ($ x.ast)))]))
    ...))

(define-ast-macros
  [SQL:Name Name]
  [SQL:Ident Ident]
  [SQL:TableRef TableRef]
  [SQL:TableExpr TableExpr]
  [SQL:ScalarExpr ScalarExpr]
  [SQL:Statement Statement]
  [SQL:Select Select])
