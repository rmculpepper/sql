#lang racket/base
(require "ast.rkt"
         (submod "parse.rkt" names))
(provide (all-defined-out))

;; make-ident-ast : (U String Symbol) -> Ident-AST
(define (make-ident-ast s)
  (cond [(string? s) (id:quoted s)]
        [(symbol? s) (id:normal s)]))

;; make-name-ast : (Rec X (U Symbol Name-AST (Listof X))) -> Name-AST
(define (make-name-ast s)
  (let loop ([x s])
    (cond [(symbol? x)
           (or (parse-name x) (error 'make-name-ast "cannot parse name: ~e" s))]
          [(name-ast? x) x]
          [(list? x) (name-list->name (map make-name-ast x))])))

;; value->scalar-expr-ast : Any -> scalar-expr-ast?
(define (value->scalar-expr-ast value)
  ;; note: equivalent to (scalar-expr-qq ,value)
  (scalar:unquote value))
