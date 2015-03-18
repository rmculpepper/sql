#lang racket/base
(require (for-syntax racket/base
                     (rename-in syntax/parse [attribute $])
                     "ast.rkt"
                     "parse.rkt")
         racket/class
         racket/match
         db/base
         unstable/custom-write
         "emit.rkt")
(provide (except-out (all-defined-out)
                     define-ast-macros))

;; ============================================================
;; Determine emit-sql based on db connection

(define (base-get-emit-sql obj)
  (cond [(connection? obj)
         (base-get-emit-sql (connection-dbsystem obj))]
        [(dbsystem? obj)
         (base-get-emit-sql (dbsystem-name obj))]
        [else
         (case obj
           [(postgresql) postgresql-emit-sql]
           [else standard-emit-sql])]))

(define current-get-emit-sql (make-parameter base-get-emit-sql))

(define (get-emit-sql c) ((current-get-emit-sql) c))

;; ----------------------------------------
;; Convenience functions

(define (statement->string s [obj #f])
  (send (get-emit-sql obj) statement->string s))
(define (table-ref->string t [obj #f])
  (send (get-emit-sql obj) table-ref->string t))
(define (table-expr->string t [obj #f])
  (send (get-emit-sql obj) table-expr->string t))
(define (scalar-expr->string e [obj #f])
  (send (get-emit-sql obj) scalar-expr->string e))

;; ============================================================
;; Helpers

(begin-for-syntax
  ;; Goal: convert scalar:unquotes (if any) into placeholders and
  ;; parameters; but they conflict with existing placeholders. Cases:
  ;; - exist scalar:placeholders, but no scalar:unquotes
  ;;   => convert to string
  ;; - exist scalar:unquotes, but no scalar:placeholders
  ;;   => convert to prop:statement instance => statement-binding
  ;; - exist both
  ;;   => error! (for now---eventually would like to support)
  ;; - exist neither
  ;;   => convert to string
  (define (make-stmt-expr stx ast)
    (define seen-placeholder? #f)
    (define r-unquoted-exprs null)
    ;; Generic traversal
    (define (loop x)
      (cond [(scalar:placeholder? x)
             (set! seen-placeholder? #t)
             x]
            [(scalar:unquote? x)
             (set! r-unquoted-exprs
                   (cons (scalar:unquote-expr x) r-unquoted-exprs))
             (scalar:placeholder)]
            ;; Generic traversal cases
            [(list? x)
             (map loop x)]
            [(prefab-struct-key x)
             => (lambda (key)
                  (define fields (cdr (vector->list (struct->vector x))))
                  (apply make-prefab-struct key (map loop fields)))]
            [else x]))
    (define ast* (loop ast))
    (cond [(and seen-placeholder? (pair? r-unquoted-exprs))
           (raise-syntax-error #f
             "statement contains both placeholders and value-unquotes"
             stx)]
          [(pair? r-unquoted-exprs)
           ;; Use #'here lexical context for embedded AST unquotes
           (with-syntax ([ast* (datum->syntax #'here ast*)]
                         [(unquoted-expr ...) (reverse r-unquoted-exprs)])
             #'(sql-statement (quasiquote ast*)
                              (list unquoted-expr ...)))]
          [else
           ;; Use #'here lexical context for embedded AST unquotes
           (with-syntax ([ast* (datum->syntax #'here ast*)])
             #'(sql-statement (quasiquote ast*) #f))])))

(struct sql-statement (ast args)
        #:property prop:statement
        (lambda (self c)
          (define sql (sql-statement-sql self c))
          (define pst (send c prepare 'sql-statement sql #t))
          (cond [(sql-statement-args self)
                 => (lambda (args) (send pst bind 'sql-statement args))]
                [else pst]))
        #:property prop:custom-write
        (make-constructor-style-printer
         (lambda (self) 'sql-statement)
         ;; FIXME: what if default emit-sql raises error on ast? should catch...
         (lambda (self)
           (cons (sql-statement-sql self #f)
                 (or (sql-statement-args self) null)))))

(define (sql-statement-sql s [obj #f])
  (match s
    [(sql-statement ast _)
     (statement->string ast obj)]))

;; ============================================================

(define-syntax (select stx)
  (syntax-parse stx
    [:SelectInner (make-stmt-expr stx ($ ast))]))

(define-syntax (insert stx)
  (syntax-parse stx
    [:InsertInner (make-stmt-expr stx ($ ast))]))

(define-syntax (update stx)
  (syntax-parse stx
    [:UpdateInner (make-stmt-expr stx ($ ast))]))

(define-syntax (delete stx)
  (syntax-parse stx
    [:DeleteInner (make-stmt-expr stx ($ ast))]))

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
