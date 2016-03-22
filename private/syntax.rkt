#lang racket/base
(require (for-syntax racket/base
                     (rename-in syntax/parse [attribute $])
                     "ast.rkt"
                     "parse.rkt")
         racket/class
         racket/serialize
         racket/match
         db/base
         racket/struct
         "ast.rkt"
         "emit.rkt")
(provide (all-defined-out))

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
           [(mysql) mysql-emit-sql]
           [(sqlite3) sqlite3-emit-sql]
           ;; FIXME: ODBC?
           [else standard-emit-sql])]))

(define current-get-emit-sql (make-parameter base-get-emit-sql))

(define (get-emit-sql c) ((current-get-emit-sql) c))

(define current-sql-dialect (make-parameter #f))

;; ----------------------------------------
;; Convenience functions

(define (sql-ast->string e [dialect (current-sql-dialect)])
  (define emit (get-emit-sql dialect))
  (cond [(name-ast? e)
         (send emit name->string e)]
        [(scalar-expr-ast? e)
         (send emit scalar-expr->string e)]
        [(table-expr-ast? e)
         (send emit table-expr->string e)]
        [(table-ref-ast? e)
         (send emit table-ref->string e)]
        [(statement-ast? e)
         (send emit statement->string e)]
        [(ddl-ast? e)
         (send emit statement->string e)]))

(define (statement-ast->string e [dialect (current-sql-dialect)])
  (define emit (get-emit-sql dialect))
  (cond [(statement-ast? e)
         (send emit statement->string e)]
        [(ddl-ast? e)
         (send emit statement->string e)]
        [else
         (error 'statement-ast->string "bad value: ~e\n" e)]))

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
             "cannot use both placeholders and unquoted values"
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

(serializable-struct sql-statement (ast args)
        #:property prop:statement
        (lambda (self c)
          (define sql (sql-statement->string self c))
          (define pst (send c prepare 'sql-statement sql #t))
          (cond [(sql-statement-args self)
                 => (lambda (args) (send pst bind 'sql-statement args))]
                [else pst]))
        #:property prop:custom-write
        (make-constructor-style-printer
         (lambda (self) 'sql-statement)
         (lambda (self)
           (cons (with-handlers ([exn:fail? (lambda (e) "... not in current dialect ...")])
                   (sql-statement->string self (current-sql-dialect)))
                 (or (sql-statement-args self) null)))))

(define (sql-statement->string s [obj (current-sql-dialect)])
  (match s
    [(sql-statement ast _)
     (statement-ast->string ast obj)]))

;; ============================================================

(define-syntax (with stx)
  (syntax-parse stx
    [:WithInner (make-stmt-expr stx ($ ast))]))

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
;; DDL

(define-syntax (create-table stx)
  (syntax-parse stx
    [:CreateTableInner (make-stmt-expr stx ($ ast))]))

(define-syntax (create-view stx)
  (syntax-parse stx
    [:CreateViewInner (make-stmt-expr stx ($ ast))]))

;; ============================================================
;; ASTs

(define-syntax-rule (define-ast-macros [name nt] ...)
  (begin
    (define-syntax (name stx)
      (syntax-parse stx
        [(_ x) #:declare x nt #`(quasiquote #,(datum->syntax #'here ($ x.ast)))]))
    ...))

(define-ast-macros
  [name-qq Name]
  [ident-qq Ident]
  [table-ref-qq TableRef]
  [table-expr-qq TableExpr]
  [scalar-expr-qq ScalarExpr]
  [statement-qq Statement]
  [select-qq Select]
  [ddl-qq DDL])
