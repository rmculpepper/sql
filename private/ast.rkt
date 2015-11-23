;; Run-time SQL ast structures

#lang racket/base
(require racket/string
         (rename-in racket/match [match-define defmatch])
         racket/generic
         racket/class
         racket/format
         "jumble.rkt")
(provide (all-defined-out))

;; ============================================================
;; Abstract Nonterminals

;; The AST datatypes used prefab structs so that a compile-time AST
;; can be turned into a run-time AST simply via quoting (or
;; quasiquoting, to support run-time splicing).

;; A nonterminal NT may support the following additional forms for
;; dynamic ast composition and SQL injection:
;; - (NT:AST ,ast-expr)
;; - (NT:INJECT String)
;; - (NT:INJECT ,string-expr)
;; For those NTs, the corresponding AST type contains the following variants:
;; - (list 'unquote Syntax)               -- represents case (1)
;; - (NT:inject String)                   -- represents case (2)
;; - (NT:inject (list 'unquote Syntax)))  -- represents case (3)
;; Only the second occurs at run-time, though.

;; Note for ScalarExpr: the three following forms are distinct:
;; - (select ,expr)                      -- turns into placeholder + value
;; - (select (ScalarExpr:AST ,expr))     -- splices ast result of expr
;; - (select (ScalarExpr:INJECT ,expr))  -- splices literal SQL code
;; And note that the first form is restricted to ScalarExpr.

;; ----------------------------------------
;; Statements

(define (statement-ast? x)
  (or (statement:select? x)
      (statement:insert? x)
      (statement:update? x)
      (statement:delete? x)))

;; ----------------------------------------
;; Select

;; A Select is 
;; (statement:select (Listof SelectItem) (Listof TableRef) (Listof ScalarExpr)
;;                   (Listof Name) (Listof ScalarExpr) (U SelectExtension #f))
(struct statement:select (vals from where groupby having ext) #:prefab)

;; A SelectItem is one of
;; - (select-item:as ScalarExpr Ident)
;; - (select-item:all)
;; - ScalarExpr
(struct select-item:as (expr name) #:prefab)
(struct select-item:all () #:prefab)

;; A SelectExtension is
;; (select:extension (Listof SelectOrder) (U ScalarExpr #f) (U ScalarExpr #f))
(struct select:extension (order limit offset) #:prefab)

;; A SelectOrder is (select:order ScalarExpr (U 'asc 'desc #f))
(struct select:order (column asc/desc) #:prefab)

;; ----------------------------------------
;; Insert

;; An Insert is (statement:insert Name (U (Listof Ident) #f) TableExpr)
(struct statement:insert (table columns source) #:prefab)

;; ----------------------------------------
;; Update

;; An Update is (statement:update Name (Listof UpdateAssign) (Listof ScalarExpr))
(struct statement:update (table assign where) #:prefab)

;; An UpdateAssign is (update:assign Ident ScalarExpr)
(struct update:assign (column expr) #:prefab)

;; ----------------------------------------
;; Delete

;; A Delete is (statement:delete Name (Listof ScalarExpr))
(struct statement:delete (table where) #:prefab)

;; ----------------------------------------
;; Table References

;; A TableRef is one of
;; - (table-ref:name Name)
;; - (table-ref:as TableExpr Ident)
;; * (list 'unquote Syntax)
;; * (table-ref:inject (U String (list 'unquote Syntax)))
;; - TableExpr

(struct table-ref:name (name) #:prefab)
(struct table-ref:as (e rangevar) #:prefab)
(struct table-ref:inject (sql) #:prefab)

(define (table-ref-ast? x)
  (or (table-ref:name? x)
      (table-ref:as? x)
      (table-ref:inject? x)
      (table-expr-ast? x)))

;; ----------------------------------------
;; Table Expressions

(struct table-expr:cross-join (t1 t2) #:prefab)
(struct table-expr:join (type t1 t2 on) #:prefab)
(struct table-expr:set-op (type t1 t2 opt corr) #:prefab)
(struct table-expr:values (rows) #:prefab)
(struct table-expr:select (select) #:prefab)
(struct table-expr:inject (sql) #:prefab)

(define (table-expr-ast? x)
  (or (join-table-expr? x)
      (nonjoin-table-expr? x)
      (table-expr:inject? x)))

;; Indicates whether a join is the primary connective.
(define (join-table-expr? x)
  (or (table-expr:cross-join? x)
      (table-expr:join? x)))
(define (nonjoin-table-expr? x)
  (or (table-expr:set-op? x)
      (table-expr:values? x)
      (table-expr:select? x)))

;; ----------------------------------------
;; Scalar Expressions

;; Treat types as scalar expressions too...

;; A ScalarExpr is one of
;; - (scalar:app Op (Listof ScalarExpr))
;; - (scalar:table TableExpr)
;; - (scalar:placeholder)
;; - Name
;; - ExactInteger
;; - String
;; * (list 'unquote Syntax)
;; * (scalar:inject (U String (list 'unquote Syntax)))
;; * (scalar:unquote Syntax)  -- to be converted to placeholder
(struct scalar:app (op args) #:prefab)
(struct scalar:placeholder () #:prefab)
(struct scalar:table (te) #:prefab)
(struct scalar:inject (s) #:prefab)
(struct scalar:unquote (expr) #:prefab)

(define (scalar-expr-ast? x)
  (or (scalar:app? x)
      (scalar:table? x)
      (scalar:placeholder? x)
      (name-ast? x)
      (exact-integer? x)
      (string? x)
      (scalar:inject? x)))

(define (infix-op-entry sym [op-string (~a " " sym " ")] #:arity [arity '(1)])
  (list sym arity (infix-op op-string)))
(define ((fun-op op-string #:arg-sep [arg-sep ", "]) . args)
  (J op-string "(" (J-join args arg-sep) ")"))
(define ((infix-op separator) . args)
  (J "(" (J-join args separator) ")"))

;; An OpEntry is one of
;; - (list Symbol Arity Formatter)
;; - (list Regexp (Symbol -> (list Arity Formatter)))
;; where Arity     = Nat | (Nat) -- latter indicates arity at least
;;       Formatter = String ... -> String

(define standard-ops
  `([cast      2  ,(fun-op "CAST" #:arg-sep " AS ")]
    ;; [coalesce (2) ,(fun-op "COALESCE")]
    [extract 2 ,(fun-op "EXTRACT" #:arg-sep " FROM ")]
    ,(infix-op-entry '|| " || ") ;; HACK! Note "||" reads as the empty symbol!
    ,(infix-op-entry '\|\| " || ")
    ,(infix-op-entry '+ " + ")
    ,(infix-op-entry '- " - ")
    ,(infix-op-entry '* " * ")
    ,(infix-op-entry '/ " / ")
    ,(infix-op-entry 'and " AND ")
    ,(infix-op-entry 'or  " OR ")
    [exists 1 ,(lambda (arg) (J "EXISTS " arg))]
    [not-exists 1 ,(lambda (arg) (J "NOT EXISTS " arg))]
    ;; Treat any other symbol composed of just the following
    ;; characters as a binary operator.
    [#rx"^[-~!@#$%^&*_=+|<>?/]+$"
     ,(lambda (op) (list 2 (infix-op (format " ~a " op))))]
    ;; Auto infix/suffix operators
    ;; (:like: x y)            "x LIKE y"
    ;; (:between:and: x y z)   "x BETWEEN y AND z"
    ;; (:is-null x)            "x IS NULL"
    ;; (:is-not-null x)        "x IS NOT NULL"
    [#rx"^[:][-a-zA-Z_]+(?:[:][-a-zA-Z_]+)*([:])?$"
     ,(lambda (op arg-follows?)
        (define parts (string-split op #rx"[:]" #:trim? #t))
        (list (+ (length parts) (if arg-follows? 1 0))
              (lambda args
                (J "(" (interleave args (map normalize-op-part parts)) ")"))))]
    ;; Modifiers (non-parenthesized)
    ;; (all% x)  "ALL x"
    [#rx"^[%]([a-zA-Z]+)$"
     ,(lambda (op part)
        (list 1 (lambda (arg) (J arg " " (normalize-op-part part)))))]
    [#rx"^([a-zA-Z]+)[%]$"
     ,(lambda (op part)
        (list 1 (lambda (arg) (J (normalize-op-part part) " " arg))))]
    ))

(define (normalize-op-part s)
  (string-upcase (string-replace s #rx"-" " ")))

(define (interleave as bs)
  (cond [(and (pair? as) (pair? bs))
         (list* (car as) " " (interleave bs (cdr as)))]
        [(and (pair? as) (null? bs))
         as]
        [else bs]))

(define (op-entry op)
  (let loop ([ops standard-ops])
    (cond [(null? ops) #f]
          [(symbol? (caar ops))
           (cond [(eq? op (caar ops))
                  (car ops)]
                 [else (loop (cdr ops))])]
          [(regexp? (caar ops))
           (cond [(regexp-match (caar ops) (symbol->string op))
                  => (lambda (m) (cons op (apply (cadar ops) m)))]
                 [else (loop (cdr ops))])])))

(define (op-formatter op-name)
  (cond [(op-entry op-name) => caddr]
        [else #f]))

(define (check-arity op-name n-args)
  (cond [(op-entry op-name)
         => (lambda (entry) (arity-includes? (cadr entry) n-args))]
        [else #t]))

(define (arity-includes? a n)
  (cond [(pair? a) (> n (car a))]
        [else (= n a)]))

;; ----------------------------------------

;; A Name is one of
;; - Ident                -- unqualified name
;; - (qname Name Ident)   -- qualified name
(struct qname (qual id) #:prefab)

(define (name-ast? x)
  (or (ident-ast? x)
      (qname? x)))

;; An Ident is one of
;; - Symbol               -- to be transmitted unquoted
;; - (id:literal String)  -- to be quoted when emitted
(struct id:quoted (s) #:prefab)

(define (ident-ast? x)
  (or (symbol? x)
      (id:quoted? x)))

(define (SQL-regular-id? s)
  (regexp-match? #rx"^[a-zA-Z][a-zA-Z0-9_]*$" s))
