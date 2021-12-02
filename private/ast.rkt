;; Run-time SQL ast structures

#lang racket/base
(require racket/string
         (rename-in racket/match [match-define defmatch])
         racket/format)
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

;; The data definitions in the rest of this module use the following
;; convention: variants that exist both at compile time and run time
;; are marked with a "-"; variants that exist only at compile time are
;; marked with "*"; and variants that exist only at run time are
;; marked with "+".

;; ----------------------------------------
;; DDL Statements

(define (ddl-ast? x)
  (or (ddl:create-table? x)
      (ddl:create-table-as? x)
      (ddl:create-view? x)))

;; A DDL is one of
;; - (ddl:create-table Name Boolean Boolean (Listof Column) (Listof Constraint))
;; - (ddl:create-table-as Name Boolean Boolean Statement)
;; - (ddl:create-view Name Statement)
;; A Column is (column Ident ScalarExpr Boolean (U #f ScalarExpr))
(struct ddl:create-table (name temp? ifnotexists? columns constraints) #:prefab)
(struct ddl:create-table-as (name temp? ifnotexists? select) #:prefab)
(struct ddl:create-view (name temp? rhs) #:prefab)
(struct column (name type not-null? maybe-default) #:prefab)

;; A Constraint is one of
;; - (constraint:named Ident Constraint)
;; - (constraint:check ScalarExpr)
;; - (constraint:primary-key (Listof Ident))
;; - (constraint:unique (Listof Ident))
;; - (constraint:references (Listof Ident) Name (U (Listof Ident) #f) (U #f Action) (U #f Action))
;; An Action is one of
;; - 'set-null 
;; - 'set-default 
;; - 'cascade 
;; - 'restrict 
;; - 'no-action
(struct constraint:named (name constraint) #:prefab)
(struct constraint:check (expr) #:prefab)
(struct constraint:primary-key (columns) #:prefab)
(struct constraint:unique (columns) #:prefab)
(struct constraint:references (columns foreign-table foreign-columns on-delete on-update) #:prefab)

;; ----------------------------------------
;; Statements

(define (statement-ast? x)
  (or (statement:with? x)
      (statement:select? x)
      (statement:insert? x)
      (statement:update? x)
      (statement:delete? x)))

(define (select-like-statement? x)
  (match x
    [(? statement:select?) #t]
    [(statement:with rec? headers rhss body)
     (and (andmap select-like-statement? rhss)
          (select-like-statement? body))]
    [_ #f]))

;; ----------------------------------------
;; With

;; A With is
;; (statement:with Boolean (Listof WithHeader) (Listof Select) Statement)
;; where WithHeader = (cons Ident (U #f (Listof Ident)))
(struct statement:with (rec? headers rhss body) #:prefab)

;; ----------------------------------------
;; Select

;; A Select is 
;; (statement:select (U 'all 'distinct #f) (Listof SelectItem)
;;                   (Listof TableRef) (Listof ScalarExpr)
;;                   (Listof Name) (Listof ScalarExpr) (U SelectExtension #f))
(struct statement:select (distinct vals from where groupby having ext) #:prefab)

;; A SelectItem is one of
;; - (select-item:as ScalarExpr Ident)
;; - (select-item:all)
;; * (list 'unquote Syntax)
;; + (Listof SelectItem)
;; - ScalarExpr
(struct select-item:as (expr name) #:prefab)
(struct select-item:all () #:prefab)

(define (select-item-ast? v)
  (or (select-item:as? v)
      (select-item:all? v)
      (and (list? v) (andmap select-item-ast? v))
      (scalar-expr-ast? v)))

;; A SelectExtension is
;; (select:extension (Listof SelectOrder) (U ScalarExpr #f) (U ScalarExpr #f))
(struct select:extension (order limit offset) #:prefab)

;; A SelectOrder is (select:order ScalarExpr (U 'asc 'desc #f))
(struct select:order (column asc/desc) #:prefab)

;; ----------------------------------------
;; Insert

;; An Insert is (statement:insert Name (U (Listof Ident) #f) TableExpr (U 'ignore #f))
(struct statement:insert (table columns source on-conflict) #:prefab)

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
;; * (table-ref:inject (list 'unquote Syntax))
;; - (table-ref:inject String)
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

;; A TableExpr is one of
;; - (table-expr:cross-join TableRef TableRef)
;; - (table-expr:join JoinType TableRef TableRef ???)
;; - (table-expr:set-op SetOp TableRef TableRef ?? ??)
;; - (table-expr:values (Listof (Listof ScalarExpr)))
;; - (table-expr:select SelectStatement)
;; * (list 'unquote Syntax)
;; * (table-expr:inject (list 'unquote Syntax))
;; - (table-expr:inject String)

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

;; check-same-length : (Listof (Listof Any)) -> Boolean
;; Returns #t IFF all inner lists are the same length.
;; This is needed as a check for table-expr:values
;; at both compile-time and runtime
;; (i.e. for syntax like (values* (1 2 3) (4 5 6))
;; in the TableExpr syntax class
;; and the make-values*-table-expr-ast function).
(define check-same-length
  (match-lambda
    ['() #t]
    [(cons this more)
     (define len0
       (length this))
     (for/and ([this (in-list more)])
       (= len0 (length this)))]))

;; ----------------------------------------
;; Scalar Expressions

;; Treat types as scalar expressions too...

;; A ScalarExpr is one of
;; - Name
;; - ExactInteger
;; - String
;; - (scalar:app (U Name Symbol) (Listof ScalarExpr))
;; - (scalar:table TableExpr)
;; - (scalar:case (Listof (cons ScalarExpr ScalarExpr)) ScalarExpr)
;; - (scalar:case-of ScalarExpr (Listof (cons ScalarExpr ScalarExpr)) ScalarExpr)
;; - (scalar:exists TableExpr)
;; - (scalar:in-table ScalarExpr TableExpr)
;; - (scalar:in-values ScalarExpr (Listof ScalarExpr))
;; - (scalar:some/all Symbol ScalarExpr (U 'some 'all) (U TableExpr ScalarExpr))
;; - (scalar:placeholder)
;; * (list 'unquote Syntax)
;; * (scalar:inject (list 'unquote Syntax))
;; - (scalar:inject String)
;; * (scalar:unquote (list 'unquote Syntax))  -- to be converted to placeholder (w/ expression)
;; + (scalar:unquote Any)     -- to be converted to placeholder (w/ value)
(struct scalar:app (op args) #:prefab)
(struct scalar:table (te) #:prefab)
(struct scalar:case (cases else) #:prefab)
(struct scalar:case-of (value cases else) #:prefab)
(struct scalar:exists (te) #:prefab)
(struct scalar:in-table (e1 e2) #:prefab)
(struct scalar:in-values (e1 es2) #:prefab)
(struct scalar:some/all (op e1 quant e2) #:prefab)
(struct scalar:placeholder () #:prefab)
(struct scalar:inject (s) #:prefab)
(struct scalar:unquote (expr) #:prefab)

(define (scalar-expr-ast? x)
  (or (name-ast? x)
      (exact-integer? x)
      (string? x)
      (scalar:app? x)
      (scalar:table? x)
      (scalar:case? x)
      (scalar:case-of? x)
      (scalar:exists? x)
      (scalar:in-table? x)
      (scalar:in-values? x)
      (scalar:some/all? x)
      (scalar:placeholder? x)
      (scalar:inject? x)
      (scalar:unquote? x)))

;; An Arity is one of
;; - Nat
;; - (list Nat ...)   -- multiple arities
;; - (box Nat)        -- arity at least

(define (arity-includes? a n)
  (cond [(box? a) (>= n (unbox a))]
        [(list? a) (member n a)]
        [else (= n a)]))

(define (arity->string a)
  (cond [(box? a) (format "~a or more arguments" (unbox a))]
        [(list? a) (string-join (map ~a a) ", " #:before-last ", or" #:after-last "arguments")]
        [(= a 1) "1 argument"]
        [else (format "~a arguments" a)]))

;; An OpEntry is one of
;; - (list Symbol Arity)
;; - (list Regexp Arity)

(define operator-symbol-rx ;; disallow "--"
  #rx"^(?:[~!@#%^&*_=+|<>?/]|-(?!-))+$")

(define (operator-symbol? sym)
  (regexp-match? operator-symbol-rx (symbol->string sym)))

;; This table should contain a superset of the tables of dialect.rkt.

(define standard-arities
  `(;; Functions
    [cast           2]
    [extract        2]
    [overlay    (3 4)]
    [position       2]
    [substring  (2 3)]
    [trim-leading   2]
    [trim-trailing  2]
    [trim-both      2]
    [count-all      0]
    [typeof         2]

    ;; Operators
    [||           #&1]
    [\|\|         #&1]
    [+            #&1]
    [-            #&2]
    [*            #&1]
    [/            #&1]
    [and          #&1]
    [or           #&1]
    [not            1]
    [is-null        1]
    [is-not-null    1]
    [is-true        1]
    [is-not-true    1]
    [is-false       1]
    [is-not-false   1]
    [is-unknown     1]
    [is-not-unknown 1]
    [collate        2]
    [distinct-from  2]
    [between-and    3]
    [like       (2 3)]
    [ilike      (2 3)]
    [similar-to (2 3)]
    ;; Treat any other symbol composed of just the following
    ;; characters as a non-chaining binary operator.
    [,operator-symbol-rx 2]

    ;; Field reference
    ;; (.field x)    "x.field"
    [#rx"^[.]([a-zA-Z_][a-zA-Z_0-9]*)$" 1]
    ;; (.*) = "*", (.* t) = "t.*"
    [.*         (0 1)]

    ;; Other notations
    [%ref         #&2]
    [%row         #&2]
    [row          #&0]
    [%array       #&1]
    ))

(define (op-entry op)
  (let loop ([ops standard-arities])
    (cond [(null? ops) #f]
          [(symbol? (caar ops))
           (cond [(eq? op (caar ops))
                  (car ops)]
                 [else (loop (cdr ops))])]
          [(regexp? (caar ops))
           (cond [(regexp-match (caar ops) (symbol->string op))
                  (car ops)]
                 [else (loop (cdr ops))])])))

(define (op-arity op-name)
  (cond [(op-entry op-name) => cadr]
        [else #f]))

;; ----------------------------------------

;; A Name is one of
;; - Ident                -- unqualified name
;; - (qname Name Ident)   -- qualified name
;; * (list 'unquote Syntax)
(struct qname (qual id) #:prefab)

(define (name-ast? x)
  (or (qname? x)
      (ident-ast? x)))

;; An Ident is one of
;; - (id:normal Symbol)   -- to be transmitted unquoted
;; - (id:literal String)  -- to be quoted when emitted
;; * (list 'unquote Syntax)
(struct id:normal (s) #:prefab)
(struct id:quoted (s) #:prefab)

(define (ident-ast? x)
  (or (id:normal? x)
      (id:quoted? x)))
