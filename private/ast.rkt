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
;; DDL Statements

(define (ddl-ast? x)
  (ddl:create-table? x)
  (ddl:create-table-as? x)
  (ddl:create-view? x))

;; A DDL is one of
;; - (ddl:create-table Ident Boolean (Listof Column) (Listof Constraint))
;; - (ddl:create-table-as Ident Boolean Statement)
;; - (ddl:create-view Ident Statement)
;; A Column is (column Ident ScalarExpr Boolean)
(struct ddl:create-table (name temp? columns constraints) #:prefab)
(struct ddl:create-table-as (name temp? select) #:prefab)
(struct ddl:create-view (name rhs) #:prefab)
(struct column (name type not-null?) #:prefab)

;; A Constraint is one of
;; - (constraint:named Ident Constraint)
;; - (constraint:check ScalarExpr)
;; - (constraint:primary-key (Listof Ident))
;; - (constraint:unique (Listof Ident))
;; - (constraint:references (Listof Ident) Name (U (Listof Ident) #f))
(struct constraint:named (name constraint) #:prefab)
(struct constraint:check (expr) #:prefab)
(struct constraint:primary-key (columns) #:prefab)
(struct constraint:unique (columns) #:prefab)
(struct constraint:references (columns foreign-table foreign-columns) #:prefab)

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
;; - Name
;; - ExactInteger
;; - String
;; - (scalar:app Op (Listof ScalarExpr))
;; - (scalar:table TableExpr)
;; - (scalar:case (Listof (cons ScalarExpr ScalarExpr)) ScalarExpr)
;; - (scalar:case-of ScalarExpr (Listof (cons ScalarExpr ScalarExpr)) ScalarExpr)
;; - (scalar:exists TableExpr)
;; - (scalar:in ScalarExpr TableExpr)
;; - (scalar:some/all Boolean ScalarExpr Op (U TableExpr ScalarExpr))
;; - (scalar:placeholder)
;; * (list 'unquote Syntax)
;; * (scalar:inject (U String (list 'unquote Syntax)))
;; * (scalar:unquote Syntax)  -- to be converted to placeholder
(struct scalar:app (op args) #:prefab)
(struct scalar:table (te) #:prefab)
(struct scalar:case (cases else) #:prefab)
(struct scalar:case-of (value cases else) #:prefab)
(struct scalar:exists (te) #:prefab)
(struct scalar:in (e1 e2) #:prefab)
(struct scalar:some/all (all? e1 op e2) #:prefab)
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
      (scalar:in? x)
      (scalar:some/all? x)
      (scalar:placeholder? x)
      (scalar:inject? x)))

(define ((fun-op op-string #:arg-sep [arg-sep ", "]) . args)
  (J op-string "(" (J-join args arg-sep) ")"))

(define ((weird-fun-op op-string arg-prefixes) . args)
  (J op-string "("
     (for/list ([prefix (in-list arg-prefixes)] [arg (in-list args)])
       (if prefix (J prefix arg) arg))
     ")"))

(define (infix-op-entry sym [op-string (~a " " sym " ")] #:arity [arity '#&1])
  (list sym arity (infix-op op-string)))
(define ((infix-op separator) . args)
  (J "(" (J-join args separator) ")"))

(define ((outfix-op separators) . args)
  (J "(" (car args)
     (for/list ([arg (in-list (cdr args))]
                [separator (in-list separators)])
       (J separator arg))
     ")"))
(define ((prefix-op prefix) arg)   (J "(" prefix arg ")"))
(define ((postfix-op postfix) arg) (J "(" arg postfix ")"))


;; An OpEntry is one of
;; - (list Symbol Arity Formatter)
;; - (list Regexp (Symbol -> (list Arity Formatter)))
;; where Arity     = Nat | (Nat ...) | (Box Nat) -- latter indicates arity at least
;;       Formatter = String ... -> String

;; (define operator-symbol-rx #rx"^[-~!@#%^&*_=+|<>?/]+$")

(define operator-symbol-rx
  ;; disallow "--"
  (let ([chars "[~!@#%^&*_=+|<>?/]"])
    (regexp (format "^(?:~a|[-](?:~a|$))+$" chars chars))))


(define standard-ops
  `(;; Functions
    [cast         2  ,(weird-fun-op "CAST" '(#f " AS "))]
    [extract      2  ,(weird-fun-op "EXTRACT" '(#f " FROM "))]
    [overlay   (3 4) ,(weird-fun-op "OVERLAY" '(#f " PLACING " " FROM " " FOR "))]
    [position     2  ,(weird-fun-op "POSITION" '(#f " IN "))]
    [substring (2 3) ,(weird-fun-op "SUBSTRING" '(#f "FROM" "FOR"))]
    [trim-leading  2 ,(lambda (arg1 arg2) (J "TRIM(LEADING "  arg1 " FROM " arg2 ")"))]
    [trim-trailing 2 ,(lambda (arg1 arg2) (J "TRIM(TRAILING " arg1 " FROM " arg2 ")"))]
    [trim-both     2 ,(lambda (arg1 arg2) (J "TRIM(BOTH "     arg1 " FROM " arg2 ")"))]
    [count-all     0 ,(lambda () "COUNT(*)")]

    ;; Operators
    ,(infix-op-entry '|| " || ") ;; HACK! Note "||" reads as the empty symbol!
    ,(infix-op-entry '\|\| " || ")
    ,(infix-op-entry '+ " + ")
    ,(infix-op-entry '- " - ")
    ,(infix-op-entry '* " * ")
    ,(infix-op-entry '/ " / ")
    ,(infix-op-entry 'and " AND ")
    ,(infix-op-entry 'or  " OR ")
    ;; Treat any other symbol composed of just the following
    ;; characters as a non-chaining binary operator.
    [,operator-symbol-rx
     ,(lambda (op) (list 2 (infix-op (format " ~a " op))))]
    [is-null        1 ,(postfix-op " IS NULL")]
    [is-not-null    1 ,(postfix-op " IS NOT NULL")]
    [is-true        1 ,(postfix-op " IS TRUE")]
    [is-not-true    1 ,(postfix-op " IS NOT TRUE")]
    [is-false       1 ,(postfix-op " IS FALSE")]
    [is-not-false   1 ,(postfix-op " IS NOT FALSE")]
    [is-unknown     1 ,(postfix-op " IS UNKNOWN")]
    [is-not-unknown 1 ,(postfix-op " IS NOT UNKNOWN")]
    [collate        2 ,(infix-op   " COLLATE ")]
    [distinct-from  2 ,(infix-op " DISTINCT FROM ")]
    [not-distinct-from 2 ,(infix-op " NOT DISTINCT FROM ")]
    [between-and       3 ,(outfix-op '(" BETWEEN " " AND "))]
    [not-between-and   3 ,(outfix-op '(" NOT BETWEEN " " AND "))]
    [like       (2 3) ,(outfix-op '(" LIKE " " ESCAPE "))]
    [not-like   (2 3) ,(outfix-op '(" NOT LIKE " " ESCAPE "))]
    [ilike      (2 3) ,(outfix-op '(" ILIKE " " ESCAPE "))]
    [not-ilike  (2 3) ,(outfix-op '(" NOT ILIKE " " ESCAPE "))]
    [similar-to (2 3) ,(outfix-op '(" SIMILAR TO " " ESCAPE "))]
    [not-similar-to (2 3) ,(outfix-op '(" NOT SIMILAR TO " " ESCAPE "))]

    ;; Field reference
    ;; (.field x)    "x.field"
    [#rx"^[.]([a-zA-Z_][a-zA-Z_0-9]*)$"
     ,(lambda (op field-name) (list 1 (lambda (arg) (J "(" arg ")." field-name))))]
    ;; (.*) = "*", (.* t) = "t.*"
    [.*      (0 1) ,(case-lambda [() "*"] [(arg) (J "(" arg ").*")])]

    ;; Other notations
    [%ref    #&2 ,(lambda (array . indexes) (J "(" array ")[" (J-join indexes ",") "]"))]
    [%row    #&2 ,(lambda args (J "(" (J-join args ",") ")"))]
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
  (cond [(box? a) (>= n (unbox a))]
        [(list? a) (member n a)]
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
