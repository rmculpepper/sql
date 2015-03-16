;; Run-time SQL ast structures

#lang racket/base
(require racket/string
         (rename-in racket/match [match-define defmatch])
         racket/generic
         racket/class
         racket/format
         "jumble.rkt")
(provide (all-defined-out))

;; TODO:
;; - more comprehensive expr/function support
;; - turn emit into class w/ overridable methods
;; - macros, convert compile-time AST to run-time AST
;;   - statements, also public NTs!
;; - add INJECT form to main NTs: converts string to SQL
;; - 2 types of escape (ie unquote)
;;   - for ScalarExpr, turn into param (but need db-lib improvements)
;;     - also support $n params, w/ order permutation
;;   - for other NTs, need to insert AST dynamically
;; - keep original syntax around for (static) error checking
;; - support more syntax
;;   - table/view definition (for creation, for validation)
;; - check types (assumes schema?)
;; - check range-vars used correctly
;; - check aggregates used correctly

;; ============================================================
;; Abstract Nonterminals

;; The AST datatypes used prefab structs so that a compile-time AST
;; can be turned into a run-time AST simply via quoting (or
;; quasiquoting, to support run-time splicing).

;; A nonterminal NT may support the following additional forms for
;; dynamic ast composition and SQL injection:
;; - (NT:AST ,expr)
;; - (NT:INJECT String)
;; - (NT:INJECT ,expr)
;; For those NTs, the corresponding AST type contains the following variants:
;; - (list 'unquote Syntax)                        -- represents case (1)
;; - (NT:inject (U String (list 'unquote Syntax))) -- represents case (2) and (3)

;; Note for ScalarExpr: the three following forms are distinct:
;; - (select ,expr)                      -- TODO: turns into placeholder
;; - (select (ScalarExpr:AST ,expr))     -- splices ast result of expr
;; - (select (ScalarExpr:INJECT ,expr))  -- splices literal SQL code
;; And note that the first form is restricted to ScalarExpr (and not
;; implemented yet!).

;; ----------------------------------------
;; Select

;; A Select is 
;; (stmt:select (Listof SelectItem) (Listof TableRef) (Listof ScalarExpr)
;;              (Listof Name) (Listof ScalarExpr) (U SelectExtension #f))
(struct stmt:select (vals from where groupby having ext) #:prefab)

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

;; An Insert is (stmt:insert Name (U (Listof Ident) #f) TableExpr)
(struct stmt:insert (table columns source) #:prefab)

;; ----------------------------------------
;; Update

;; An Update is (stmt:update Name (Listof UpdateAssign) (Listof ScalarExpr))
(struct stmt:update (table assign where) #:prefab)

;; An UpdateAssign is (update:assign Ident ScalarExpr)
(struct update:assign (column expr) #:prefab)

;; ----------------------------------------
;; Delete

;; A Delete is (stmt:delete Name (Listof ScalarExpr))
(struct stmt:delete (table where) #:prefab)

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

(define (table-ref? x)
  (or (table-ref:name? x)
      (table-ref:as? x)
      (table-ref:inject? x)
      (table-expr? x)))

;; ----------------------------------------
;; Table Expressions

(struct table-expr:cross-join (t1 t2) #:prefab)
(struct table-expr:join (type t1 t2 on) #:prefab)
(struct table-expr:set-op (type t1 t2 opt corr) #:prefab)
(struct table-expr:values (rows) #:prefab)
(struct table-expr:select (select) #:prefab)
(struct table-expr:inject (sql) #:prefab)

(define (table-expr? x)
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
;; - (scalar:placeholder)
;; - Name
;; - ExactInteger
;; - String
;; * (list 'unquote Syntax)
;; * (scalar:inject (U String (list 'unquote Syntax)))
(struct scalar:app (op args) #:prefab)
(struct scalar:placeholder () #:prefab)
(struct scalar:inject (s) #:prefab)

(define (scalar-expr? x)
  (or (scalar:app? x)
      (scalar:placeholder? x)
      (name? x)
      (exact-integer? x)
      (string? x)
      (scalar:inject? x)))

;; FIXME: support:
;; - CASE {WHEN cond THEN result}* {ELSE result}? END
;; - EXISTS (subquery)
;; - expr IN (subquery)
;; - expr NOT IN (subquery)
;; - row-constructor op (subquery)
;; - expr IN (value ...), etc
;; - expr op ANY (subquery)
;; - expr op ALL (subquery)

(define (infix-op-entry sym [op-string (~a " " sym " ")] #:arity [arity '(1)])
  (list sym arity (infix-op op-string)))
(define ((fun-op op-string #:arg-sep [arg-sep ","]) . args)
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
    [coalesce (2) ,(fun-op "COALESCE")]
    [is-null   1  ,(lambda (arg) (J "(" arg " IS NULL)"))]
    [is-not-null 1 ,(lambda (arg) (J "(" arg " IS NOT NULL)"))]
    [extract 2 ,(fun-op "EXTRACT" #:arg-sep " FROM ")]
    ,(infix-op-entry '+)
    ,(infix-op-entry '-)
    ,(infix-op-entry '*)
    ,(infix-op-entry '/)
    ,(infix-op-entry '|| " || ") ;; HACK! Note "||" reads as the empty symbol!
    ,(infix-op-entry 'string-append " || ")
    ,(infix-op-entry 'string+ " || ")
    ,(infix-op-entry 'and " AND ")
    ,(infix-op-entry 'or  " OR ")
    ,(infix-op-entry 'like " LIKE " #:arity 2)
    ,(infix-op-entry 'not-like " NOT LIKE " #:arity 2)
    ;; Treat any other symbol composed of just the following
    ;; characters as a binary operator.
    [#rx"^[~!@#$%^&*-_=+|<>?/]+$"
     ,(lambda (sym) (list 2 (infix-op (format " ~a " sym))))]
    ))

(define (op-entry op)
  (let loop ([ops standard-ops])
    (cond [(null? ops) #f]
          [(symbol? (caar ops))
           (cond [(eq? op (caar ops))
                  (car ops)]
                 [else (loop (cdr ops))])]
          [(regexp? (caar ops))
           (cond [(regexp-match? (caar ops) (symbol->string op))
                  (cons op ((cadar ops) (caar ops)))]
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

(define (name? x)
  (or (ident? x)
      (qname? x)))

;; An Ident is one of
;; - Symbol               -- to be transmitted unquoted
;; - (id:literal String)  -- to be quoted when emitted
(struct id:quoted (s) #:prefab)

(define (ident? x)
  (or (symbol? x)
      (id:quoted? x)))
