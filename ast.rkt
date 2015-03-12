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
;; - identifier and string SQL safety
;; - qualified identifiers
;; - 2 types of escapes (unquote):
;; - auto functions???
;; - keep original syntax around for (static) error checking
;; - support more syntax
;;   - table/view definition (for creation, for validation)
;; - check types (assumes schema?)
;; - check range-vars used correctly
;; - check aggregates used correctly
;; - BTW, this shouldn't be called racquel; that name should be reserved
;;   for something more high-level. This is just db/sql.

;; ============================================================
;; Abstract Nonterminals

;; ----------------------------------------
;; Select

;; A Select is 
;; (stmt:select (Listof SelectItem) (Listof TableRef) (Listof ScalarExpr)
;;              (Listof Name) (Listof ScalarExpr) (U SelectExtension #f))
(struct stmt:select (vals from where groupby having ext) #:transparent)

;; A SelectItem is one of
;; - (select-item:as ScalarExpr Ident)
;; - (select-item:all)
;; - ScalarExpr
(struct select-item:as (expr name) #:transparent)
(struct select-item:all () #:transparent)

;; A SelectExtension is
;; (select:extension (Listof SelectOrder) (U ScalarExpr #f) (U ScalarExpr #f))
(struct select:extension (order limit offset) #:transparent)

;; A SelectOrder is (select:order ScalarExpr (U 'asc 'desc #f))
(struct select:order (column asc/desc) #:transparent)

;; ----------------------------------------
;; Insert

;; An Insert is (stmt:insert Name (U (Listof Ident) #f) TableExpr)
(struct stmt:insert (table columns source) #:transparent)

;; ----------------------------------------
;; Update

;; An Update is (stmt:update Name (Listof UpdateAssign) (Listof ScalarExpr))
(struct stmt:update (table assign where) #:transparent)

;; An UpdateAssign is (update:assign Ident ScalarExpr)
(struct update:assign (column expr) #:transparent)

;; ----------------------------------------
;; Delete

;; A Delete is (stmt:delete Name (Listof ScalarExpr))
(struct stmt:delete (table where) #:transparent)

;; ----------------------------------------
;; Table References

;; A TableRef is one of
;; - (table-ref:name Name)
;; - (table-ref:as TableExpr Ident)
;; - TableExpr

(struct table-ref:name (name) #:transparent)
(struct table-ref:as (e rangevar) #:transparent)

;; ----------------------------------------
;; Table Expressions

(struct table-expr:cross-join (t1 t2) #:transparent)
(struct table-expr:join (type t1 t2 on) #:transparent)
(struct table-expr:set-op (type t1 t2 opt corr) #:transparent)
(struct table-expr:values (rows) #:transparent)
(struct table-expr:select (select) #:transparent)

(define (table-expr? x)
  (or (join-table-expr? x)
      (nonjoin-table-expr? x)))

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
;; - (scalar:literal String)
;; - Name
;; - ExactInteger
;; - String
(struct scalar:app (op args) #:transparent)
(struct scalar:placeholder () #:transparent)
(struct scalar:literal (s) #:transparent)

(define (infix-op-entry sym [op-string (~a " " sym " ")])
  (list sym '(1) (infix-op op-string)))
(define ((fun-op op-string #:arg-sep [arg-sep ","]) . args)
  (J op-string "(" (J-join args arg-sep) ")"))
(define ((infix-op separator) . args)
  (J "(" (J-join args separator) ")"))

(define standard-ops
  `(;;[Symbol Arity Formatter]
    ;; where Arity     = Nat | (Nat) -- latter indicates arity at least
    ;;       Formatter = String ... -> String
    [cast      2  ,(fun-op "cast" #:arg-sep " as ")]
    [coalesce (2) ,(fun-op "coalesce")]
    ,(infix-op-entry '+)
    ,(infix-op-entry '-)
    ,(infix-op-entry '*)
    ,(infix-op-entry '/)
    ,(infix-op-entry 'string-append " || ")
    ,(infix-op-entry 'string+ " || ")
    ,(infix-op-entry '=)
    ,(infix-op-entry '<>)
    ,(infix-op-entry '<)
    ,(infix-op-entry '<=)
    ,(infix-op-entry '>)
    ,(infix-op-entry '>=)))

(define (op-formatter op-name)
  (cond [(assoc op-name standard-ops) => caddr]
        [else #f]))

(define (check-arity op-name n-args)
  (cond [(assoc op-name standard-ops)
         => (lambda (entry) (arity-includes? (cadr entry) n-args))]
        [else #t]))

(define (arity-includes? a n)
  (cond [(pair? a) (> n (car a))]
        [else (= n a)]))

;; ----------------------------------------

;; A Name is one of
;; - Ident                -- unqualified name
;; - (qname Name Ident)   -- qualified name
(struct qname (qual id) #:transparent)

;; An Ident is one of
;; - Symbol               -- to be transmitted unquoted
;; - (id:literal String)  -- to be quoted when emitted
(struct id:literal (s) #:transparent)

;; TODO:
;; - have predicate for unquoted ids?
;; - have mode where Racket identifier parsed as lit-id?
;; - ...?
