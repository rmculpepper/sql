;; Run-time SQL ast structures

#lang racket/base
(require racket/string
         (rename-in racket/match [match-define defmatch])
         racket/generic
         racket/class
         racket/format)
(provide (all-defined-out))

;; TODO:
;; - keep original syntax around for (static) error checking
;; - support more syntax
;;   - INSERT, UPDATE, DELETE, ...
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
;;              (Listof Symbol) (Listof ScalarExpr) (U SelectExtension #f))
(struct stmt:select (vals from where groupby having ext) #:transparent)

;; A SelectItem is one of
;; - (select-item:as ScalarExpr Symbol)
;; - ScalarExpr
(struct select-item:as (expr name) #:transparent)

;; A SelectExtension is
;; (select:extension (Listof SelectOrder) (U ScalarExpr #f) (U ScalarExpr #f))
(struct select:extension (order limit offset) #:transparent)

;; A SelectOrder is (select:order Symbol (U 'asc 'desc #f))
(struct select:order (column asc/desc) #:transparent)

;; ----------------------------------------
;; Insert

;; An Insert is (stmt:insert Symbol (U (Listof Symbol) #f) TableExpr)
(struct stmt:insert (table columns source) #:transparent)

;; ----------------------------------------
;; Update

;; An Update is (stmt:update Symbol (Listof UpdateAssign) (Listof ScalarExpr))
(struct stmt:update (table assign where) #:transparent)

;; An UpdateAssign is (update:assign Symbol ScalarExpr)
(struct update:assign (column expr) #:transparent)

;; ----------------------------------------
;; Table References

;; A TableRef is one of
;; - (table-ref:id Symbol)
;; - (table-ref:as TableExpr Symbol)
;; - TableExpr

(struct table-ref:id (id) #:transparent)
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
;; - Symbol
;; - ExactInteger
;; - String
(struct scalar:app (op args) #:transparent)
(struct scalar:placeholder () #:transparent)
(struct scalar:literal (s) #:transparent)

;; An Op is (op Arity Formatter)
;; where Arity     = Nat | (Nat) -- latter indicates arity at least
;;       Formatter = Value ... -> String
(struct op (arity formatter) #:transparent)

(define (arity-includes? a n)
  (cond [(pair? a) (> n (car a))]
        [else (= n a)]))

(define ((infix-op separator) . args)
  (~a "(" (string-join args separator) ")"))

(define (infix-op-entry sym [op-string (~a " " sym " ")])
  (list sym (op '(1) (infix-op op-string))))

(define ((fun-op op-string #:arg-sep [arg-sep ","]) . args)
  (~a op-string "(" (string-join args arg-sep) ")"))

(define standard-ops
  `([cast ,(op 2 (fun-op "cast" #:arg-sep " as "))]
    [coalesce ,(op '(2) (fun-op "coalesce"))]
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
