;; Run-time SQL ast structures

#lang racket/base
(require racket/string
         (rename-in racket/match [match-define defmatch])
         racket/generic
         racket/class
         racket/format)
(provide (all-defined-out))

;; ============================================================
;; Abstract Nonterminals

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
(struct scalar:app (op args) #:transparent)

;; An Op is (op Arity Formatter)
;; where Arity     = Nat | (Nat) -- latter indicates arity at least
;;       Formatter = Value ... -> String
(struct op (arity formatter) #:transparent)

(define (arity-includes? a n)
  (cond [(pair? a) (> n (car a))]
        [else (= n a)]))

(define ((infix-op separator) . args)
  (~a "(" (string-join args separator) ")"))

(define (infix-op-entry sym [op-string (~a sym)])
  (list sym (op '(1) (infix-op op-string))))

(define ((fun-op op-string #:arg-sep [arg-sep ","]) . args)
  (~a op-string "(" (string-join args arg-sep) ")"))

(define standard-ops
  `([cast ,(op 2 (fun-op "cast" #:arg-sep " as "))]
    [coalesce ,(op '(0) (fun-op "coalesce"))]
    ,(infix-op-entry '+)
    ,(infix-op-entry '-)
    ,(infix-op-entry '*)
    ,(infix-op-entry '/)
    ,(infix-op-entry 'string-append "||")
    ,(infix-op-entry 'string+ "||")
    ,(infix-op-entry '=)
    ,(infix-op-entry '<>)
    ,(infix-op-entry '<)
    ,(infix-op-entry '<=)
    ,(infix-op-entry '>)
    ,(infix-op-entry '>=)))

;; ============================================================
;; Parsing

(require syntax/parse
         (only-in syntax/parse [attribute $]))

(define-syntax-class TableRef
  #:attributes (ast)
  #:datum-literals (as)
  (pattern table-name:Ident
           #:attr ast (table-ref:id ($ table-name.sym)))
  (pattern (as table-name:Ident range-var:Ident)
           #:attr ast (table-ref:as (table-ref:id ($ table-name.sym))
                                    ($ range-var.sym)))
  (pattern (as t:TableExpr range-var:Ident)
           #:attr ast (table-ref:as ($ t.ast) ($ range-var.sym)))
  (pattern :TableExpr))

(define-syntax-class TableExpr
  #:attributes (ast)
  #:datum-literals (cross-join values values*)
  (pattern (cross-join t1:TableRef t2:TableRef)
           #:attr ast (table-expr:cross-join ($ t1.ast) ($ t2.ast)))
  (pattern (j:Join t1:TableRef t2:TableRef :join-on-clause)
           #:attr ast (table-expr:join (syntax-e #'j) ($ t1.ast) ($ t2.ast) ($ on)))
  (pattern (so:SetOp t1:expr t2:expr :maybe-all :set-op-clause)
           #:attr ast (table-expr:set-op (syntax-e #'so)
                                         (parse-table-expr #'t1)
                                         (parse-table-expr #'t2)
                                         (attribute all?)
                                         (attribute corr)))
  (pattern (values e:expr ...)
           #:attr ast (table-expr:values
                       (list (map parse-scalar-expr (syntax->list #'(e ...))))))
  (pattern (values* [e:expr ...] ...)
           #:attr ast (table-expr:values
                       (for/list ([es (syntax->list #'((e ...) ...))])
                         (for/list ([e (syntax->list es)])
                           (parse-scalar-expr e)))))
  )

(define-syntax-class Join
  (pattern (~datum inner-join))
  (pattern (~datum left-join))
  (pattern (~datum right-join))
  (pattern (~datum full-join)))
(define-syntax-class SetOp
  (pattern (~datum union))
  (pattern (~datum intersect))
  (pattern (~datum except)))

(define-splicing-syntax-class set-op-clause
  (pattern (~seq #:corresponding)
           #:attr corr 'auto)
  (pattern (~seq #:corresponding-by (column:id ...))
           #:attr corr (syntax->datum #'(column ...)))
  (pattern (~seq)
           #:attr corr #f))

(define-splicing-syntax-class maybe-all
  (pattern (~seq #:all) #:attr all? #t)
  (pattern (~seq #:all) #:attr all? #f))

(define-splicing-syntax-class join-on-clause
  (pattern (~seq #:natural)
           #:attr on '(natural))
  (pattern (~seq #:using (column:id ...))
           #:attr on `(using ,(syntax->datum #'(column ...))))
  (pattern (~seq #:on condition:ScalarExpr)
           #:attr on `(on ,($ condition.ast))))

;; ----------------------------------------
#|

(define-syntax-class symbol-select-expr
  #:datum-literals (select)
  (pattern (~and (select . _) :inner-select-expr)))

(define-syntax-class inner-select-expr
  (pattern (_ (~or (~once sel:select-values-clause)
                   (~optional from:select-from-clause)
                   (~optional where:select-where-clause))
              ...)))

(define-splicing-syntax-class select-values-clause
  #:attributes ([ast 1])
  (pattern (~seq #:values :select-item ...)))

(define-syntax-class select-item
  #:attributes (ast)
  #:datum-literals (as)
  (pattern (as expr:ScalarExpr column:Ident)
           #:attr ast (select-item:as ($ expr.ast) ($ column.sym)))
  (pattern expr:ScalarExpr
           #:attr ast ($ expr.ast)))

(define-splicing-syntax-class select-from-clause
  #:attributes ([ast 1])
  (pattern (~seq #:from :table-ref-stx ...)))

(define-splicing-syntax-class select-where-clause
  #:attributes ([ast 1])
  (pattern (~seq #:where :ScalarExpr ...)))
|#

;; ----------------------------------------

(define-syntax-class ScalarExpr
  #:attributes (ast)
  (pattern n:exact-integer
           #:attr ast (syntax-e #'n))
  (pattern s:str
           #:attr ast (syntax-e #'s))
  (pattern var:id
           #:attr ast (syntax-e #'var))
  (pattern (o:Op arg:ScalarExpr ...)
           #:fail-unless (arity-includes? (op-arity (attribute o.op))
                                          (length (syntax->list #'(arg ...))))
                         "wrong arity"
           #:attr ast (scalar:app (attribute o.op) (attribute arg.ast))))

(define-syntax-class Op
  #:attributes (op)
  (pattern o:Ident
           #:attr op (cond [(assq ($ o.sym) standard-ops) => cadr] [else #f])
           #:when ($ op)))

;; ----------------------------------------

(define-syntax-class Ident
  #:attributes (sym)
  (pattern x:id #:attr sym (syntax-e #'x)))

;; ----------------------------------------

(define (parse-table-ref stx)
  (syntax-parse stx [x:TableRef ($ x.ast)]))
(define (parse-table-expr stx)
  (syntax-parse stx [x:TableExpr ($ x.ast)]))
(define (parse-scalar-expr stx)
  (syntax-parse stx [x:ScalarExpr ($ x.ast)]))
